open Game_engine_interface
open Game_state

module Make
    (Maze : MAZE)
    (Pacman : PACMAN)
    (Ghost : GHOST)
    (Constants : CONSTANTS) =
struct
  module Movement = Movement.Make (Maze) (Pacman) (Ghost) (Constants)

  (* ============================================================= *)
  (* WORLD STATE RECORD                                            *)
  (* ============================================================= *)

  type world = {
    maze : Maze.t;  (** Immutable maze state. *)
    pac : Pacman.t;  (** Pac-Man's position and direction. *)
    ghosts : Ghost.t list;  (** All active ghosts. *)
    score : int;  (** Player score. *)
    lives : int;  (** Remaining lives. *)
    state : game_state;  (** Global game state. *)
    pacdead_timer : int;  (** Freeze countdown after death. *)
    move_cooldown : int;  (** Frames until Pac-Man can move again. *)
    ghost_move_accumulators : float list;
  }

  let initial_world maze pac ghosts =
    {
      maze;
      pac;
      ghosts;
      score = 0;
      lives = Constants.starting_lives;
      state = Intro;
      pacdead_timer = 0;
      move_cooldown = 0;
      ghost_move_accumulators = List.map (fun _ -> 0.0) ghosts;
    }

  let start w =
    match w.state with
    | Intro -> { w with state = Playing }
    | _ -> w

  (** Respawns Pac-Man and all ghosts to their original starting positions.
      Lives decrease by 1; cooldowns reset; world unfreezes back to [Playing].
  *)
  let respawn w =
    let px, py = Constants.pacman_start_pos in
    let ghosts =
      List.map
        (fun (gx, gy) -> Ghost.create gx gy)
        Constants.ghost_start_positions
    in
    {
      w with
      lives = w.lives - 1;
      pac = Pacman.create px py;
      ghosts;
      pacdead_timer = 0;
      state = Playing;
      move_cooldown = 0;
      ghost_move_accumulators = List.map (fun _ -> 0.0) ghosts;
    }

  let get_factor (mode : Ghost.speed) =
    match mode with
    | Ghost.Fast -> 2.0
    | Ghost.Regular -> 1.0
    | Ghost.Slow -> 0.5
    | Ghost.Paused -> 0.0

  (** One frame of gameplay. Movement is tile-based and throttled by independent
      cooldowns. Collision is checked before and after movement. *)
  let update_playing w =
    (* ---------------- Early collision BEFORE movement ---------------- *)
    let pac_pos = Pacman.position w.pac in
    let hit_immediate =
      List.exists (fun g -> pac_pos = Ghost.position g) w.ghosts
    in
    if hit_immediate then
      { w with state = PacDead; pacdead_timer = Constants.pacdead_pause_frames }
    else
      (* --- Update ghost timers --- *)
      let time = 1.0 /. float_of_int Constants.fps in
      let time_update = List.map (Ghost.update_duration ~time) w.ghosts in

      (* Pac-Man movement: either decrement cooldown OR move now *)
      let pac', move_cooldown' =
        if w.move_cooldown > 0 then (w.pac, w.move_cooldown - 1)
        else
          let p = Movement.move_pacman w.maze w.pac in
          (p, Constants.movement_delay)
      in
      let move = float_of_int Constants.ghost_move_cooldown in
      let combine = List.combine time_update w.ghost_move_accumulators in

      (* Ghost movement: same pattern, but separate timer *)
      let process_list =
        List.map
          (fun (g, accum) ->
            let mode = Ghost.get_speed g in
            let factor = get_factor mode in
            let new_accum = accum +. factor in
            if new_accum >= move then
              let pac_pos_for_ghost = Pacman.position pac' in
              let moved_ghost =
                Movement.move_ghost w.maze g pac_pos_for_ghost
              in
              let remain = new_accum -. move in
              (moved_ghost, remain)
            else (g, new_accum))
          combine
      in
      let ghosts_after_move, ghost_accumulators' = List.split process_list in

      (* ---------------- Pellet Eating ---------------- *)
      let px', py' = Pacman.position pac' in
      let maze', score', ghosts_after_pellets =
        if Maze.pellet_at w.maze px' py' then
          let maze_after_eat = Maze.eat_pellet w.maze px' py' in
          (* --- Power Pellet Eaten --- *)
          let new_score =
            if Maze.is_power_pellet w.maze px' py' then
              w.score + Constants.power_pellet_score
            else w.score + Constants.pellet_score
          in
          (maze_after_eat, new_score, ghosts_after_move)
        else (w.maze, w.score, ghosts_after_move)
      in

      (* ---------------- Level Complete ---------------- *)
      let state_after_pellets =
        if not (Maze.pellets_exist maze') then LevelComplete else Playing
      in

      (* ---------------- Collision AFTER movement ---------------- *)
      let hit_after =
        List.exists
          (fun g -> Pacman.position pac' = Ghost.position g)
          ghosts_after_pellets
      in

      let final_state = if hit_after then PacDead else state_after_pellets in

      let pacdead_timer =
        if final_state = PacDead then Constants.pacdead_pause_frames else 0
      in

      (* ---------------- Build next world ---------------- *)
      {
        w with
        pac = pac';
        ghosts = ghosts_after_pellets;
        maze = maze';
        score = score';
        state = final_state;
        pacdead_timer;
        move_cooldown = move_cooldown';
        ghost_move_accumulators = ghost_accumulators';
      }

  (** [update_world w] is the main game loop step. It dispatches to other
      functions based on the current [w.state]. *)
  let update_world w =
    match w.state with
    | Intro -> w (* No updates happen in Intro state *)
    | GameOver -> w (* No updates happen in GameOver state *)
    | LevelComplete -> w (* No updates happen in LevelComplete state *)
    | PacDead ->
        (* In PacDead state, just count down the timer *)
        if w.pacdead_timer > 0 then
          { w with pacdead_timer = w.pacdead_timer - 1 }
        else if w.lives <= 1 then
          (* Timer done, no lives left *)
          { w with state = GameOver }
        else
          (* Timer done, lives remain *)
          respawn w
    | Playing ->
        (* Normal gameplay *)
        update_playing w
end
