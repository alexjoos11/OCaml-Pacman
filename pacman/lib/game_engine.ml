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
    powerup_timer : int;  (** Frames remaining in power-up mode. *)
    move_cooldown : int;  (** Frames until Pac-Man can move again. *)
    ghost_move_cooldown : int;  (** Frames until ghosts can move again. *)
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
      powerup_timer = 0;
      move_cooldown = 0;
      ghost_move_cooldown = 0;
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
      powerup_timer = 0;
      ghost_move_cooldown = 0;
    }

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
      (* Pac-Man movement: either decrement cooldown OR move now *)
      let pac', move_cooldown' =
        if w.move_cooldown > 0 then (w.pac, w.move_cooldown - 1)
        else
          let p = Movement.move_pacman w.maze w.pac in
          (p, Constants.movement_delay)
      in

      (* Ghost movement: same pattern, but separate timer *)
      let ghosts', ghost_cd' =
        if w.ghost_move_cooldown > 0 then (w.ghosts, w.ghost_move_cooldown - 1)
        else
          let moved =
            List.map
              (fun g -> Movement.move_ghost w.maze g (Pacman.position pac'))
              w.ghosts
          in
          (moved, Constants.ghost_move_cooldown)
      in

      (* ---------------- Pellet Eating ---------------- *)
      let px', py' = Pacman.position pac' in
      let maze', score' =
        let item_type = Maze.item_at w.maze px' py' in
        if item_type <> None then
          match item_type with
          | Some Pellet ->
              (Maze.eat_item w.maze px' py', w.score + Constants.pellet_score)
          | Some PowerPellet -> failwith "Power pellet eating not implemented"
          | _ -> failwith "Unexpected/unimplemented item type"
        else (w.maze, w.score)
      in

      (* ---------------- Level Complete ---------------- *)
      let state_after_pellets =
        if not (Maze.items_exist maze') then LevelComplete else Playing
      in

      (* ---------------- Collision AFTER movement ---------------- *)
      let hit_after =
        List.exists (fun g -> Pacman.position pac' = Ghost.position g) ghosts'
      in

      let final_state = if hit_after then PacDead else state_after_pellets in

      let pacdead_timer =
        if final_state = PacDead then Constants.pacdead_pause_frames else 0
      in

      (* ---------------- Build next world ---------------- *)
      {
        w with
        pac = pac';
        ghosts = ghosts';
        maze = maze';
        score = score';
        state = final_state;
        pacdead_timer;
        powerup_timer = w.powerup_timer;
        move_cooldown = move_cooldown';
        ghost_move_cooldown = ghost_cd';
      }

  let update_world w =
    match w.state with
    | Intro -> w
    | GameOver -> w
    | LevelComplete -> w
    | PacDead ->
        if w.pacdead_timer > 0 then
          { w with pacdead_timer = w.pacdead_timer - 1 }
        else if w.lives <= 1 then { w with state = GameOver }
        else respawn w
    | PowerUp ->
        if w.powerup_timer > 0 then
          { w with powerup_timer = w.powerup_timer - 1 }
        else
          let ghosts' =
            List.map (fun g -> Ghost.set_frightened g false) w.ghosts
          in
          { w with ghosts = ghosts'; state = Playing }
    | Playing -> update_playing w
end
