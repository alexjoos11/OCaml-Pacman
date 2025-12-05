open Game_engine_interface
open Game_state

module Make
    (Maze : MAZE)
    (Pacman : PACMAN)
    (Ghost : GHOST)
    (Constants : CONSTANTS) =
struct
  module Movement = Movement.Make (Maze) (Pacman) (Ghost) (Constants)

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
    ghost_move_accumulators : float list;
    frames_alive : int;
    speedup_timer : int;
  }

  (** Set the default values for when the game begins. *)
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
      ghost_move_accumulators = List.map (fun _ -> 0.0) ghosts;
      frames_alive = 0;
      speedup_timer = 0;
    }

  let start w =
    match w.state with
    | Intro -> { w with state = Playing }
    | _ -> w

  (** Pac-Man is respawned and all ghosts to their original starting positions.
      The number of lives is decreased by 1, cooldowns reset, and world
      unfreezes back to [Playing]. *)
  let respawn w =
    let px, py = Constants.pacman_start_pos in
    let ghosts = List.map (fun g -> Ghost.respawn g) w.ghosts in
    {
      w with
      lives = w.lives - 1;
      pac = Pacman.create px py;
      ghosts;
      pacdead_timer = 0;
      state = Playing;
      move_cooldown = 0;
      powerup_timer = 0;
      ghost_move_accumulators = List.map (fun _ -> 0.0) ghosts;
      frames_alive = 0;
      speedup_timer = 0;
    }

  (** One frame of gameplay logic where movement is tile-based and collision is
      checked before and after movement. *)
  let update_playing w =
    (* Count frames that Pacman has been alive *)
    let w = { w with frames_alive = w.frames_alive + 1 } in

    (* Want speedup to happen every 7.5 seconds. this converts frames to
       seconds *)
    let frames_to_sec = int_of_float (7.5 *. float_of_int Constants.fps) in

    (* Update speedup message timer every frame *)
    let w =
      if
        frames_to_sec > 0 && w.frames_alive > 0
        && w.frames_alive mod frames_to_sec = 0
      then
        (* This means we hit a multiple of 7.5 seconds so show "SPEED UP!" for 1
           second *)
        { w with speedup_timer = Constants.fps }
      else if w.speedup_timer > 0 then
        (* Used for countdown message and is on screen for fps frames = 1
           second *)
        { w with speedup_timer = w.speedup_timer - 1 }
      else w
    in

    (* Collision occurs before movement *)
    let pac_pos = Pacman.position w.pac in
    let hit_immediate =
      match w.state with
      | PowerUp -> false
      | _ ->
          List.exists
            (fun g -> pac_pos = Ghost.position g && not (Ghost.is_eaten g))
            w.ghosts
    in
    if hit_immediate then
      { w with state = PacDead; pacdead_timer = Constants.pacdead_pause_frames }
    else
      (* Pac-Man movement move now or decrement the cooldown *)
      let pac', move_cooldown' =
        if w.move_cooldown > 0 then (w.pac, w.move_cooldown - 1)
        else
          let p = Movement.move_pacman w.maze w.pac in

          (* Find how many 7.5-second blocks have elapsed *)
          let blocks =
            if frames_to_sec <= 0 then 0 else w.frames_alive / frames_to_sec
          in

          (* Find the new delay which can't be less than 1 *)
          let new_delay =
            let del = Constants.movement_delay - blocks in
            if del <= 0 then 1 else del
          in

          (p, new_delay)
      in

      (* Update the ghost timers *)
      let time = 1.0 /. float_of_int Constants.fps in
      let time_update = List.map (Ghost.update_duration ~time) w.ghosts in
      let move_threshold = float_of_int Constants.ghost_move_cooldown in
      let combined = List.combine time_update w.ghost_move_accumulators in

      (* Ghost movement with a separate timer *)
      let process_list =
        List.map
          (fun (g, accum) ->
            let factor = Ghost.speed_factor g in
            let new_accum = accum +. factor in
            if new_accum >= move_threshold then
              let g' =
                if Ghost.(is_at_home g && is_eaten g) then Ghost.respawn g
                else
                  let pac_pos_for_ghost = Pacman.position pac' in
                  Movement.move_ghost w.maze g pac_pos_for_ghost
              in
              let remain = new_accum -. move_threshold in
              (g', remain)
            else (g, new_accum))
          combined
      in
      let ghosts', ghost_accumulators' = List.split process_list in

      (* Eating pellets *)
      let px', py' = Pacman.position pac' in
      let maze', score', ghosts', powerup_timer', state' =
        let item_type = Maze.item_at w.maze px' py' in
        if item_type <> None then
          match item_type with
          (* Eat a normal pellet so update score and maze *)
          | Some Pellet ->
              ( Maze.eat_item w.maze px' py',
                w.score + Constants.pellet_score,
                ghosts',
                w.powerup_timer,
                w.state )
          (* Eat a Power Pellet to update MAZE, SCORE, GHOSTS, POWERUP TIMER,
             and STATE *)
          | Some PowerPellet ->
              ( Maze.eat_item w.maze px' py',
                w.score + Constants.power_pellet_score,
                List.map (fun g -> Ghost.set_frightened g true) ghosts',
                Constants.power_pellet_duration_frames,
                PowerUp )
          | Some Cherry ->
              ( Maze.eat_item w.maze px' py',
                w.score + Constants.cherry_score,
                ghosts',
                w.powerup_timer,
                Playing )
          | _ -> failwith "Unexpected/unimplemented item type"
        else (w.maze, w.score, ghosts', w.powerup_timer, w.state)
      in

      (* Level has been completed *)
      let state' =
        if not (Maze.items_exist maze') then LevelComplete else state'
      in

      (* Collision occurs after moving *)
      let pac_hurt, score', ghosts' =
        match state' with
        | PowerUp ->
            let p_pos = Pacman.position pac' in
            let ghosts_rev, score' =
              List.fold_left
                (fun (acc_g, acc_score) g ->
                  let g_pos = Ghost.position g in
                  if g_pos = p_pos && not (Ghost.is_eaten g) then
                    let g' = Ghost.set_eaten g true in
                    (g' :: acc_g, acc_score + Constants.ghost_eaten_score)
                  else (g :: acc_g, acc_score))
                ([], score') ghosts'
            in
            (false, score', List.rev ghosts_rev)
        | Playing ->
            let p_pos = Pacman.position pac' in
            let pac_hurt =
              List.exists
                (fun g ->
                  let g_pos = Ghost.position g in
                  g_pos = p_pos && not (Ghost.is_eaten g))
                ghosts'
            in
            (pac_hurt, score', ghosts')
        | LevelComplete -> (false, score', ghosts')
        | _ ->
            failwith
              "Collision AFTER movement section of game_engine.ml:\n\
              \              Unexpected game state after pellet consumption"
      in

      let final_state = if pac_hurt then PacDead else state' in

      let pacdead_timer =
        if final_state = PacDead then Constants.pacdead_pause_frames else 0
      in

      (* Build the next world *)
      {
        w with
        pac = pac';
        ghosts = ghosts';
        maze = maze';
        score = score';
        state = final_state;
        pacdead_timer;
        powerup_timer = powerup_timer';
        move_cooldown = move_cooldown';
        ghost_move_accumulators = ghost_accumulators';
      }

  (** [update_world w] sends information to other functions based on the current
      [w.state]. *)
  let update_world w =
    match w.state with
    | Intro -> w (* No updates happen in Intro state *)
    | GameOver _ -> w (* No updates happen in GameOver state *)
    | LevelComplete -> w (* No updates happen in LevelComplete state *)
    | PacDead ->
        (* In PacDead state, just count down the timer *)
        if w.pacdead_timer > 0 then
          { w with pacdead_timer = w.pacdead_timer - 1 }
        else if w.lives <= 1 then (
          let player_score = w.score in
          let old_high_score = High_score.load_score () in
          let update_high_score = player_score > old_high_score in
          if update_high_score then High_score.save_score player_score;

          let display_high_score =
            if update_high_score then player_score else old_high_score
          in
          let game_end_info =
            Game_state.GameOver
              {
                final_score = player_score;
                old_high_score = display_high_score;
                update_high_score;
              }
          in
          (* Timer done so no lives left *)
          { w with state = game_end_info; lives = 0 })
        else respawn w
    | PowerUp ->
        if w.powerup_timer > 0 then
          update_playing { w with powerup_timer = w.powerup_timer - 1 }
        else
          let ghosts' =
            List.map
              (fun g ->
                if Ghost.is_eaten g then
                  Ghost.respawn g (* Failsafe for a dumb ghost *)
                else
                  let g = Ghost.set_frightened g false in
                  Ghost.set_eaten g false)
              w.ghosts
          in
          let w = { w with ghosts = ghosts'; state = Playing } in
          update_playing w
    | Playing -> update_playing w
end
