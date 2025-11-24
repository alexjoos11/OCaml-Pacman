open Game_engine_interface
open Game_state

module Make
    (Maze : MAZE)
    (Pacman : PACMAN)
    (Ghost : GHOST)
    (Constants : CONSTANTS) =
struct
  type world = {
    maze : Maze.t;  (** Current maze layout and pellet state. *)
    pac : Pacman.t;  (** Pac-Man's current position and direction. *)
    ghosts : Ghost.t list;  (** All active ghosts in the world. *)
    score : int;  (** Player's accumulated score. *)
    lives : int;  (** Remaining lives before Game Over. *)
    state : game_state;  (** Current global game state. *)
    pacdead_timer : int;
    move_cooldown : int;
  }

  (** A complete snapshot of the game at a single moment in time. The world is
      immutable: each update produces a new world value. *)

  (** Construct the starting world given an initial maze, Pac-Man, and a list of
      ghosts. Score is set to 0 and lives come from [Constants.starting_lives].
  *)
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
    }

  let start w =
    match w.state with
    | Intro -> { w with state = Playing }
    | _ -> w

  (**[try_move maze (x,y) (nx,ny) move_fn entity] attempts to move [entity] from
     its current tile [(x,y)] to the target tile [(nx,ny)].

     - If [(nx,ny)] is outside the maze bounds, the entity remains at [(x,y)].
     - If [(nx,ny)] contains a wall according to [Maze.is_wall], the entity
       remains at [(x,y)].
     - Otherwise, [move_fn entity nx ny] is applied to produce the updated
       entity.

     This helper is used for both Pac-Man and ghosts to ensure consistent rules
     for movement across the maze. *)
  let try_move maze _pos (nx, ny) move_fn entity =
    if Maze.is_wall maze nx ny then entity else move_fn entity nx ny

  (** Helper: respawn Pac-Man and all ghosts after death.

      - Decrements lives.
      - Resets Pac-Man to starting tile.
      - Resets ghost list to their initial starting positions.
      - Clears the pacdead timer.
      - Returns the world in [Playing] state. *)
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
    }

  (** Performs a single simulation step of active gameplay:
      - Pac-Man attempts to move one tile in his current direction.
      - Ghosts each attempt to move according to their movement rules.
      - Pellets may be eaten, updating the maze and score.
      - Level completion or Pac-Man death may occur.

      Returns a new world with updated positions and state. *)
  let update_playing w =
    (* ---- Early collision check BEFORE movement ---- *)
    let pac_pos = Pacman.position w.pac in
    let pac_dead_start =
      List.exists (fun g -> pac_pos = Ghost.position g) w.ghosts
    in
    if pac_dead_start then
      (* Death occurs immediately, and we start the freeze timer *)
      { w with state = PacDead; pacdead_timer = Constants.pacdead_pause_frames }
      (* ---- Movement cooldown ---- *)
    else if w.move_cooldown > 0 then
      (* Movement is temporarily paused. Pac-Man and ghosts stay completely
         still this frame.

         This avoids "moving every frame" at 60 FPS, which is too fast. Instead,
         we decrement the cooldown and wait until it reaches 0 before permitting
         another tile movement. *)
      { w with move_cooldown = w.move_cooldown - 1 }
    else
      (* ---- Pac-Man movement ---- *)
      let px, py = pac_pos in
      let desired_px, desired_py = Pacman.next_position w.pac in
      let pac' =
        try_move w.maze (px, py) (desired_px, desired_py) Pacman.move_to w.pac
      in

      (* ---- Ghost movement ---- *)
      let ghosts' =
        List.map
          (fun g ->
            let gx, gy = Ghost.position g in
            let pac_px, pac_py = Pacman.position pac' in
            let dx, dy = Ghost.next_position g ~pac_pos:(pac_px, pac_py) in
            try_move w.maze (gx, gy) (dx, dy) Ghost.move_to g)
          w.ghosts
      in

      (* ---- Pellet Eating ---- *)
      let px', py' = Pacman.position pac' in
      let maze', score' =
        if Maze.pellet_at w.maze px' py' then
          (Maze.eat_pellet w.maze px' py', w.score + Constants.pellet_score)
        else (w.maze, w.score)
      in

      (* ---- Level Complete? ---- *)
      let state_after_pellets =
        if not (Maze.pellets_exist maze') then LevelComplete else Playing
      in

      (* ---- Death Check AFTER movement ---- *)
      let pac_dead_after =
        List.exists (fun g -> Pacman.position pac' = Ghost.position g) ghosts'
      in

      let final_state =
        if pac_dead_after then PacDead else state_after_pellets
      in

      let pacdead_timer =
        if final_state = PacDead then Constants.pacdead_pause_frames else 0
      in

      {
        w with
        pac = pac';
        ghosts = ghosts';
        maze = maze';
        score = score';
        state = final_state;
        pacdead_timer;
        move_cooldown = Constants.movement_delay;
      }

  (** [update_world w] advances the game state by one frame.

      The behavior depends on the current global game state:

      - [Intro] — No simulation occurs. The world is frozen until the user
        starts the game (handled externally via [start]).

      - [GameOver] — The game has ended and no further updates occur. The world
        is frozen until restarted.

      - [LevelComplete] — All pellets have been eaten. For a single-level game,
        the world remains frozen; the renderer may display a "You Win!" screen.
        No automatic respawn or reset occurs.

      - [PacDead] — Pac-Man has collided with a ghost. A short freeze period
        occurs (controlled by [pacdead_timer]). When the timer expires:

      • If lives remain, Pac-Man and ghosts are respawned and the state returns
      to [Playing]. • If no lives remain, the state transitions to [GameOver].

      - [Playing] — A normal simulation step occurs (movement, collision checks,
        pellet updates, scoring, and win/loss detection). *)
  let update_world w =
    match w.state with
    | Intro -> w
    | GameOver -> w
    | LevelComplete -> w
    | PacDead ->
        if w.pacdead_timer > 0 then
          (* Still counting down the freeze period *)
          { w with pacdead_timer = w.pacdead_timer - 1 }
        else if w.lives <= 1 then
          (* Timer expired and no lives left → Game Over *)
          { w with state = GameOver }
        else
          (* Timer expired, lives remain → respawn everything *)
          respawn w
    | Playing -> update_playing w
end
