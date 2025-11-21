open Game_engine_interface

module Make
    (Maze : MAZE)
    (Pacman : PACMAN)
    (Ghost : GHOST)
    (Constants : CONSTANTS) =
struct
  (** High-level states that describe the game's current phase. These states
      determine how [update_world] behaves. *)
  type game_state =
    | Intro
    | Playing
    | PacDead
    | LevelComplete
    | GameOver

  type world = {
    maze : Maze.t;  (** Current maze layout and pellet state. *)
    pac : Pacman.t;  (** Pac-Man's current position and direction. *)
    ghosts : Ghost.t list;  (** All active ghosts in the world. *)
    score : int;  (** Player's accumulated score. *)
    lives : int;  (** Remaining lives before Game Over. *)
    state : game_state;  (** Current global game state. *)
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
    }

  (** [try_move maze (x,y) (nx,ny) move_fn entity] attempts to move an entity
      from [(x,y)] to [(nx,ny)]. If the target tile is a wall, the entity
      remains in place; otherwise [move_fn] applies the movement.

      Pac-Man and ghosts both use this helper via their own [move_to] functions.
  *)
  let try_move maze (_x, _y) (nx, ny) move_fn entity =
    if Maze.is_wall maze nx ny then entity else move_fn entity nx ny

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
      (* Pac-Man starts on a ghost: immediate death, no movement or pellet
         logic *)
      { w with state = PacDead }
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
            let dx, dy = Ghost.next_position g in
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
        if Maze.pellets_remaining maze' = 0 then LevelComplete else Playing
      in

      (* ---- Death Check AFTER movement ---- *)
      let pac_dead_after =
        List.exists (fun g -> Pacman.position pac' = Ghost.position g) ghosts'
      in
      let final_state =
        if pac_dead_after then PacDead else state_after_pellets
      in

      {
        w with
        pac = pac';
        ghosts = ghosts';
        maze = maze';
        score = score';
        state = final_state;
      }

  (** [update_world w] advances the world by one frame, behaving differently
      depending on the global game state:

      - [Intro] or [GameOver]: No updates; world remains unchanged.
      - [Playing]: Standard simulation step using [update_playing].
      - [PacDead]: Either decrement a life and respawn, or transition to
        [GameOver].
      - [LevelComplete]: Respawn entities and continue playing (future: load
        next maze). *)
  let update_world w =
    match w.state with
    | Intro -> w
    | GameOver -> w
    | PacDead ->
        if w.lives <= 1 then { w with state = GameOver }
        else
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
            state = Playing;
          }
    | LevelComplete ->
        let px, py = Constants.pacman_start_pos in
        let ghosts =
          List.map
            (fun (gx, gy) -> Ghost.create gx gy)
            Constants.ghost_start_positions
        in
        {
          w with
          (* Future extension: load next maze layout. *)
          maze = w.maze;
          pac = Pacman.create px py;
          ghosts;
          state = Playing;
        }
    | Playing -> update_playing w
end
