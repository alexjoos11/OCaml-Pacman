open Game_engine_interface

module Make
    (Maze : MAZE)
    (Pacman : PACMAN)
    (Ghost : GHOST)
    (Constants : CONSTANTS) =
struct
  (** [move_pacman maze pac] attempts to move Pac-Man one tile in his current
      direction.

      Behavior:
      - Computes Pac-Man’s intended next tile using [Pacman.next_position].
      - If the tile is a wall (according to [Maze.is_wall]), Pac-Man does not
        move.
      - Otherwise, Pac-Man moves into the tile via [Pacman.move_to].

      This function handles only movement rules. Collisions, pellets, scoring,
      and game-state transitions are handled by the game engine. *)
  let move_pacman maze pac =
    let nx, ny = Pacman.next_position pac in
    if Maze.is_wall maze nx ny then pac else Pacman.move_to pac nx ny

  (** A list of fallback directions a ghost may take when its intended chase
      direction is blocked by a wall. These represent the four cardinal moves:
      right, left, down, up.

      These directions are only used when the primary chase direction is
      invalid. A random valid fallback direction is chosen each frame. *)
  let fallback_dirs = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]

  (** [move_ghost maze g pac_pos] computes the ghost's movement for one engine
      tick and returns the updated ghost.

      Behavior:
      - Computes the ghost's intended chase direction using
        [Ghost.next_position]. This direction always aims toward Pac-Man's
        position.
      - If the chase tile is not a wall, the ghost moves directly into it.
      - If the chase tile *is* a wall, the ghost falls back to one of the
        adjacent non-wall tiles. A valid fallback is selected uniformly at
        random from [fallback_dirs].
      - If all adjacent tiles are walls, the ghost remains in place.

      This function enforces ghost movement logic only. Collision handling,
      death transitions, and state updates are performed by the engine. *)
  let move_ghost maze g pac_pos =
    let gx, gy = Ghost.position g in
    let desired_x, desired_y = Ghost.next_position g ~pac_pos in

    if not (Maze.is_wall maze desired_x desired_y) then
      Ghost.move_to g desired_x desired_y
    else
      let valid =
        List.filter
          (fun (dx, dy) ->
            let tx, ty = (gx + dx, gy + dy) in
            not (Maze.is_wall maze tx ty))
          fallback_dirs
      in
      match valid with
      | [] -> g
      | dirs ->
          let dx, dy = List.nth dirs (Random.int (List.length dirs)) in
          Ghost.move_to g (gx + dx) (gy + dy)
end
