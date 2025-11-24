open Game_engine_interface

(** Movement logic for Pac-Man and ghosts.

    This module contains the low-level movement rules used by the game engine:
    - Pac-Man moves exactly one tile in his current direction unless blocked.
    - Ghosts attempt a chase move toward Pac-Man; if blocked, they fall back to
      a random valid adjacent direction. *)
module Make
    (Maze : MAZE)
    (Pacman : PACMAN)
    (Ghost : GHOST)
    (Constants : CONSTANTS) : sig
  val move_pacman : Maze.t -> Pacman.t -> Pacman.t
  (** [move_pacman maze pac] returns Pac-Man after attempting to move him one
      tile in his current direction.

      Behavior:
      - If the target tile is a wall according to [Maze.is_wall], Pac-Man does
        *not* move.
      - Otherwise, Pac-Man moves into the target tile using [Pacman.move_to].

      This function applies only Pac-Man’s movement rules; it does not check for
      ghosts, pellets, or collisions. Those are handled by the engine. *)

  val move_ghost : Maze.t -> Ghost.t -> int * int -> Ghost.t
  (** [move_ghost maze g pac_pos] computes the ghost’s movement for one frame,
      returning its updated position.

      Behavior:
      - First tries the ghost's chase direction produced by
        [Ghost.next_position], which always aims toward Pac-Man.
      - If that direction leads into a wall, the ghost falls back to a random
        adjacent non-wall tile.
      - If *all* adjacent tiles are walls, the ghost remains in place.

      This function performs *movement only* — collision detection, death
      handling, and state transitions are handled by the engine. *)
end
