open Game_engine_interface

(** Movement logic for Pac-Man and ghosts.

    This module contains the movement rules used by the game engine such as that
    Pac-Man moves exactly one tile in his current direction unless blocked.
    Also, ghosts attempt a chase move toward Pac-Man and if blocked, they fall
    back to a random valid direction. *)
module Make
    (Maze : MAZE)
    (Pacman : PACMAN)
    (Ghost : GHOST)
    (Constants : CONSTANTS) : sig
  val move_pacman : Maze.t -> Pacman.t -> Pacman.t
  (** [move_pacman] requires the maze [maze] and pacman's current state [pac]
      and returns Pac-Man after attempting to move him one tile in his current
      direction. If the target tile is a wall according to [Maze.is_wall],
      Pac-Man does not move. Otherwise, Pac-Man moves into the target tile using
      [Pacman.move_to]. *)

  val move_ghost : Maze.t -> Ghost.t -> int * int -> Ghost.t
  (** [move_ghost maze g pac_pos] requires the maze [maze] the ghost [g] and
      pacman's current position [pac_pos] and computes the ghost’s movement for
      one frame, returning its updated position. At first the ghost's chase
      direction is produced by [Ghost.next_position], which always positions
      toward Pac-Man. If that direction leads into a wall, the ghost falls back
      to a random non-wall tile. If all nearby tiles are walls, the ghost
      remains in place. *)
end
