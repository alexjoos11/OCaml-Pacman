(** This type defines the four directions that Pacman can move. *)
type direction =
  | Up
  | Down
  | Left
  | Right

type t
(** Abstract Pac-Man state which hides the internal representation of Pac-Man’s
    position and movement state from the game engine. *)

val create : int -> int -> t
(** [create] constructs a new Pac-Man located at tile [(x, y)] with an initial
    default direction. *)

val direction : t -> direction
(** [direction] returns Pac-man's direction of movemement. *)

val set_direction : t -> direction -> t
(** [set_direction] requires Pac-Man's current state [pac] and a direction [dir]
    and returns a new Pac-Man state where Pac-Man’s current direction is updated
    to [dir]. *)

val position : t -> int * int
(** [position] requires Pac-Man's current state [pac] and returns Pac-Man’s
    current tile coordinates as [(x, y)]. *)

val next_position : t -> int * int
(** [next_position] requires Pac-Man's current state [pac] and computes the tile
    [(nx, ny)] that Pac-Man *intends* to move to based solely on his current
    direction. *)

val move_to : t -> int -> int -> t
(** [move_to] requires Pac-Man's position [pac] and the [x] and [y] intended
    position and returns a new Pac-Man state positioned at tile [(x, y)]. The
    game engine uses this after deciding whether movement is legal (e.g.,
    checking for walls). *)
