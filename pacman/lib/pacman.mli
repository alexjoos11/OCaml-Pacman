type direction =
  | Up
  | Down
  | Left
  | Right

type t
(** Abstract Pac-Man state. This type hides the internal representation of
    Pac-Man’s position and movement state from the engine. *)

val create : int -> int -> t
(** [create x y] constructs a new Pac-Man located at tile [(x, y)] with an
    initial default direction (implementation-defined). *)

val set_direction : t -> direction -> t
(** [set_direction pac dir] returns a new Pac-Man state where Pac-Man’s current
    direction is updated to [dir]. This only changes the direction; it does
    *not* move Pac-Man on the map. *)

val position : t -> int * int
(** [position pac] returns Pac-Man’s current tile coordinates as [(x, y)]. *)

val next_position : t -> int * int
(** [next_position pac] computes the tile [(nx, ny)] that Pac-Man *intends* to
    move to based solely on his current direction.

    This does *not* check walls, ghosts, pellets, or any other game rules. It is
    simply the one-tile step in Pac-Man’s current direction. *)

val move_to : t -> int -> int -> t
(** [move_to pac x y] returns a new Pac-Man state positioned at tile [(x, y)].
    The game engine uses this after deciding whether movement is legal (e.g.,
    checking for walls). Pac-Man does not decide this himself. *)
