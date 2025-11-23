(* the direction of the pacman*)
type direction

type t
(** Abstract ghost state. This type represents a single ghost’s position and any
    internal state needed for its movement behavior. The internal representation
    is hidden from the engine. *)

val create : int -> int -> t
(** [create x y] constructs a new ghost located at tile [(x, y)]. The movement
    behavior of the ghost is determined entirely by the [next_position]
    function. *)

val position : t -> int * int
(** [position g] returns the ghost’s current tile coordinates as [(x, y)]. *)

val next_position : t -> pac_pos:int * int -> int * int
(** [next_position g] computes the tile [(nx, ny)] that the ghost intends to
    move to, based on Pac-Man's location [pac_pos]*)

val move_to : t -> int -> int -> t
(** [move_to g x y] returns a new ghost state positioned at tile [(x, y)]. The
    engine calls this only after determining that the ghost’s intended move is
    legal. Ghosts themselves do not perform wall checks or decide if movement is
    allowed. *)

val follow : t -> direction -> t
(*for any ghost that will follow the pacman character, this function takes in
  the current direction of the pacman and determines its movement thusly. The
  ghost(s) will be moving with a constant speed after starting a specified [x]
  number of pixels behind the pacman. If the pacman is moving straight, this
  function tells the ghost to do the same. However, if the pacman turns, the
  ghost will know which direction to turn in [initial displacement]/[speed]
  seconds*)
