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

val next_position : t -> int * int
(** [next_position g] computes the tile [(nx, ny)] that the ghost intends to
    move to *before* the engine checks walls or enforces legality.

    The ghost's intended movement may be:
    - fixed (e.g., always moving right),
    - random,
    - or based on any simple movement rule chosen by the implementation.

    This function does *not* update the ghost’s actual position. *)

val move_to : t -> int -> int -> t
(** [move_to g x y] returns a new ghost state positioned at tile [(x, y)]. The
    engine calls this only after determining that the ghost’s intended move is
    legal. Ghosts themselves do not perform wall checks or decide if movement is
    allowed. *)
