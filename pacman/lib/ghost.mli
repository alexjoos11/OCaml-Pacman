type t
(** Abstract ghost state. This type represents a single ghost’s position and any
    internal state needed for its movement behavior. The internal representation
    is hidden from the engine. *)

val create : int -> int -> t
(** [create x y] constructs a new unfrightened ghost located at tile [(x, y)].
    The movement behavior of the ghost is determined entirely by the
    [next_position] function. *)

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

val set_frightened : t -> bool -> t
(** [set_frightened g frightened] returns a new ghost identical to [g] but with
    its [frightened] state set to [frightened]. *)

val is_frightened : t -> bool
(** [is_frightened g] is true if the ghost is currently in frightened mode. *)

val set_eaten : t -> bool -> t
(** [set_eaten g eaten] returns a new ghost identical to [g] but with its
    [eaten] state set to [eaten]. *)

val is_eaten : t -> bool
(** [is_eaten g] is true if the ghost has been eaten by Pac-Man*)
