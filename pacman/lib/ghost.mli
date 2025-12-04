(* speed represents the different modes the ghosts are and each mode is a
   different speed the ghosts move at. *)
type speed =
  | Fast
  | Regular
  | Slow
  | Paused

type t
(* Abstract ghost state. This type represents a single ghost’s position and any
   internal state needed for its movement behavior. The internal representation
   is hidden from the engine. *)

val create : int -> int -> Ai.ai -> t
(** [create x y ai] constructs a new unfrightened ghost located at tile
    [(x, y)]. The movement behavior of the ghost is determined entirely by the
    [next_position] function. *)

val position : t -> int * int
(** [position g] requires requires the ghost state [g] of type t and returns the
    ghost’s current tile coordinates as [(x, y)] represented as a tuple. *)

val next_position : t -> pac_pos:int * int -> int * int
(** [next_position g] requires the ghost state [g] of type t and a tuple
    [pac_pos] for pacman's current tile. [next_position] computes the tile
    [(nx, ny)] that the ghost intends to move to, based on Pac-Man's location
    and returns a tuple [(nx, ny)] representing the ghost's next tile. If the
    ghost's mode is [Paused] then the ghost's current position will be returned.
*)

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

val respawn : t -> t
(** [respawn g] returns a new ghost identical to [g] but located at its home
    position and in attack mode. *)

val is_at_home : t -> bool
(** [is_at_home g] is true if the ghost is currently at its home position. *)

val color : t -> Raylib.Color.t
(** [color g] returns the color of the ghost. *)

val get_speed : t -> speed
(** [get_speed] requires the ghost's state [g] of type t and returns the ghost's
    current speed mode. This is used by the game engine to know when to call
    [next_position]. *)

val set_speed : t -> speed -> float -> t
(** [set_speed] requires the ghost state [g] of type t, the new speed mode to
    apply to the ghost [mode], and the time that the speed mode lasts for
    [duration]. If [duration <= 0.0] then the mode lasts forever or until
    set_mode is called again. [set_speed] returns a new ghost state with an
    updated speed and duration for the speed mode. *)

val get_time : t -> float
(** [get_timer g] returns the remaining time on the ghost's current speed mode
    timer. *)

val update_duration : t -> time:float -> t
(** [update_duration] requires the ghost state [g] of type t and the time that
    has passed since the last update [time]. [update_duration] returns a new
    ghost state and if the timer has expired the ghost state's speed will change
    to [Regular] speed. If the timer has not expired the timer will be reduced
    by [time] and the speed mode will remain the same. *)

val speed_factor : t -> float
(** [speed_factor g] returns the movement multiplier for the ghost [g]'s current
    speed mode.

    The mapping is:
    - [Fast] → 2.0
    - [Regular] → 1.0
    - [Slow] → 0.5
    - [Paused] → 0.0

    The game engine uses this multiplier to scale the ghost’s internal movement
    accumulator, causing ghosts in different modes to move at different rates.
*)
