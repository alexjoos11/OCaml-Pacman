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

val create : int -> int -> t
(** [create x y] requires the [x] and [y] coordinates both of type int and
    returns a new ghost located at tile [(x, y)] represented as a tuple.*)

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
(** [move_to g x y] requires the ghost state [g] of type t and the new [nx] and
    [ny] coordinates and returns a new ghost with a new position. [move_to] is
    only called after calling [next_move] proving that the ghost's new position
    is allowed. *)

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
