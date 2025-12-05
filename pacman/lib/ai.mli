type ai = {
  attack : x:int -> y:int -> pac_pos:int * int -> int * int;
  runaway : x:int -> y:int -> pac_pos:int * int -> int * int;
  go_home : x:int -> y:int -> home:int * int -> int * int;
  color : Raylib.Color.t;
}
(** [ai] defines the movement of ghosts which defines the logic of movement
    based on different modes including [attack] the chase Pacman, [runaway]
    which is when Pacman has eaten a power pellet, and [go_home] to return back
    to initial position after being eaten. [color] corresponds to a ghost's
    color for rendering. *)

val defaulty : ai
(** [defaulty] is a AI ghost, with our default attack, and default color red *)

val orangefaulty : ai
(** [orangefaulty] is an orange default color for ghost*)

val pinkfaulty : ai
(** [pinkfaulty] is a pink default color for ghost*)

val cyanfaulty : ai
(** [cyanfaulty] is a cyan default color for ghost*)
