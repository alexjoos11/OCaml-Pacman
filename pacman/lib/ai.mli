type ai = {
  attack : x:int -> y:int -> pac_pos:int * int -> int * int;
  runaway : x:int -> y:int -> pac_pos:int * int -> int * int;
  go_home : x:int -> y:int -> home:int * int -> int * int;
  color : Raylib.Color.t;
}
(** [ai] represents the movement mechanics of a specific ghost, which can be one
    of the predefined personalities defined in this mli. an ai covers the
    movement logic for ghosts in all 3 modes: attack, frightened, and eaten (how
    the ghost goes home after being eaten). *)

val defaulty : ai
(** [defaulty] is a predefined AI ghost, with our default attack, and default
    color red *)

val greenfaulty : ai
(** [greenfaulty] is a predefined AI ghost, with our default attack, but they
    are green unlike defaulty *)
