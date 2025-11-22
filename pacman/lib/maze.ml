type t = unit

let create () = ()
let width _ = 28
let height _ = 31
let is_wall _ _ _ = false
(* No walls anywhere in the stub *)

let pellet_at _ _ _ = false
(* No pellets *)

let eat_pellet m _ _ = m
(* Maze never changes *)

let pellets_remaining _ = 1
(* Pretend there is always 1 pellet so level never ends *)
