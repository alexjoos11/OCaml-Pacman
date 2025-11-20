type t = {
  x : int;
  y : int;
  powered : bool;
}

let create x y = { x; y; powered = false }

(* Stub just returns same pacman *)
let update t ~maze:_ = t
let position t = (t.x, t.y)
let powered t = t.powered
