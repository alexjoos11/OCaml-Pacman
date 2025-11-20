type t = {
  x : int;
  y : int;
}

let create x y = { x; y }
let update t ~maze:_ ~pac:_ = t
let position t = (t.x, t.y)
