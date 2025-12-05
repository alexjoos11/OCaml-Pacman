type direction =
  | Up
  | Down
  | Left
  | Right

type t = {
  x : int;
  y : int;
  dir : direction;
}

let create x y = { x; y; dir = Right }
let direction p = p.dir
let set_direction p d = { p with dir = d }
let position p = (p.x, p.y)

let next_position p =
  match p.dir with
  | Up -> (p.x, p.y - 1)
  | Down -> (p.x, p.y + 1)
  | Left -> (p.x - 1, p.y)
  | Right -> (p.x + 1, p.y)

let move_to p nx ny = { p with x = nx; y = ny }
