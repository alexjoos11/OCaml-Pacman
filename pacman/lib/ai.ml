open Raylib

type ai = {
  attack : x:int -> y:int -> pac_pos:int * int -> int * int;
  runaway : x:int -> y:int -> pac_pos:int * int -> int * int;
  go_home : x:int -> y:int -> home:int * int -> int * int;
  color : Color.t;
}

let standard_attack ~x ~y ~pac_pos:(px, py) =
  let gx, gy = (x, y) in
  let dx = px - gx in
  let dy = py - gy in
  if abs dx > abs dy then (* Move horizontally toward Pac-Man *)
    if dx > 0 then (gx + 1, gy) else (gx - 1, gy)
  else if dy > 0 then (* Move vertically downward *) (gx, gy + 1)
  else if dy < 0 then (* Move vertically upward *) (gx, gy - 1)
  else (* Already on Pac-Man's tile *) (gx, gy)

let standard_runaway ~x ~y ~pac_pos:(px, py) =
  let gx, gy = (x, y) in
  let dx = px - gx in
  let dy = py - gy in
  if abs dx > abs dy then if dx > 0 then (gx - 1, gy) else (gx + 1, gy)
  else if dy > 0 then (gx, gy - 1)
  else if dy < 0 then (gx, gy + 1)
  else (gx, gy)

let standard_go_home ~x ~y ~home:(hx, hy) =
  let dx = hx - x in
  let dy = hy - y in
  if abs dx > abs dy then if dx > 0 then (x + 1, y) else (x - 1, y)
  else if dy > 0 then (x, y + 1)
  else if dy < 0 then (x, y - 1)
  else (x, y)

let defaulty : ai =
  {
    attack = standard_attack;
    runaway = standard_runaway;
    go_home = standard_go_home;
    color = Color.(red);
  }

let greenfaulty : ai =
  {
    attack = standard_attack;
    runaway = standard_runaway;
    go_home = standard_go_home;
    color = Color.(green);
  }
