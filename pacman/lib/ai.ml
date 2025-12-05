open Raylib

type ai = {
  attack : x:int -> y:int -> pac_pos:int * int -> int * int;
  runaway : x:int -> y:int -> pac_pos:int * int -> int * int;
  go_home : x:int -> y:int -> home:int * int -> int * int;
  color : Color.t;
}
(** [ai] represents the movement and color of ghosts. *)

(** [standard_attack] requires the [x] and [y] position position of the ghost
    and the [px] and [py] position of pacman to minimize the distance to Pacman
    used in attack mode. *)
let standard_attack ~x ~y ~pac_pos:(px, py) =
  let gx, gy = (x, y) in
  let dx = px - gx in
  let dy = py - gy in

  (* Move horizontally toward Pac-Man *)
  if abs dx > abs dy then
    (* Move vertically downward, otherwise move vertically upward *)
    if dx > 0 then (gx + 1, gy) else (gx - 1, gy)
  else if dy > 0 then (gx, gy + 1)
  else if dy < 0 then (gx, gy - 1)
  (* Currently on Pac-Man's tile *)
    else (gx, gy)

(** [standard_runaway] requires the [x] and [y] position of the ghost and [px]
    and [py] position of Pacman to maximize the distance from Pacman used in
    frightened mode. *)
let standard_runaway ~x ~y ~pac_pos:(px, py) =
  let gx, gy = (x, y) in
  let dx = px - gx in
  let dy = py - gy in
  if abs dx > abs dy then if dx > 0 then (gx - 1, gy) else (gx + 1, gy)
  else if dy > 0 then (gx, gy - 1)
  else if dy < 0 then (gx, gy + 1)
  else (gx, gy)

(** [standard_go_home] requires the [x] and [y] position of the ghost and [px]
    and [py] position of Pacman for the ghost to return back to its initial
    position. *)
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

let orangefaulty : ai =
  {
    attack = standard_attack;
    runaway = standard_runaway;
    go_home = standard_go_home;
    color = Color.(orange);
  }

let pinkfaulty : ai =
  {
    attack = standard_attack;
    runaway = standard_runaway;
    go_home = standard_go_home;
    color = Color.(pink);
  }

let cyanfaulty : ai =
  {
    attack = standard_attack;
    runaway = standard_runaway;
    go_home = standard_go_home;
    color = Color.create 0 255 255 255;
  }
