type speed =
  | Fast
  | Regular
  | Slow
  | Paused

type t = {
  x : int;
  y : int;
  mode : speed;
  timer : float;
}

let next_mode current =
  match current with
  | Regular -> Fast
  | Fast -> Slow
  | Slow -> Paused
  | Paused -> Regular

let create x y = { x; y; mode = Regular; timer = 5.0 }
let position g = (g.x, g.y)
let get_time g = g.timer

let next_position g ~pac_pos:(px, py) =
  let gx, gy = (g.x, g.y) in
  if g.mode = Paused then (gx, gy)
  else
    let dx = px - gx in
    let dy = py - gy in

    if abs dx > abs dy then if dx > 0 then (gx + 1, gy) else (gx - 1, gy)
    else if dy > 0 then (gx, gy + 1)
    else if dy < 0 then (gx, gy - 1)
    else (gx, gy)

let move_to g nx ny = { g with x = nx; y = ny }
let get_speed g = g.mode
let set_speed g mode duration = { g with mode; timer = duration }

(** HELPER: to switch modes**)
let switch_mode g = { g with mode = next_mode g.mode; timer = 5.0 }

let update_duration g ~time =
  let new_timer = g.timer -. time in
  if new_timer <= 0.0 then switch_mode g else { g with timer = new_timer }

let speed_factor g =
  match get_speed g with
  | Fast -> 2.0
  | Regular -> 1.0
  | Slow -> 0.5
  | Paused -> 0.0
