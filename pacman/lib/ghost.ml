type direction =
  | Up
  | Down
  | Left
  | Right

type t = {
  x : int;
  y : int;
}

(** [create] creates a new ghost at the given coordinates [x] and [y]. *)
let create x y = { x; y }

(** [position] returns the ghost's current position (x, y). *)
let position g = (g.x, g.y)

(** [next_position] calculated the next tile by chasing Pac-Man's position
    [pac_pos]. *)
let next_position g ~pac_pos:(px, py) =
  (* Get ghost's current position. *)
  let gx = g.x in
  let gy = g.y in

  (* Calculate horizontal and vertical distance to Pac-Man. *)
  let dx = px - gx in
  let dy = py - gy in

  (* Blinky's AI: 1. Find the axis (horizontal or vertical) with the largest
     distance. 2. Move one step along that axis to close the distance. 3. If the
     distances are equal then move vertically. *)
  if abs dx > abs dy then if dx > 0 then (gx + 1, gy) else (gx - 1, gy)
  else if dy > 0 then (gx, gy + 1)
  else if dy < 0 then (gx, gy - 1)
  else (gx, gy)

(** [move_to] requires [nx] and [ny] which are the x and y coordinates for the
    ghost's new position and is called by the engine after it has confirmed that
    the move to (nx, ny) is legal. This function returns a new ghost record with
    the updated position. *)
let move_to g nx ny = { x = nx; y = ny }
