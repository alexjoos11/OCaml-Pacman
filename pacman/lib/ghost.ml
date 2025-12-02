type t = {
  x : int;  (** Current x-coordinate. *)
  y : int;  (** Current y-coordinate. *)
}


(** [create x y] creates a new ghost located at tile [(x, y)]. *)
let create x y = { x; y }

(** [position g] returns the ghost's current tile coordinates as [(x, y)]. *)
let position g = (g.x, g.y)

(** [next_position g ~pac_pos] computes the tile that the ghost *intends* to
    move to next based on Pac-Man’s current position [pac_pos].

    This is a simplified version of **Blinky’s chase algorithm** from the
    original Pac-Man: the ghost moves one tile toward Pac-Man along whichever
    axis has the largest distance to close. If the distances are equal, the
    ghost prefers vertical movement.

    This produces fast, predictable “direct chaser” behavior without
    implementing the full multi-mode AI (scatter/frightened/chase cycles).

    IMPORTANT: This function does *not* perform wall or boundary checks. The
    game engine decides whether the move is legal before applying it. *)

let next_position g ~pac_pos:(px, py) =
  let gx, gy = (g.x, g.y) in
  let dx = px - gx in
  let dy = py - gy in

  if abs dx > abs dy then
    (* Move horizontally toward Pac-Man *)
    if dx > 0 then (gx + 1, gy) else (gx - 1, gy)
  else if dy > 0 then
    (* Move vertically downward *)
    (gx, gy + 1)
  else if dy < 0 then
    (* Move vertically upward *)
    (gx, gy - 1)
  else
    (* Already on Pac-Man's tile *)
    (gx, gy)

(** [move_to g nx ny] returns a new ghost located at tile [(nx, ny)]. The game
    engine calls this after confirming that movement to the tile is legal (i.e.,
    not a wall). *)

let move_to g nx ny = { x = nx; y = ny }
