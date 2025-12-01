(** Representation of Pac-Man’s movement direction. *)
type direction =
  | Up  (** Move one tile upward (decreasing y). *)
  | Down  (** Move one tile downward (increasing y). *)
  | Left  (** Move one tile left (decreasing x). *)
  | Right  (** Move one tile right (increasing x). *)

type t = {
  x : int;  (** Current x-coordinate in tile space. *)
  y : int;  (** Current y-coordinate in tile space. *)
  dir : direction;  (** Current intended movement direction. *)
}
(** Internal representation of Pac-Man's state. This includes:
    - tile coordinates [(x, y)]
    - the current movement direction

    Although this record is concrete inside the implementation, the module
    interface exposes [type t] abstractly, ensuring the game engine cannot
    depend on this internal structure. *)

(** [create x y] constructs a new Pac-Man initially positioned at tile [(x, y)].
    Pac-Man always starts facing [Right]. This choice has no gameplay effect but
    ensures a consistent initial direction. *)
let create x y = { x; y; dir = Right }

let direction p = p.dir

(** [set_direction p d] produces a new Pac-Man state identical to [p], except
    that his intended movement direction is updated to [d].

    This does *not* move Pac-Man on the map — it only records the direction that
    [next_position] will use. *)
let set_direction p d = { p with dir = d }

(** [position p] returns Pac-Man’s current tile coordinates as [(x, y)]. This is
    used by the engine for movement, collision, and rendering. *)
let position p = (p.x, p.y)

(** [next_position p] computes the tile Pac-Man *wants* to move to next, based
    solely on his current direction.

    IMPORTANT:
    - This function does *not* check walls, boundaries, ghosts, or pellets.
    - It simply expresses Pac-Man’s intended movement.
    - The game engine is responsible for rejecting illegal moves. *)
let next_position p =
  match p.dir with
  | Up -> (p.x, p.y - 1)
  | Down -> (p.x, p.y + 1)
  | Left -> (p.x - 1, p.y)
  | Right -> (p.x + 1, p.y)

(** [move_to p nx ny] returns a new Pac-Man state located at tile [(nx, ny)].
    The movement rules (e.g., wall blocking) are enforced by the game engine
    *before* calling this function.

    Pac-Man never decides on his own whether movement is allowed — he only
    updates coordinates when instructed by the engine. *)
let move_to p nx ny = { p with x = nx; y = ny }
