type t
(** Abstract maze state. This type represents the current layout of the maze,
    including walls, pellets, and any other level-specific information. Its
    internal representation is hidden from the engine. *)

val is_wall : t -> int -> int -> bool
(** [is_wall maze x y] is [true] if the tile at coordinates [(x, y)] contains a
    wall and cannot be entered by Pac-Man or ghosts. Returns [false] if the tile
    is open. *)

val pellet_at : t -> int -> int -> bool
(** [pellet_at maze x y] is [true] if a pellet is currently located at tile
    [(x, y)]. Returns [false] if there is no pellet there. *)

val eat_pellet : t -> int -> int -> t
(** [eat_pellet maze x y] returns a new maze state where the pellet at tile
    [(x, y)] has been removed (if one existed). This function is pure: it does
    not mutate the original maze. *)

val pellets_remaining : t -> int
(** [pellets_remaining maze] returns the number of pellets still present in the
    maze. When this reaches zero, the engine considers the level complete. *)
