type t
(** Abstract maze state. This type represents the current layout of the maze,
    including walls, pellets, and any other level-specific information. Its
    internal representation is hidden from the engine. *)

val create : unit -> t
(** [create ()] constructs the default maze for the level. The exact layout
    (walls, pellets, empty tiles) is determined by the implementation. *)

val width : t -> int
(** [width m] returns the width of maze [m] in tiles. *)

val height : t -> int
(** [height m] returns the height of maze [m] in tiles. *)

val is_wall : t -> int -> int -> bool
(** [is_wall m x y] is [true] if the tile at coordinates [(x, y)] is a wall or
    if the coordinates lie outside the maze boundaries. *)

val pellet_at : t -> int -> int -> bool
(** [pellet_at m x y] is [true] if a pellet is currently located at tile
    [(x, y)]. Returns [false] for empty tiles, walls, or out-of-bounds
    coordinates. *)

val eat_pellet : t -> int -> int -> t
(** [eat_pellet m x y] returns a new maze state where the pellet at tile
    [(x, y)] has been removed, if one existed there. If the tile does not
    contain a pellet, [m] is returned unchanged. This function is pure and does
    not mutate the original maze. *)

val pellets_exist : t -> bool
(** [pellets_exist m] is [true] if there is at least one pellet anywhere in maze
    [m]. This function short-circuits and stops scanning as soon as a pellet is
    found. When it returns [false], the maze contains no pellets and the level
    should be considered complete. *)

val is_power_pellet : t -> int -> int -> bool
(** [is_power_pellet m x y] is true if the pellet at [(x, y)] is a power pellet.
*)

(** {1 Test-Only Helpers}

    The following value is exposed only for unit testing. It allows test modules
    to construct small, custom maze layouts without relying on the default maze
    provided by [create].

    This value is wrapped in [\*\*\/\*\*] comments so it is hidden from
    generated documentation but remains accessible to the test suite. *)

(**/**)

val create_from_chars_for_tests : string list -> t

(**/**)
