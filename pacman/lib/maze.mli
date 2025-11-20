type t

val load : string -> t
val is_wall : t -> int -> int -> bool
val pellet_at : t -> int -> int -> bool
val eat_pellet : t -> int -> int -> t
val pellets_remaining : t -> int
