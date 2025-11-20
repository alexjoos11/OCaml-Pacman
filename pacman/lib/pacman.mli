type t

val create : int -> int -> t

val update : t -> maze:Maze.t -> t
(** Pure update — no SDL. Maze is used for walls & pellets. *)

val position : t -> int * int
(** Extract position *)

val powered : t -> bool
(** Whether Pac-Man is powered up. *)
