type t

val create : int -> int -> t

val update : t -> maze:Maze.t -> pac:int * int -> t
(** Pure update; receives pacman position. *)

val position : t -> int * int
