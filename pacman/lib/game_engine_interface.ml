(** Maze interface — no dependence on Pacman or Ghost *)
module type MAZE = sig
  type t

  val is_wall : t -> int -> int -> bool
  (** True if there's a wall at (x,y) *)

  val pellet_at : t -> int -> int -> bool
  (** True if a pellet exists at (x,y) *)

  val eat_pellet : t -> int -> int -> t
  (** Remove pellet from tile, return new maze *)

  val pellets_remaining : t -> int
  (** Number of pellets left *)
end

module type PACMAN = sig
  type t

  type direction =
    | Up
    | Down
    | Left
    | Right

  val create : int -> int -> t

  val set_direction : t -> direction -> t
  (** Set desired movement direction *)

  val position : t -> int * int
  (** Current position *)

  val next_position : t -> int * int
  (** The tile Pac-Man *wants* to move to next *)

  val move_to : t -> int -> int -> t
  (** Force Pac-Man to move to (x,y). Used by the engine if legal. *)
end

module type GHOST = sig
  type t

  val create : int -> int -> t
  val position : t -> int * int

  val next_position : t -> int * int
  (** Compute the tile the ghost *wants* to move to *)

  val move_to : t -> int -> int -> t
  (** Force ghost to move to a tile *)
end

(** Game constants *)
module type CONSTANTS = sig
  val pacman_start_pos : int * int
  val ghost_start_positions : (int * int) list
  val starting_lives : int
  val pellet_score : int
end
