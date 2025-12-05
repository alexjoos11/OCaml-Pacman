(** This file defines the module types required by the game engine functor. *)

(** [MAZE] describes the game operations for the game's grid including walls,
    pellets, power pellets, and fruits. *)
module type MAZE = sig
  (* [t] is an abstract type that represents the state of the maze including
     walls, pellets, power pellets, and fruits. *)
  type t

  (* [item] represents the items Pacman can eat. *)
  type item =
    | Pellet (* increases the score *)
    | PowerPellet (* ghosts become frightened *)
    | Cherry (* bonus fruit item *)

  (* [tile] represents a single grid cell. *)
  type tile

  val is_wall : t -> int -> int -> bool
  (** [is_wall] requires the [x] and [y] grid coordinate and returns [True] if
      there is a wall or [False] otherwise. *)

  val item_at : t -> int -> int -> item option
  (** [item_at] requires the [x] and [y] grid coordinate and returns [True] if
      there is something Pacman can eat there, [False] otherwise. *)

  val eat_item : t -> int -> int -> t
  (** [eat_item] requires the [x] and [y] grid coordinate and returns a new maze
      with an item at that coorindate removed if an item exists there. *)

  val items_exist : t -> bool
  (** [items_exist] returns [True] if there are items in maze, [False]
      otherwise. *)
end

(** [PACMAN] describes the state and movement logic of Pacman. *)
module type PACMAN = sig
  (* The abstract type representing the Pacman *)
  type t

  val create : int -> int -> t
  (** [create] requires the [x] and [y] grid coordinates and initialzed Pacman
      at that position with the default values. *)

  val position : t -> int * int
  (** [position] returns the current [x] and [y] position of Pacman. *)

  val next_position : t -> int * int
  (** [next_position] calculates the grid x and y position Pac-Man intends to
      move to. *)

  val move_to : t -> int -> int -> t
  (** [move_to] update's Pacman's position to be at [x] and [y]. *)
end

(** [GHOST] represents the state and behavior of ghosts. *)
module type GHOST = sig
  (* The abstract type representing a ghost's state. *)
  type t

  val create : int -> int -> Ai.ai -> t
  (** [create] requires [x] and [y] coordinates and the ghost's behavior [ai]
      and initializes the ghost accordingly. *)

  val position : t -> int * int
  (** [position] requires a ghost type [t] and returns the ghost's [x] and [y]
      position. *)

  val next_position : t -> pac_pos:int * int -> int * int
  (** [next_position] requires a ghost type [t] and and the pacman's position
      and calculates the ghost's next position based on it's mode. *)

  val move_to : t -> int -> int -> t
  (** [move_to] updates the ghost's state to be at a specific [x] and [y]
      position. *)

  val set_frightened : t -> bool -> t
  (** [set_frightened] requires a ghost type [t] and if the frightened mode is
      [active] enables or disables the mode based on if the mode is active. *)

  val is_frightened : t -> bool
  (** [is_frightened] returns [True] if the ghost is vulnerable to being eaten.
  *)

  val set_eaten : t -> bool -> t
  (** [set_eaten] requires a ghost type [t] and if the eaten mode is [active] 
    and then sets the ghost's state to eaten if [active] is true. *)
  val is_eaten : t -> bool
  (** [is_eaten] returns true if the ghost has been eaten by Pacman. *)
  val respawn : t -> t
  (** [respawn] resets the ghost to its starting state and location. *)
  val is_at_home : t -> bool
  (** [is_at_home] returns [True] if the ghost is currently inside the spawn 
    region. *)
  val color : t -> Raylib.Color.t
  (** [color] returns the color associated with a ghost. *)
  val update_duration : t -> time:float -> t
  (** [update_duration] requires [time] of a mode and increments timers for 
  each mode.  *)
  val speed_factor : t -> float
  (** [speed_factor] returns the value to factor for the ghost's speed based on 
  the ghost's current state.  *)
end

(** [CONSTANTS] describes the configuration values to balance the game. These 
constants are described in [constants.ml] file. *)
module type CONSTANTS = sig
  val pacman_start_pos : int * int
  val ghost_start_positions : (int * int) list
  val starting_lives : int
  val pellet_score : int
  val fps : int
  val pacdead_pause_frames : int
  val movement_delay : int
  val ghost_move_cooldown : int
  val power_pellet_score : int
  val ghost_eaten_score : int
  val power_pellet_duration_frames : int
  val cherry_score : int
end
