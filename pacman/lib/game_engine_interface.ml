(** This file defines the module types required by the game engine functor.

    The engine is written as a functor so that it does not depend on any
    particular implementation of mazes, Pac-Man state, ghost movement, or game
    constants. Instead, the engine only relies on the *interfaces* described
    here.

    Each module type specifies the operations the engine needs, without exposing
    how the underlying data is represented. This achieves two goals:

    1. **Abstraction:** The engine does not know or assume how mazes, Pac-Man,
    or ghosts store their internal fields. Their types are abstract, so
    implementations can change freely without modifying the engine.

    2. **Modularity and Testability:** Because the engine depends only on module
    types, different implementations can be plugged in:
    - a real maze for the actual game,
    - a simple stub maze for unit testing,
    - different ghost movement rules,
    - different starting conditions or constants.

    This structure also allows unit tests to provide minimal stub modules (e.g.,
    a maze represented as [unit]) while letting the engine behave normally.

    By defining these module types in one place, the game engine functor can
    remain completely generic, reusable, and decoupled from any particular game
    implementation. *)

module type MAZE = sig
  type t

  type item =
    | Pellet
    | PowerPellet
    | Cherry

  type tile

  val is_wall : t -> int -> int -> bool
  val item_at : t -> int -> int -> item option
  val eat_item : t -> int -> int -> t
  val items_exist : t -> bool
end

module type PACMAN = sig
  type t

  val create : int -> int -> t
  val position : t -> int * int
  val next_position : t -> int * int
  val move_to : t -> int -> int -> t
end

module type GHOST = sig
  type t

  val create : int -> int -> Ai.ai -> t
  val position : t -> int * int
  val next_position : t -> pac_pos:int * int -> int * int
  val move_to : t -> int -> int -> t
  val set_frightened : t -> bool -> t
  val is_frightened : t -> bool
  val set_eaten : t -> bool -> t
  val is_eaten : t -> bool
  val respawn : t -> t
  val is_at_home : t -> bool
  val color : t -> Raylib.Color.t
end

module type CONSTANTS = sig
  val pacman_start_pos : int * int
  val ghost_start_positions : (int * int) list
  val starting_lives : int
  val pellet_score : int
  val pacdead_pause_frames : int
  val movement_delay : int
  val ghost_move_cooldown : int
  val power_pellet_score : int
  val ghost_eaten_score : int
  val power_pellet_duration_frames : int
  val cherry_score : int
end
