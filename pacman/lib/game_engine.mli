open Game_engine_interface

module Make : functor
  (Maze : MAZE)
  (Pacman : PACMAN)
  (Ghost : GHOST)
  (Constants : CONSTANTS)
  -> sig
  (** The various high-level states the game can be in. *)
  type game_state =
    | Intro  (** Initial state before the player begins. *)
    | Playing
        (** Normal gameplay: Pac-Man and ghosts move, pellets can be eaten. *)
    | PacDead
        (** Pac-Man has collided with a ghost and will lose a life or end the
            game. *)
    | LevelComplete
        (** All pellets have been eaten; the level transitions to a new layout.
        *)
    | GameOver  (** No lives remain; the game ends. *)

  type world = {
    maze : Maze.t;
        (** Representation of the maze, including walls and pellets. *)
    pac : Pacman.t;  (** Pac-Man’s current state (position and direction). *)
    ghosts : Ghost.t list;
        (** List of all ghosts currently active in the world. *)
    score : int;  (** Player’s current score. *)
    lives : int;
        (** Number of lives remaining. When this reaches zero, the game ends. *)
    state : game_state;  (** The current high-level state of the game. *)
  }
  (** A complete snapshot of the current game world. This includes the maze,
      Pac-Man, all ghosts, scoring information, remaining lives, and the current
      high-level game state. *)

  val initial_world : Maze.t -> Pacman.t -> Ghost.t list -> world
  (** [initial_world maze pac ghosts] constructs a new game world with:
      - the given maze,
      - Pac-Man starting from [pac],
      - the given list of ghosts,
      - score set to [0],
      - lives set to [Constants.starting_lives],
      - game state set to [Intro].

      No movement or pellet interactions occur at creation. *)

  val update_world : world -> world
  (** [update_world w] advances the game world by one update step.

      Behavior depends on [w.state]:
      - [Intro] or [GameOver]: the world remains unchanged.
      - [Playing]: Pac-Man and ghosts attempt to move, pellets may be eaten,
        collisions may occur, and the game may transition to [PacDead] or
        [LevelComplete].
      - [PacDead]: the engine decrements a life or transitions to [GameOver] if
        no lives remain, then resets positions.
      - [LevelComplete]: transitions to a new level layout (if provided) and
        resets positions.

      This function never mutates the original world; it always returns a new
      updated world value. *)
end
