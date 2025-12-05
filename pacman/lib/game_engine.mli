open Game_engine_interface
open Game_state

module Make : functor
  (Maze : MAZE)
  (Pacman : PACMAN)
  (Ghost : GHOST)
  (Constants : CONSTANTS)
  -> sig
  type world = {
    maze : Maze.t;
        (* Representation of the maze, including walls and pellets. *)
    pac : Pacman.t;  (** Pac-Man’s current state (position and direction). *)
    ghosts : Ghost.t list;
        (* List of all ghosts currently active in the world. *)
    score : int; (* Player’s current score. *)
    lives : int;
        (* Number of lives remaining. When this reaches zero, the game ends. *)
    state : game_state;  (** The current high-level state of the game. *)
    pacdead_timer : int;
        (* Countdown in frames used when Pac-Man dies.

           When Pac-Man collides with a ghost, the engine enters the [PacDead]
           state. Instead of instantly respawning him, we pause gameplay for
           clarity.

           The timer is decremented each frame while in [PacDead]. When it
           reaches zero, the engine respawns Pac-Man (if lives remain) or
           transitions to [GameOver]. *)
    powerup_timer : int;
        (* Countdown in frames for how long the power-up (frightened)
           lasts.contents

           When Pac-Man collides with a power pellet, the enginer enters the
           [PowerUp] state. This sets all ghosts to frightened mode for a
           limited time.into frightened mode, and begins to decrement this timer
           each frame.

           When it reaches zero, the engine reverts all eaten and frighten
           ghosts to their original state and the game state transitions to
           [Playing] *)
    move_cooldown : int;
        (* [move_cooldown] is the number of frames remaining before Pac-Man and
           ghosts are allowed to move again.

           A movement delay is applied to slow down gameplay and avoid moving
           one tile per rendered frame. When [move_cooldown > 0], the engine
           skips all movement logic for the current frame and simply decrements
           the counter. *)
    ghost_move_accumulators : float list;
        (* A list of per-ghost movement counters. Each element in the list
           corresponds to a ghost in [w.ghosts]. On each frame, a value based on
           the ghost's speed mode is added to its accumulator. When an
           accumulator passes a threshold (from
           [Constants.ghost_move_cooldown]), that ghost moves and the threshold
           is subtracted from its accumulator. *)
    frames_alive : int;
        (* Used to keep track of how long pacman has been alive. This helps with
           adjusting speed *)
    speedup_timer : int;
        (* Counts frames and is used for the speedup display message *)
  }
  (** Represents the current game world and the components within it. *)

  val start : world -> world
  (** [start w] transitions the world from [Intro] into [Playing]. Calling
      [start] in any other state returns the unmodified [w]. *)

  val initial_world : Maze.t -> Pacman.t -> Ghost.t list -> world
  (** [initial_world maze pac ghosts] constructs a new game world with the given
      maze, Pac-Man starting from [pac], the given list of ghosts, score set to
      [0], lives set to [Constants.starting_lives], game state set to [Intro].
  *)

  val update_world : world -> world
  (** [update_world w] advances the game world by one update step. Behavior
      depends on [w.state] where in [Intro] or [GameOver] the world remains
      unchanged. For [Playing] Pac-Man and ghosts attempt to move, pellets may
      be eaten, collisions may occur, and the game may transition to [PacDead]
      or [LevelComplete]. In [PacDead] the engine decrements a life or
      transitions to [GameOver] if no lives remain, then resets positions.
      Lastly for [LevelComplete]: transitions to a new level layout (if
      provided) and resets positions. A new updated world value is always
      created.*)
end
