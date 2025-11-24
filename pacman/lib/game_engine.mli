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
        (** Representation of the maze, including walls and pellets. *)
    pac : Pacman.t;  (** Pac-Man’s current state (position and direction). *)
    ghosts : Ghost.t list;
        (** List of all ghosts currently active in the world. *)
    score : int;  (** Player’s current score. *)
    lives : int;
        (** Number of lives remaining. When this reaches zero, the game ends. *)
    state : game_state;  (** The current high-level state of the game. *)
    pacdead_timer : int;
        (** Countdown in frames used when Pac-Man dies.

            When Pac-Man collides with a ghost, the engine enters the [PacDead]
            state. Instead of instantly respawning him, we pause gameplay for
            clarity.

            The timer is decremented each frame while in [PacDead]. When it
            reaches zero, the engine respawns Pac-Man (if lives remain) or
            transitions to [GameOver]. *)
    move_cooldown : int;
        (** Number of frames remaining before Pac-Man and ghosts are allowed to
            move again.

            A movement delay is applied after *each tile movement* to slow down
            gameplay and avoid moving one tile per rendered frame. When
            [move_cooldown > 0], the engine skips all movement logic for the
            current frame and simply decrements the counter. *)
    ghost_move_cooldown : int;
        (** Countdown in frames controlling how often ghosts are allowed to
            move.

            Unlike Pac-Man, ghosts tend to jitter or change direction too
            quickly when updated every frame at full framerate. This cooldown
            introduces a small delay between ghost movement steps.

            - When > 0, all ghosts remain still for this frame.
            - When it reaches 0, ghosts are allowed to move once, and the
              cooldown is reset to [Constants.ghost_movement_delay].

            This keeps ghosts moving smoothly and prevents rapid oscillation
            against walls or corners. *)
  }
  (** A complete snapshot of the current game world. This includes the maze,
      Pac-Man, all ghosts, scoring information, remaining lives, and the current
      high-level game state. *)

  val start : world -> world
  (** [start w] transitions the world from [Intro] into [Playing]. Calling
      [start] in any other state returns [w] unchanged. *)

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
