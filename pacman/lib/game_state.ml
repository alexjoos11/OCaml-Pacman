(** The various high-level states the game can be in. *)
type game_state =
  | Intro  (** Initial state before the player begins. *)
  | Playing
      (** Normal gameplay so Pac-Man and ghosts move and pellets can be eaten.
      *)
  | PacDead
      (** Pac-Man has collided with a ghost and will lose a life or end the
          game. *)
  | PowerUp
      (** Pac-Man has eaten a power pellet and is invincible so ghosts are
          frightened. *)
  | LevelComplete
      (** All pellets have been eaten and the level transitions to a new layout.
      *)
  | GameOver of {
      final_score : int;
      old_high_score : int;
      update_high_score : bool;
    }
