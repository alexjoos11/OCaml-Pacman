(** The various high-level states the game can be in. *)
type game_state =
  | Intro  (** Initial state before the player begins. *)
  | Playing
      (** Normal gameplay: Pac-Man and ghosts move, pellets can be eaten. *)
  | PacDead
      (** Pac-Man has collided with a ghost and will lose a life or end the
          game. *)
  | PowerUp
      (** Pac-Man has eaten a power pellet and is invincible; ghosts are
          frightened. *)
  | LevelComplete
      (** All pellets have been eaten; the level transitions to a new layout. *)
  | GameOver  (** No lives remain; the game ends. *)
