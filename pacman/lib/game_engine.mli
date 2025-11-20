type game_state =
  | Intro
  | Playing
  | PacDead
  | LevelComplete
  | GameOver

type world = {
  maze : Maze.t;
  pac : Pacman.t;
  ghosts : Ghost.t list;
  score : int;
  lives : int;
  state : game_state;
}

val initial_world : Maze.t -> Pacman.t -> Ghost.t list -> world

val update_world : world -> world
(** Pure update of one frame; no SDL. *)
