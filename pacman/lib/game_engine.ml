open Maze
open Pacman
open Ghost

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

let initial_world maze pac ghosts =
  { maze; pac; ghosts; score = 0; lives = 3; state = Intro }

(* Stub that does nothing yet *)
let update_world w =
  (* For now, just return world unchanged *)
  w
