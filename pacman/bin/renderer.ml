open Raylib
open Paclib

let tile_size = Constants.tile_size

type world_view = {
  maze : Maze.t;
  pac : Pacman.t;
  ghosts : Ghost.t list;
  score : int;
  lives : int;
  state : Game_state.game_state;
}

let draw_maze maze =
  let width = Maze.width maze in
  let height = Maze.height maze in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if Maze.is_wall maze x y then
        draw_rectangle (x * tile_size) (y * tile_size) tile_size tile_size
          Color.darkgray
      else if Maze.pellet_at maze x y then
        draw_circle
          ((x * tile_size) + (tile_size / 2))
          ((y * tile_size) + (tile_size / 2))
          3.0 Color.yellow
    done
  done

let draw_pac pac =
  let px, py = Pacman.position pac in
  draw_circle
    ((px * tile_size) + (tile_size / 2))
    ((py * tile_size) + (tile_size / 2))
    10.0 Color.yellow

let draw_ghost ghost =
  let gx, gy = Ghost.position ghost in
  draw_rectangle (gx * tile_size) (gy * tile_size) tile_size tile_size Color.red

let draw (w : world_view) =
  (* draw the maze in all states except maybe intro/gameover *)
  draw_maze w.maze;

  (* draw entities only during gameplay-like states *)
  begin
    match w.state with
    | Game_state.Playing | Game_state.LevelComplete | Game_state.PacDead ->
        draw_pac w.pac;
        List.iter draw_ghost w.ghosts
    | Intro | GameOver ->
        () (* do not draw pac/ghosts on title or gameover if you don't want *)
  end;

  (* UI always visible *)
  draw_text (Printf.sprintf "Score: %d" w.score) 10 10 20 Color.white;
  draw_text (Printf.sprintf "Lives: %d" w.lives) 10 35 20 Color.white;

  (* Overlay text depending on game state *)
  match w.state with
  | Game_state.Intro ->
      draw_text "PAC-MAN" 120 120 40 Color.yellow;
      draw_text "Press SPACE to start" 80 220 30 Color.white
  | Game_state.PacDead -> draw_text "YOU DIED!" 140 200 40 Color.red
  | Game_state.LevelComplete ->
      draw_text "LEVEL COMPLETE!" 100 200 40 Color.green
  | Game_state.GameOver -> draw_text "GAME OVER" 140 200 40 Color.red
  | Game_state.Playing -> ()
