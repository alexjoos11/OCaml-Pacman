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

let draw (world : world_view) =
  draw_maze world.maze;
  draw_pac world.pac;
  List.iter draw_ghost world.ghosts;
  draw_text (Printf.sprintf "Score: %d" world.score) 10 10 20 Color.white;
  draw_text (Printf.sprintf "Lives: %d" world.lives) 10 40 20 Color.white
