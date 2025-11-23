(* ===================================================== *)
(*  Game Constants — Single Source of Truth              *)
(* ===================================================== *)

(* --- Maze dimensions (in tiles) --- *)
let maze_width = 28
let maze_height = 31

(* --- Tile size (in pixels) --- *)
let tile_size = 20

(* --- Window size (in pixels) --- *)
let window_width = maze_width * tile_size
let window_height = maze_height * tile_size

(* --- Frame rate --- *)
let fps = 60

(* --- Pac-Man gameplay constants --- *)
let pacman_start_pos = (15, 23)

let ghost_start_positions =
  [ (14, 14) (* Add more ghost start positions here if you want *) ]

let starting_lives = 3
let pellet_score = 10
