(* ===================================================== *)
(* Game Constants — Single Source of Truth              *)
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
(* let pacman_start_pos = (15, 23) *)
(* let ghost_start_positions = [ (9, 3); (24, 3) ] *)
let pacman_start_pos = (13, 23)
let ghost_start_positions = [ (12, 13); (15, 13); (12, 15); (15, 15) ]
let starting_lives = 3
let pellet_score = 10
let pacdead_pause_frames = 50
let movement_delay = 7
let ghost_move_cooldown = 12
let power_pellet_score = 50
let ghost_eaten_score = 200
let power_pellet_duration_frames = fps * 10
let cherry_score = 100
