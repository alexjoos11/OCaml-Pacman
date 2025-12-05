(* Maze dimensions constants (in tiles) *)
let maze_width = 28
let maze_height = 31

(* Tile size constant (in pixels) *)
let tile_size = 20

(* Window size constants (in pixels) *)
let window_width = maze_width * tile_size
let window_height = maze_height * tile_size

(* Frame rate constant *)
let fps = 60

(* --- Pac-Man gameplay constants --- *)

(* Pac-Man's starting position *)
let pacman_start_pos = (13, 23)

(* Ghosts' starting positions *)
let ghost_start_positions = [ (12, 13); (15, 13); (12, 15); (15, 15) ]

(* Initial amount of Pacman's lives *)
let starting_lives = 3

(* Points per pellet *)
let pellet_score = 10

(* Frames to pause when Pacman loses a life *)
let pacdead_pause_frames = 50

(* Delay of movement for Pacman and ghosts *)
let movement_delay = 7

(* Ghosts cooldown constant *)
let ghost_move_cooldown = 12

(* Points per power pellet *)
let power_pellet_score = 50

(* Points when a ghost is eaten *)
let ghost_eaten_score = 200

(* Power pellet duration of effects on Pacman *)
let power_pellet_duration_frames = fps * 10

(* Points per cherry *)
let cherry_score = 100
