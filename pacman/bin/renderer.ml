open Raylib
open Paclib

let tile_size = Constants.tile_size
let window_width = Constants.window_width
let window_height = Constants.window_height

(* ===================================================== *)
(*  Types                                                *)
(* ===================================================== *)

type world_view = {
  maze : Maze.t;
  pac : Pacman.t;
  ghosts : Ghost.t list;
  score : int;
  lives : int;
  state : Game_state.game_state;
}
(** [world_view] is a lightweight, read-only snapshot of the game world. The
    renderer only needs visual information (positions, score, lives, etc.), not
    the internal engine state or update logic.

    Using this keeps the renderer decoupled from the game engine and makes
    rendering purely about drawing, not game rules. *)

(* ===================================================== *)
(*  Frame Counter                                        *)
(* ===================================================== *)

(** A simple frame counter for small UI animations (blinking text, pulsing
    title, etc.). *)
let frame_counter = ref 0

let tick () = frame_counter := !frame_counter + 1

(* ===================================================== *)
(*  UI Helper Functions                                  *)
(* ===================================================== *)

(** Compute the x-position needed to horizontally center text. *)
let center_x text font_size =
  let w = measure_text text font_size in
  (window_width - w) / 2

(** Draw text horizontally centered. *)
let draw_centered text y font_size color =
  draw_text text (center_x text font_size) y font_size color

(** Draw centered text with a black outline for a retro arcade feel. *)
let draw_centered_outline text y font_size color =
  let x = center_x text font_size in
  let o = Color.black in
  draw_text text (x - 2) (y - 2) font_size o;
  draw_text text (x + 2) (y - 2) font_size o;
  draw_text text (x - 2) (y + 2) font_size o;
  draw_text text (x + 2) (y + 2) font_size o;
  draw_text text x y font_size color

(** Show text that appears and disappears on a timer. *)
let blinking ?(rate = 30) text y size color =
  if !frame_counter / rate mod 2 = 0 then draw_centered text y size color

(** Draw text that gently moves up and down for a “pulsing” effect. *)
let pulsing_title text base_y size color =
  let t = float_of_int !frame_counter /. 20.0 in
  let offset = int_of_float (sin t *. 5.0) in
  draw_centered_outline text (base_y + offset) size color

(* ===================================================== *)
(*  Maze & Entity Rendering                              *)
(* ===================================================== *)

(** Draw the maze grid, walls, power pellets, and pellets. *)
let draw_maze maze =
  let w = Maze.width maze in
  let h = Maze.height maze in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      if Maze.is_wall maze x y then
        draw_rectangle (x * tile_size) (y * tile_size) tile_size tile_size
          Color.darkblue
      else if Maze.pellet_at maze x y then
        draw_circle
          ((x * tile_size) + (tile_size / 2))
          ((y * tile_size) + (tile_size / 2))
          3.0 Color.yellow
      else if Maze.power_pellet_at maze x y then
        draw_circle
          ((x * tile_size) + (tile_size / 2))
          ((y * tile_size) + (tile_size / 2))
          6.0 Color.orange
    done
  done

(** Draw Pac-Man at his current tile. *)
let draw_pac pac =
  let px, py = Pacman.position pac in
  draw_circle
    ((px * tile_size) + (tile_size / 2))
    ((py * tile_size) + (tile_size / 2))
    10.0 Color.yellow

(** Draw a ghost. *)
let draw_ghost ghost =
  let gx, gy = Ghost.position ghost in
  draw_rectangle (gx * tile_size) (gy * tile_size) tile_size tile_size Color.red

(* ===================================================== *)
(*  Main Draw Function                                   *)
(* ===================================================== *)

(** Draw everything for one frame:
    - Maze
    - Pac-Man and ghosts (when appropriate)
    - Score and lives
    - Overlay UI depending on the current game state

    This function only draws. It does not change the world. *)
let draw (w : world_view) =
  (* Update animations *)
  tick ();

  (* Background maze *)
  draw_maze w.maze;

  (* Entities depending on game phase *)
  begin match w.state with
  | Game_state.Playing | Game_state.LevelComplete | Game_state.PacDead ->
      draw_pac w.pac;
      List.iter draw_ghost w.ghosts
  | Intro | GameOver -> ()
  end;

  (* HUD *)
  draw_text (Printf.sprintf "Score: %d" w.score) 10 10 20 Color.white;
  draw_text (Printf.sprintf "Lives: %d" w.lives) 10 35 20 Color.white;

  (* On-screen messages based on the phase of the game *)
  match w.state with
  | Game_state.Intro ->
      pulsing_title "PAC-MAN" 100 60 Color.yellow;
      blinking "Press SPACE to start" 250 30 Color.white
  | Game_state.PacDead -> draw_centered_outline "YOU DIED!" 200 50 Color.red
  | Game_state.LevelComplete ->
      draw_centered_outline "LEVEL COMPLETE!" 200 50 Color.green
  | Game_state.GameOver ->
      draw_centered_outline "GAME OVER" 200 55 Color.red;
      blinking "Press SPACE to restart" 260 25 Color.white
  | Game_state.Playing -> ()
