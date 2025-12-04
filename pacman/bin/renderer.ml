open Raylib
open Paclib

let tile_size = Constants.tile_size
let window_width = Constants.window_width
let window_height = Constants.window_height

(* ===================================================== *)
(* Types                                                *)
(* ===================================================== *)

type world_view = {
  maze : Maze.t;
  pac : Pacman.t;
  (* --- THIS TYPE IS MODIFIED --- *)
  ghosts : (Ghost.t * Ghost.speed * float) list;
  score : int;
  lives : int;
  state : Game_state.game_state;
}
(** [world_view] is a lightweight, read-only snapshot of the game world. ...
    (omitted comment) ... *)

(* ===================================================== *)
(* Frame Counter                                        *)
(* ===================================================== *)

(** A simple frame counter for small UI animations (blinking text, pulsing
    title, etc.). *)
let frame_counter = ref 0

let tick () = frame_counter := !frame_counter + 1

(* ===================================================== *)
(* UI Helper Functions                                  *)
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

(** [string_of_speed_mode mode] converts a speed mode into a printable string.
*)
let string_of_speed_mode mode =
  match mode with
  | Ghost.Fast -> "Fast"
  | Ghost.Regular -> "Regular"
  | Ghost.Slow -> "Slow"
  | Ghost.Paused -> "Paused"

(* ===================================================== *)
(* Maze & Entity Rendering                              *)
(* ===================================================== *)

(** Draw the maze grid, walls, and pellets. *)
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
    done
  done

(* determines position of mouth *)
let facing_angle = function
  | Pacman.Right -> 0.0
  | Pacman.Left -> 180.0
  | Pacman.Up -> 270.0
  | Pacman.Down -> 90.0

(** Draw Pac-Man at his current tile. *)
let draw_pac pac =
  let px, py = Pacman.position pac in
  let cx = (px * tile_size) + (tile_size / 2) in
  let cy = (py * tile_size) + (tile_size / 2) in

  (* open and clos emouth *)
  let t = Raylib.get_time () in
  let max_open = 50.0 in
  let mouth = max_open *. abs_float (sin (t *. 8.0)) in

  (* determining which direction the mouth should face *)
  let angle = facing_angle (Pacman.direction pac) in

  (* creating wedge for mouth *)
  let start_angle = angle +. (mouth /. 2.0) in
  let end_angle = angle +. 360.0 -. (mouth /. 2.0) in

  (* draws pacman *)
  Raylib.draw_circle_sector
    (Vector2.create (float cx) (float cy))
    10.0 start_angle end_angle 32 Color.yellow

(** Draw a ghost. *)
let draw_ghost (ghost, mode, _timer) =
  (* The _timer tells OCaml we are receiving it but ignoring it here *)
  let gx, gy = Ghost.position ghost in
  let ghost_color =
    match mode with
    | Ghost.Slow -> Color.blue
    | Ghost.Paused -> Color.yellow
    | Ghost.Fast -> Color.orange
    | Ghost.Regular -> Color.red
  in
  draw_rectangle (gx * tile_size) (gy * tile_size) tile_size tile_size
    ghost_color

(* ===================================================== *)
(* Main Draw Function                                   *)
(* ===================================================== *)

(** Draw everything for one frame. ... (omitted comment) ... *)
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
  | Game_state.Intro | Game_state.GameOver _ -> ()
  end;

  (* HUD *)
  draw_text (Printf.sprintf "Score: %d" w.score) 10 10 20 Color.white;
  draw_text (Printf.sprintf "Lives: %d" w.lives) 10 35 20 Color.white;

  (* --- THIS SECTION IS MODIFIED --- *)
  (* Draw ghost speed modes in the top-left *)
  List.iteri
    (fun i (_ghost, mode, timer) ->
      (* 1. Get the mode as a string *)
      let mode_text = string_of_speed_mode mode in

      (* 2. Get the timer as a string, but only if it's active *)
      let timer_text =
        if timer > 0.0 then Printf.sprintf " (%.1fs)" timer else ""
      in

      (* 3. Combine them and draw *)
      let text = Printf.sprintf "Ghost %d: %s%s" (i + 1) mode_text timer_text in
      let y_pos = 60 + (i * 25) in
      draw_text text 10 y_pos 20 Color.white)
    w.ghosts;

  (* --- END OF MODIFIED SECTION --- *)

  (* On-screen messages based on the phase of the game *)
  match w.state with
  | Game_state.Intro ->
      pulsing_title "PAC-MAN" 100 60 Color.yellow;
      blinking "Press SPACE to start" 250 30 Color.white
  | Game_state.PacDead -> draw_centered_outline "YOU DIED!" 200 50 Color.red
  | Game_state.LevelComplete ->
      draw_centered_outline "LEVEL COMPLETE!" 200 50 Color.green
  | Game_state.GameOver info ->
      draw_centered_outline "GAME OVER" 200 55 Color.red;
      let score_text = Printf.sprintf "Final Score: %d" info.final_score in
      draw_centered score_text 280 30 Color.white;
      let high_score =
        if info.update_high_score then info.final_score else info.old_high_score
      in
      let high_score_text = Printf.sprintf "High Score: %d" high_score in
      draw_centered high_score_text 320 30 Color.yellow;

      if info.update_high_score then
        blinking "NEW HIGH SCORE!" 370 35 Color.green;

      blinking "Press SPACE to restart" 260 25 Color.white
  | Game_state.Playing -> ()
