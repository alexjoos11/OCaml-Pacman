open Raylib
open Paclib

(* Instantiate the concrete engine *)
module Engine = Game_engine.Make (Maze) (Pacman) (Ghost) (Constants)

(** [width] is set to the window width's value in constants. *)
let width = Constants.window_width
(** [height] is set to the window height's value in constants. *)
let height = Constants.window_height
(** [fps] is set to the frames per second in constants. *)
let fps = Constants.fps

let () =
  init_window width height "Pac-Man OCaml";
  set_target_fps fps;

  (* Initial Game State *)

  (* Maze Creation *)
  let maze = Maze.create "data/classic.txt" in

  (* Initial Pac-Man's position and behavior *)
  let px, py = Constants.pacman_start_pos in
  let pac = Pacman.create px py in

  (* Initial ghosts behavior *)
  let ghosts =
    let _, ghosts_rev =
      List.fold_left
        (fun (count, acc) (gx, gy) ->
          if count < 1 then
            (count + 1, Ghost.create gx gy Ai.orangefaulty :: acc)
          else if count < 2 then
            (count + 1, Ghost.create gx gy Ai.pinkfaulty :: acc)
          else if count < 3 then
            (count + 1, Ghost.create gx gy Ai.cyanfaulty :: acc)
          else (count, Ghost.create gx gy Ai.defaulty :: acc))
        (0, []) Constants.ghost_start_positions
    in
    List.rev ghosts_rev
  in

  (* Start world in Intro state *)
  let world = ref (Engine.initial_world maze pac ghosts) in

  (* Attempt direction change, block turns into walls *)
  let safe_turn desired_dir =
    let trial_pac = Pacman.set_direction !world.pac desired_dir in
    let nx, ny = Pacman.next_position trial_pac in
    if Maze.is_wall !world.maze nx ny then
      (* Reject turn and keep current direction *)
      Pacman.direction !world.pac
    else desired_dir
  in

  (* Main Game Loop *)
  while not (window_should_close ()) do
    (* Handle Player Input *)
    let world_after_input =
      match !world.state with
      | Game_state.Intro ->
          if is_key_pressed Key.Space then Engine.start !world else !world
      | Game_state.Playing | Game_state.PowerUp ->
          let dir_opt =
            if is_key_down Key.Up then Some (safe_turn Pacman.Up)
            else if is_key_down Key.Down then Some (safe_turn Pacman.Down)
            else if is_key_down Key.Left then Some (safe_turn Pacman.Left)
            else if is_key_down Key.Right then Some (safe_turn Pacman.Right)
            else None
          in
          begin match dir_opt with
          | Some d -> { !world with pac = Pacman.set_direction !world.pac d }
          | None -> !world
          end
      | Game_state.LevelComplete | Game_state.GameOver _ ->
          if is_key_pressed Key.Space then Engine.initial_world maze pac ghosts
          else !world
      | Game_state.PacDead ->
          (* No input in dead state. *)
          !world
    in

    (* Update Engine *)
    world := Engine.update_world world_after_input;

    (* Rendering *)
    begin_drawing ();
    clear_background Color.black;

    let ghosts_with_data =
      List.map (fun g -> (g, Ghost.get_speed g, Ghost.get_time g)) !world.ghosts
    in
    let view =
      {
        Renderer.maze = !world.maze;
        pac = !world.pac;
        ghosts = ghosts_with_data;
        score = !world.score;
        lives = !world.lives;
        state = !world.state;
        speedup_timer = !world.speedup_timer;
      }
    in
    Renderer.draw view;

    end_drawing ()
  done;
  close_window ()
