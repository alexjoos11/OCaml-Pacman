open Raylib
open Paclib

(* Instantiate the concrete engine *)
module Engine = Game_engine.Make (Maze) (Pacman) (Ghost) (Constants)

(* Window config *)
let width = Constants.window_width
let height = Constants.window_height
let fps = Constants.fps

let () =
  (* ========================================================== *)
  (*  Window Initialization                                      *)
  (* ========================================================== *)
  init_window width height "Pac-Man OCaml";
  set_target_fps fps;

  (* ========================================================== *)
  (*  Initial Game State                                         *)
  (* ========================================================== *)

  (* Maze *)
  let maze = Maze.create "data/classic.txt" in

  (* Pac-Man *)
  let px, py = Constants.pacman_start_pos in
  let pac = Pacman.create px py in

  (* Ghosts *)
  let ghosts =
    List.map
      (fun (gx, gy) -> Ghost.create gx gy)
      Constants.ghost_start_positions
  in

  (* Start world in Intro state *)
  let world = ref (Engine.initial_world maze pac ghosts) in

  (* ------------------------------------------------------------ *)
  (*  Attempt direction change, block turns into walls    *)
  (* ------------------------------------------------------------ *)
  let safe_turn desired_dir =
    let trial_pac = Pacman.set_direction !world.pac desired_dir in
    let nx, ny = Pacman.next_position trial_pac in
    if Maze.is_wall !world.maze nx ny then
      (* Reject turn — keep current direction *)
      Pacman.direction !world.pac
    else desired_dir
  in

  (* ========================================================== *)
  (*  Main Game Loop                                            *)
  (* ========================================================== *)
  while not (window_should_close ()) do
    (* ---------------------------------------------------------- *)
    (* 1. Handle Player Input                                     *)
    (* ---------------------------------------------------------- *)
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
      | Game_state.LevelComplete ->
          if is_key_pressed Key.Space then Engine.initial_world maze pac ghosts
          else !world
      | Game_state.PacDead | Game_state.GameOver ->
          (* No input in dead/game-over states *)
          !world
    in

    (* ---------------------------------------------------------- *)
    (* 2. Engine Update                                           *)
    (* ---------------------------------------------------------- *)
    world := Engine.update_world world_after_input;

    (* ---------------------------------------------------------- *)
    (* 3. Rendering                                               *)
    (* ---------------------------------------------------------- *)
    begin_drawing ();
    clear_background Color.black;

    let view =
      {
        Renderer.maze = !world.maze;
        pac = !world.pac;
        ghosts = !world.ghosts;
        score = !world.score;
        lives = !world.lives;
        state = !world.state;
      }
    in
    Renderer.draw view;

    end_drawing ()
  done;

  (* ========================================================== *)
  (*  Cleanup                                                    *)
  (* ========================================================== *)
  close_window ()
