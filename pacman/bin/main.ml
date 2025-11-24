open Raylib
open Paclib

(* Apply the game engine functor to our concrete implementations *)
module Engine = Game_engine.Make (Maze) (Pacman) (Ghost) (Constants)

(* Window and frame settings pulled from Constants *)
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
  (*  Create Initial Game State                                 *)
  (* ========================================================== *)

  (* Construct the maze (hardcoded layout) *)
  let maze = Maze.create () in

  (* Pac-Man spawn *)
  let px, py = Constants.pacman_start_pos in
  let pac = Pacman.create px py in

  (* Spawn all ghosts listed in Constants *)
  let ghosts =
    List.map
      (fun (gx, gy) -> Ghost.create gx gy)
      Constants.ghost_start_positions
  in

  (* The game starts in the Intro state, so we do not call Engine.start yet. *)
  let world = ref (Engine.initial_world maze pac ghosts) in

  (* ========================================================== *)
  (*  Main Game Loop                                            *)
  (* ========================================================== *)
  while not (window_should_close ()) do
    (* ---------------------------------------------------------- *)
    (* 1. Handle Player Input                                     *)
    (* ---------------------------------------------------------- *)

    (* World after processing keyboard input *)
    let world_after_input =
      match !world.state with
      | Game_state.Intro ->
          (* Press SPACE to begin the game *)
          if is_key_pressed Key.Space then Engine.start !world else !world
      | Game_state.Playing -> (
          (* Arrow keys adjust Pac-Man’s facing direction *)
          let dir_opt =
            if is_key_down Key.Up then Some Pacman.Up
            else if is_key_down Key.Down then Some Pacman.Down
            else if is_key_down Key.Left then Some Pacman.Left
            else if is_key_down Key.Right then Some Pacman.Right
            else None
          in

          match dir_opt with
          | Some d ->
              (* Update Pac-Man’s direction (does NOT move him) *)
              { !world with pac = Pacman.set_direction !world.pac d }
          | None -> !world)
      | Game_state.LevelComplete | Game_state.GameOver ->
          if is_key_pressed Key.Space then
            (* restart everything *)
            Engine.initial_world maze pac ghosts
          else !world
      | Game_state.PacDead ->
          (* Ignore input in these states *)
          !world
    in

    (* ---------------------------------------------------------- *)
    (* 2. Advance Game State                                      *)
    (* ---------------------------------------------------------- *)

    (* Engine.update_world applies movement, pellet logic, collision detection,
       death, respawn, and state transitions. *)
    world := Engine.update_world world_after_input;

    (* ---------------------------------------------------------- *)
    (* 3. Render                                                  *)
    (* ---------------------------------------------------------- *)
    begin_drawing ();
    clear_background Color.black;

    (* Convert Engine.world into Renderer.world_view (Renderer is intentionally
       decoupled from Engine) *)
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
