open Raylib
open Paclib
module Engine = Game_engine.Make (Maze) (Pacman) (Ghost) (Constants)

let width = 800
let height = 600
let fps = 60

let () =
  (* -------------------------------------------- *)
  (* Initialize Window                            *)
  (* -------------------------------------------- *)
  init_window width height "Pac-Man OCaml";
  set_target_fps fps;

  (* -------------------------------------------- *)
  (* Create Initial Game Objects                  *)
  (* -------------------------------------------- *)
  let maze = Maze.create () in
  let pac = Pacman.create 5 5 in
  let ghosts = [ Ghost.create 8 8 ] in

  (* Start the game immediately. Otherwise update_world does nothing because the
     state stays Intro. *)
  let world = ref (Engine.initial_world maze pac ghosts |> Engine.start) in

  (* -------------------------------------------- *)
  (* Game Loop                                    *)
  (* -------------------------------------------- *)
  while not (window_should_close ()) do
    (* -------------------------------------------- *)
    (* 1. Handle Input                              *)
    (* -------------------------------------------- *)
    let dir_opt =
      if is_key_down Key.Up then Some Pacman.Up
      else if is_key_down Key.Down then Some Pacman.Down
      else if is_key_down Key.Left then Some Pacman.Left
      else if is_key_down Key.Right then Some Pacman.Right
      else None
    in

    (* Apply direction change to pac *)
    let updated_world =
      match dir_opt with
      | Some d -> { !world with pac = Pacman.set_direction !world.pac d }
      | None -> !world
    in

    (* -------------------------------------------- *)
    (* 2. Update World via Engine                   *)
    (* -------------------------------------------- *)
    let new_world = Engine.update_world updated_world in
    world := new_world;

    (* -------------------------------------------- *)
    (* 3. Render                                    *)
    (* -------------------------------------------- *)
    begin_drawing ();
    clear_background Color.black;

    let view : Renderer.world_view =
      {
        maze = new_world.maze;
        pac = new_world.pac;
        ghosts = new_world.ghosts;
        score = new_world.score;
        lives = new_world.lives;
        state = new_world.state;
      }
    in

    Renderer.draw view;

    end_drawing ()
  done;

  (* -------------------------------------------- *)
  (* Cleanup                                       *)
  (* -------------------------------------------- *)
  close_window ()
