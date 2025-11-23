open Raylib
open Paclib
module Engine = Game_engine.Make (Maze) (Pacman) (Ghost) (Constants)

let width = 800
let height = 600
let fps = 6

let () =
  init_window width height "Pac-Man OCaml";
  set_target_fps fps;

  (* Create initial objects *)
  let maze = Maze.create () in
  let pac = Pacman.create 5 5 in
  let ghosts = [ Ghost.create 8 8 ] in

  let world = ref (Engine.initial_world maze pac ghosts) in

  while not (window_should_close ()) do
    (* 1. Input *)
    let dir_opt =
      if is_key_down Key.Up then Some Pacman.Up
      else if is_key_down Key.Down then Some Pacman.Down
      else if is_key_down Key.Left then Some Pacman.Left
      else if is_key_down Key.Right then Some Pacman.Right
      else None
    in

    let updated =
      match dir_opt with
      | Some d ->
          let w =
            if !world.state = Game_state.Intro then Engine.start !world
            else !world
          in
          { w with pac = Pacman.set_direction w.pac d }
      | None -> !world
    in

    (* 2. Engine step *)
    let new_world = Engine.update_world updated in
    world := new_world;

    (* 3. Render *)
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

  close_window ()
