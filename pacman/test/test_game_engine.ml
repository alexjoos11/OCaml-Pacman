open OUnit2
open Paclib.Game_engine_interface
open Paclib.Game_state
open Paclib.Maze
open Paclib.Ai

(* Stub Modules *)
module StubMaze = struct
  type t = unit

  type item =
    | Pellet
    | PowerPellet
    | Cherry

  type tile =
    | Wall
    | Item of item
    | Empty

  let is_wall _ _ _ = false
  let item_at _ _ _ = None
  let eat_item m _ _ = m
  let items_exist _ = true
  let pellet_at _ _ _ = false
  let eat_pellet m _ _ = m
  let pellets_exist _ = true
  let is_power_pellet _ _ _ = false
end

module StubPacman : PACMAN = struct
  type direction =
    | Up
    | Down
    | Left
    | Right

  type t = {
    x : int;
    y : int;
    dir : direction;
  }

  let create x y = { x; y; dir = Right }
  let set_direction t d = { t with dir = d }
  let position t = (t.x, t.y)

  let next_position t =
    match t.dir with
    | Up -> (t.x, t.y - 1)
    | Down -> (t.x, t.y + 1)
    | Left -> (t.x - 1, t.y)
    | Right -> (t.x + 1, t.y)

  let move_to t nx ny = { t with x = nx; y = ny }
end

module StubGhost : GHOST = struct
  type t = {
    x : int;
    y : int;
  }

  let create x y _ = { x; y }
  let position g = (g.x, g.y)
  let next_position g ~pac_pos:_ = (g.x, g.y)
  let move_to g nx ny = { x = nx; y = ny }
  let is_frightened _ = false
  let is_eaten _ = false
  let color _ = Raylib.Color.red
  let respawn g = g
  let set_frightened g _ = g
  let set_eaten g _ = g
  let is_at_home _ = false
  let update_duration g ~time:_ = g
  let speed_factor _ = 2.0
end

module StubConstants = struct
  let pacman_start_pos = (5, 5)
  let ghost_start_positions = [ (10, 10) ]
  let starting_lives = 3
  let pellet_score = 10
  let pacdead_pause_frames = 50
  let movement_delay = 5
  let ghost_move_cooldown = 12
  let power_pellet_duration_frames = 600
  let cherry_score = 100
  let power_pellet_score = 50
  let ghost_eaten_score = 200
  let fps = 60
end

module Engine =
  Paclib.Game_engine.Make (StubMaze) (StubPacman) (StubGhost) (StubConstants)

(* Stub Modules for Ghost Movement *)
module MovingGhost : GHOST = struct
  type t = {
    x : int;
    y : int;
  }

  let create x y _ = { x; y }
  let position g = (g.x, g.y)
  let next_position g ~pac_pos:_ = (g.x + 1, g.y)
  let move_to _ nx ny = { x = nx; y = ny }
  let update_duration g ~time:_ = g
  let speed_factor _ = 1.0
  let is_frightened _ = false
  let is_eaten _ = false
  let set_frightened g _ = g
  let set_eaten g _ = g
  let respawn g = g
  let is_at_home _ = false
  let color _ = Raylib.Color.red
end

module OpenMaze = struct
  type t = unit

  type item =
    | Pellet
    | PowerPellet
    | Cherry

  type tile =
    | Wall
    | Item of item
    | Empty

  let is_wall _ _ _ = false
  let pellet_at _ _ _ = false
  let eat_pellet m _ _ = m
  let pellets_exist _ = true
  let is_power_pellet _ _ _ = false
  let create_for_tests _ = ()
  let item_at _ _ _ = None
  let eat_item m _ _ = m
  let items_exist _ = true
end

module GhostMoveConstants = struct
  let pacman_start_pos = (0, 0)
  let ghost_start_positions = [ (0, 5) ]
  let starting_lives = 3
  let pellet_score = 10
  let pacdead_pause_frames = 50
  let movement_delay = 1
  let ghost_move_cooldown = 2
  let power_pellet_score = 50
  let fps = 60
  let power_pellet_duration_frames = 600
  let cherry_score = 100
  let ghost_eaten_score = 200
end

module EngineGhostMove =
  Paclib.Game_engine.Make (OpenMaze) (StubPacman) (MovingGhost)
    (GhostMoveConstants)

(* OUnit Printers *)
let string_of_pos (x, y) = Printf.sprintf "(%d, %d)" x y

let string_of_game_state = function
  | Intro -> "Intro"
  | Playing -> "Playing"
  | LevelComplete -> "LevelComplete"
  | PacDead -> "PacDead"
  | GameOver _ -> "GameOver"
  | PowerUp -> "PowerUp"

(* A basic printer for the world state *)
let string_of_world w =
  let open Engine in
  Printf.sprintf "World { state = %s; lives = %d; score = %d }"
    (string_of_game_state w.state)
    w.lives w.score

(* Helper functions for making a world *)
let mk_world () =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 10 10 Paclib.Ai.defaulty in
  Engine.initial_world () pac [ ghost ]

(* Tests for basic, game over, and intro. *)
let test_intro_no_update _ =
  let w = mk_world () in
  assert_equal ~printer:string_of_world w (Engine.update_world w)

let test_start_enters_playing _ =
  let w = mk_world () in
  assert_equal ~printer:string_of_game_state Playing (Engine.start w).state

let test_intro_stable _ =
  let w = mk_world () in
  assert_equal ~printer:string_of_world w (Engine.update_world w)

let test_gameover_stable _ =
  let w =
    let open Engine in
    {
      (mk_world ()) with
      state =
        GameOver
          { final_score = 0; old_high_score = 0; update_high_score = false };
    }
  in
  assert_equal ~printer:string_of_world w (Engine.update_world w)

(* Tests for Pac-Man's movement *)
let test_pacman_moves _ =
  let w = Engine.start (mk_world ()) in
  let w' = Engine.update_world w in
  assert_equal ~printer:string_of_pos (6, 5) (StubPacman.position w'.pac)

module WallMaze = struct
  type t = unit

  type item =
    | Pellet
    | PowerPellet
    | Cherry

  type tile =
    | Wall
    | Item of item
    | Empty

  let is_wall _ x y = (x, y) = (6, 5)
  let item_at _ _ _ = None
  let eat_item m _ _ = m
  let items_exist _ = true
end

module EngineWall =
  Paclib.Game_engine.Make (WallMaze) (StubPacman) (StubGhost) (StubConstants)

let test_wall_blocks_pacman _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 10 10 Paclib.Ai.defaulty in
  let w = EngineWall.initial_world () pac [ ghost ] |> EngineWall.start in
  assert_equal ~printer:string_of_pos (5, 5)
    (StubPacman.position (EngineWall.update_world w).pac)

(* Tests for ghost movement *)

let test_ghost_moves_when_accumulator_reaches_threshold _ =
  let pac = StubPacman.create 1 1 in
  let ghost = MovingGhost.create 0 5 Paclib.Ai.defaulty in
  let w =
    EngineGhostMove.initial_world () pac [ ghost ] |> EngineGhostMove.start
  in

  let w1 = EngineGhostMove.update_world w in
  assert_equal ~printer:string_of_pos (0, 5)
    (MovingGhost.position (List.hd w1.ghosts));

  let w2 = EngineGhostMove.update_world w1 in
  assert_equal ~printer:string_of_pos (1, 5)
    (MovingGhost.position (List.hd w2.ghosts))

let test_ghost_moves_every_second_frame _ =
  let pac = StubPacman.create 1 1 in
  let ghost = MovingGhost.create 0 5 Paclib.Ai.defaulty in
  let w =
    EngineGhostMove.initial_world () pac [ ghost ] |> EngineGhostMove.start
  in

  let w1 = EngineGhostMove.update_world w in
  let w2 = EngineGhostMove.update_world w1 in
  let w3 = EngineGhostMove.update_world w2 in
  let w4 = EngineGhostMove.update_world w3 in

  assert_equal ~printer:string_of_pos (0, 5)
    (MovingGhost.position (List.hd w1.ghosts));
  assert_equal ~printer:string_of_pos (1, 5)
    (MovingGhost.position (List.hd w2.ghosts));
  assert_equal ~printer:string_of_pos (1, 5)
    (MovingGhost.position (List.hd w3.ghosts));
  assert_equal ~printer:string_of_pos (2, 5)
    (MovingGhost.position (List.hd w4.ghosts))

(* Tests checking speed factor *)

(* Slow ghost: speed_factor = 0.5 so ghost moves every four frames *)
module SlowGhost : GHOST = struct
  type t = {
    x : int;
    y : int;
  }

  let create x y _ = { x; y }
  let position g = (g.x, g.y)
  let next_position g ~pac_pos:_ = (g.x + 1, g.y)
  let move_to _ nx ny = { x = nx; y = ny }
  let update_duration g ~time:_ = g
  let speed_factor _ = 0.5
  let is_frightened _ = false
  let is_eaten _ = false
  let set_frightened g _ = g
  let set_eaten g _ = g
  let respawn g = g
  let is_at_home _ = false
  let color _ = Raylib.Color.red
end

module EngineSlow =
  Paclib.Game_engine.Make (OpenMaze) (StubPacman) (SlowGhost)
    (GhostMoveConstants)

let test_slow_ghost_moves_every_four_frames _ =
  let pac = StubPacman.create 0 0 in
  let ghost = SlowGhost.create 0 5 Paclib.Ai.defaulty in
  let w = EngineSlow.initial_world () pac [ ghost ] |> EngineSlow.start in

  let w1 = EngineSlow.update_world w in
  let w2 = EngineSlow.update_world w1 in
  let w3 = EngineSlow.update_world w2 in
  let w4 = EngineSlow.update_world w3 in

  assert_equal ~printer:string_of_pos (0, 5)
    (SlowGhost.position (List.hd w1.ghosts));
  assert_equal ~printer:string_of_pos (0, 5)
    (SlowGhost.position (List.hd w2.ghosts));
  assert_equal ~printer:string_of_pos (0, 5)
    (SlowGhost.position (List.hd w3.ghosts));
  assert_equal ~printer:string_of_pos (1, 5)
    (SlowGhost.position (List.hd w4.ghosts))

(* Fast ghost: speed_factor = 2.0 so ghosts move every frame *)
module FastGhost : GHOST = struct
  type t = {
    x : int;
    y : int;
  }

  let create x y _ = { x; y }
  let position g = (g.x, g.y)
  let next_position g ~pac_pos:_ = (g.x + 1, g.y)
  let move_to _ nx ny = { x = nx; y = ny }
  let update_duration g ~time:_ = g
  let speed_factor _ = 2.0
  let is_frightened _ = false
  let is_eaten _ = false
  let set_frightened g _ = g
  let set_eaten g _ = g
  let respawn g = g
  let is_at_home _ = false
  let color _ = Raylib.Color.red
end

module EngineFast =
  Paclib.Game_engine.Make (OpenMaze) (StubPacman) (FastGhost)
    (GhostMoveConstants)

let test_fast_ghost_moves_every_frame _ =
  let pac = StubPacman.create 0 0 in
  let ghost = FastGhost.create 0 5 Paclib.Ai.defaulty in
  let w = EngineFast.initial_world () pac [ ghost ] |> EngineFast.start in

  let w1 = EngineFast.update_world w in
  let w2 = EngineFast.update_world w1 in

  assert_equal ~printer:string_of_pos (1, 5)
    (FastGhost.position (List.hd w1.ghosts));
  assert_equal ~printer:string_of_pos (2, 5)
    (FastGhost.position (List.hd w2.ghosts))

(* Test for fractional carry over *)
module FractionGhost : GHOST = struct
  type t = {
    x : int;
    y : int;
  }

  let create x y _ = { x; y }
  let position g = (g.x, g.y)
  let next_position g ~pac_pos:_ = (g.x + 1, g.y)
  let move_to _ nx ny = { x = nx; y = ny }
  let update_duration g ~time:_ = g
  let speed_factor _ = 0.4
  let is_frightened _ = false
  let is_eaten _ = false
  let set_frightened g _ = g
  let set_eaten g _ = g
  let respawn g = g
  let is_at_home _ = false
  let color _ = Raylib.Color.red
end

module EngineFraction =
  Paclib.Game_engine.Make (OpenMaze) (StubPacman) (FractionGhost)
    (GhostMoveConstants)

let test_accumulator_carry_over _ =
  let pac = StubPacman.create 0 0 in
  let ghost = FractionGhost.create 0 5 Paclib.Ai.defaulty in
  let w =
    EngineFraction.initial_world () pac [ ghost ] |> EngineFraction.start
  in

  let w =
    let open EngineFraction in
    { w with ghost_move_accumulators = [ 1.7 ] }
  in
  let w1 = EngineFraction.update_world w in

  assert_equal ~printer:string_of_pos (1, 5)
    (FractionGhost.position (List.hd w1.ghosts));
  assert_bool "carry-over ≈ 0.1"
    (abs_float (List.hd w1.EngineFraction.ghost_move_accumulators -. 0.1)
    < 0.0001)

(* Test for cooldown movement *)
let test_pacman_frozen_when_cooldown _ =
  let w = Engine.start (mk_world ()) in
  let w =
    let open Engine in
    { w with move_cooldown = 3 }
  in
  let w' = Engine.update_world w in
  assert_equal ~printer:string_of_pos
    (StubPacman.position w.pac)
    (StubPacman.position w'.pac);
  assert_equal ~printer:string_of_int 2 w'.Engine.move_cooldown

let test_pacman_moves_after_cooldown_expires _ =
  let w = Engine.start (mk_world ()) in
  let w =
    let open Engine in
    { w with move_cooldown = 1 }
  in
  let w1 = Engine.update_world w in
  assert_equal ~printer:string_of_pos
    (StubPacman.position w.pac)
    (StubPacman.position w1.pac);
  assert_equal ~printer:string_of_int 0 w1.Engine.move_cooldown;
  let w2 = Engine.update_world w1 in
  assert_equal ~printer:string_of_pos (6, 5) (StubPacman.position w2.pac)

let test_ghosts_frozen_when_cooldown _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 3 3 Paclib.Ai.defaulty in
  let w = Engine.initial_world () pac [ ghost ] |> Engine.start in
  let w =
    let open Engine in
    { w with move_cooldown = 2 }
  in
  let w' = Engine.update_world w in
  assert_equal ~printer:string_of_pos (3, 3)
    (StubGhost.position (List.hd w'.ghosts));
  assert_equal ~printer:string_of_int 1 w'.Engine.move_cooldown

(* Tests for when Pac-Man eats pellets *)
module PelletMaze = struct
  type t = unit

  type item =
    | Pellet
    | PowerPellet
    | Cherry

  type tile =
    | Wall
    | Item of item
    | Empty

  let is_wall _ _ _ = false
  let item_at _ x y = Some Pellet
  let eat_item m _ _ = m
  let items_exist _ = true
end

module EnginePellet =
  Paclib.Game_engine.Make (PelletMaze) (StubPacman) (StubGhost) (StubConstants)

let test_pacman_eats_pellet _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 10 10 Paclib.Ai.defaulty in
  let w = EnginePellet.initial_world () pac [ ghost ] |> EnginePellet.start in
  let w' = EnginePellet.update_world w in
  assert_equal ~printer:string_of_int StubConstants.pellet_score
    w'.EnginePellet.score

(* Tests for when a level completes *)
module EmptyMaze = struct
  type t = Paclib.Maze.t

  type item =
    | Pellet
    | PowerPellet
    | Cherry

  type tile =
    | Wall
    | Item of item
    | Empty

  let is_wall _ _ _ = false
  let item_at _ _ _ = None
  let eat_item m _ _ = m
  let items_exist _ = false
  let create_for_tests _ = create_for_tests "../data/test_empty.txt"
end

module EngineEmpty =
  Paclib.Game_engine.Make (EmptyMaze) (StubPacman) (StubGhost) (StubConstants)

let test_level_complete _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 10 10 Paclib.Ai.defaulty in
  let maze = EmptyMaze.create_for_tests () in
  let w = EngineEmpty.initial_world maze pac [ ghost ] |> EngineEmpty.start in
  assert_equal ~printer:string_of_game_state LevelComplete
    (EngineEmpty.update_world w).state

(* Test for when Pacman dies *)
let test_pac_dead_transition _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 5 5 Paclib.Ai.defaulty in
  let w = Engine.initial_world () pac [ ghost ] |> Engine.start in
  assert_equal ~printer:string_of_game_state PacDead
    (Engine.update_world w).state

let test_game_over_when_no_lives_left _ =
  let w = mk_world () in
  let w =
    let open Engine in
    { w with state = PacDead; lives = 1 }
  in
  let w' = Engine.update_world w in
  match w'.state with
  | GameOver _ -> ()
  | _ -> assert_failure "Didn't transition to GameOver state"

let test_pacdead_timer_counts_down _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 5 5 Paclib.Ai.defaulty in
  let w =
    Engine.initial_world () pac [ ghost ] |> Engine.start |> Engine.update_world
  in
  assert_equal ~printer:string_of_game_state PacDead w.state;
  let w2 = Engine.update_world w in
  assert_equal ~printer:string_of_int
    (StubConstants.pacdead_pause_frames - 1)
    w2.Engine.pacdead_timer

let test_pac_is_frozen_during_pacdead _ =
  let w =
    let open Engine in
    { (mk_world ()) with state = PacDead; pacdead_timer = 10 }
  in
  let w' = Engine.update_world w in
  assert_equal ~printer:string_of_pos
    (StubPacman.position w.pac)
    (StubPacman.position w'.pac);
  assert_equal ~printer:string_of_int 9 w'.Engine.pacdead_timer

let test_ghosts_frozen_during_pacdead _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 3 3 Paclib.Ai.defaulty in
  let w =
    let open Engine in
    {
      (Engine.initial_world () pac [ ghost ]) with
      state = PacDead;
      pacdead_timer = 5;
    }
  in
  let w' = Engine.update_world w in
  assert_equal ~printer:string_of_pos (3, 3)
    (StubGhost.position (List.hd w'.ghosts))

let test_respawn_after_pacdead_timer _ =
  let w =
    let open Engine in
    { (mk_world () |> Engine.start) with state = PacDead; pacdead_timer = 0 }
  in
  let w' = Engine.update_world w in
  assert_equal ~printer:string_of_game_state Playing w'.state;
  assert_equal ~printer:string_of_int
    (StubConstants.starting_lives - 1)
    w'.Engine.lives;
  assert_equal ~printer:string_of_pos StubConstants.pacman_start_pos
    (StubPacman.position w'.pac)

(* A maze where we can define exactly where the walls are via a list of
   coordinates *)
module ConfigurableMaze = struct
  type t = (int * int) list (* The maze state is just a list of wall (x,y) *)

  type item =
    | Pellet
    | PowerPellet
    | Cherry

  type tile =
    | Wall
    | Item of item
    | Empty

  let is_wall walls x y = List.mem (x, y) walls

  (* Required interface values *)
  let item_at _ _ _ = None
  let eat_item m _ _ = m
  let items_exist _ = false
  let pellet_at _ _ _ = false
  let eat_pellet m _ _ = m
  let pellets_exist _ = false
  let is_power_pellet _ _ _ = false
  let create_for_tests _ = []
end

(* A ghost where we can manually set the "next_position" logic *)
module TargetGhost = struct
  type t = {
    x : int;
    y : int;
    target_x : int;
    target_y : int;
  }

  let create x y _ = { x; y; target_x = x; target_y = y }
  let create_with_target x y tx ty = { x; y; target_x = tx; target_y = ty }
  let position g = (g.x, g.y)

  (* Force the ghost to want to go to our specific target, regardless of
     Pacman *)
  let next_position g ~pac_pos:_ = (g.target_x, g.target_y)
  let move_to g nx ny = { g with x = nx; y = ny }

  (* Dummy ghost state values for testing *)
  let is_frightened _ = false
  let is_eaten _ = false
  let color _ = Raylib.Color.red
  let respawn g = g
  let set_frightened g _ = g
  let set_eaten g _ = g
  let is_at_home _ = false
  let update_duration g ~time:_ = g
  let speed_factor _ = 1.0
end

module MovementTester =
  Paclib.Movement.Make (ConfigurableMaze) (StubPacman) (TargetGhost)
    (StubConstants)

(* Tests checking for direct movement of Pacman *)
let test_ghost_fallback_on_wall _ =
  let walls = [ (11, 10); (10, 9); (9, 10) ] in
  (* FIX: Use the custom creator here *)
  let ghost = TargetGhost.create_with_target 10 10 11 10 in
  let ghost' = MovementTester.move_ghost walls ghost (0, 0) in
  let gx, gy = TargetGhost.position ghost' in
  assert_equal ~printer:string_of_pos (10, 11) (gx, gy)

let test_ghost_stuck_if_surrounded _ =
  let walls = [ (11, 10); (9, 10); (10, 11); (10, 9) ] in
  let ghost = TargetGhost.create_with_target 10 10 11 10 in
  let ghost' = MovementTester.move_ghost walls ghost (0, 0) in
  assert_equal ~printer:string_of_pos (10, 10) (TargetGhost.position ghost')

let test_ghost_random_fallback _ =
  let walls = [ (11, 10); (9, 10) ] in
  let ghost = TargetGhost.create_with_target 10 10 11 10 in
  let ghost' = MovementTester.move_ghost walls ghost (0, 0) in
  let gx, gy = TargetGhost.position ghost' in
  let valid = [ (10, 11); (10, 9) ] in
  assert_bool "Ghost did not move to a valid fallback" (List.mem (gx, gy) valid)

(* Module stub used to check ghosts update their state when frightened or
   eaten *)
module StatefulGhost : GHOST = struct
  type t = {
    x : int;
    y : int;
    frightened : bool;
    eaten : bool;
    home : bool;
  }

  let create x y _ = { x; y; frightened = false; eaten = false; home = false }
  let position g = (g.x, g.y)
  let next_position g ~pac_pos:_ = (g.x, g.y)
  let move_to g nx ny = { g with x = nx; y = ny }
  let is_frightened g = g.frightened
  let is_eaten g = g.eaten
  let is_at_home g = g.home
  let set_frightened g b = { g with frightened = b }
  let set_eaten g b = { g with eaten = b }
  let respawn g = { g with frightened = false; eaten = false; home = true }
  let update_duration g ~time:_ = g
  let speed_factor _ = 1.0
  let color _ = Raylib.Color.red
end

(* Module stub used to have a maze that always has a power pellet at (5,5) *)
module PowerPelletMaze = struct
  include StubMaze

  let item_at _ x y = if x = 5 && y = 5 then Some PowerPellet else None
end

(* Module stub used to have a cherry at (5,5) *)
module CherryMaze = struct
  include StubMaze

  let item_at _ x y = if x = 5 && y = 5 then Some Cherry else None
end

(* Engine modules using the above module stubs *)
module EnginePower =
  Paclib.Game_engine.Make (PowerPelletMaze) (StubPacman) (StatefulGhost)
    (StubConstants)

module EngineCherry =
  Paclib.Game_engine.Make (CherryMaze) (StubPacman) (StatefulGhost)
    (StubConstants)

(* Test speedup timer *)
let test_speedup_timer_trigger _ =
  let w = Engine.start (mk_world ()) in

  (* 8.5 seconds * 60 fps = 510 frames. Set frames_alive to 509 so next tick
     hits 510 *)
  let target_frame = int_of_float (8.5 *. 60.0) in
  let w = { w with frames_alive = target_frame - 1; speedup_timer = 0 } in
  let w' = Engine.update_world w in
  assert_equal ~printer:string_of_int 60 w'.speedup_timer;
  let w'' = Engine.update_world w' in
  assert_equal ~printer:string_of_int 59 w''.speedup_timer

(* Test to check power pellet is eaten *)
let test_eat_power_pellet _ =
  let pac = StubPacman.create 4 5 in
  let ghost = StatefulGhost.create 10 10 Paclib.Ai.defaulty in
  let w = EnginePower.initial_world () pac [ ghost ] |> EnginePower.start in
  let w' = EnginePower.update_world w in

  assert_equal ~printer:string_of_game_state PowerUp w'.state;
  assert_equal ~printer:string_of_int StubConstants.power_pellet_score w'.score;
  assert_equal ~printer:string_of_int StubConstants.power_pellet_duration_frames
    w'.powerup_timer;
  assert_bool "Ghost should be frightened"
    (StatefulGhost.is_frightened (List.hd w'.ghosts))

(* Test to check cherry is eaten *)
let test_eat_cherry _ =
  let pac = StubPacman.create 4 5 in
  let ghost = StatefulGhost.create 10 10 Paclib.Ai.defaulty in
  let w = EngineCherry.initial_world () pac [ ghost ] |> EngineCherry.start in
  let w' = EngineCherry.update_world w in

  assert_equal ~printer:string_of_game_state Playing w'.state;
  assert_equal ~printer:string_of_int StubConstants.cherry_score w'.score

(* Test Suite *)
let suite =
  "game_engine tests"
  >::: [
         (* Basic Tests *)
         "intro no update" >:: test_intro_no_update;
         "start enters playing" >:: test_start_enters_playing;
         "intro stable" >:: test_intro_stable;
         "gameover stable" >:: test_gameover_stable;
         (* Pac-Man Movement Tests*)
         "pac moves" >:: test_pacman_moves;
         "wall blocks pacman" >:: test_wall_blocks_pacman;
         (* Ghost Schedule Tests *)
         "ghost moves threshold"
         >:: test_ghost_moves_when_accumulator_reaches_threshold;
         "ghost moves every 2 frames" >:: test_ghost_moves_every_second_frame;
         (* Change in Speed Tests *)
         "slow ghost every 4 frames" >:: test_slow_ghost_moves_every_four_frames;
         "fast ghost every frame" >:: test_fast_ghost_moves_every_frame;
         "fractional accumulator carry-over" >:: test_accumulator_carry_over;
         (* Cooldown Tests *)
         "pac frozen cooldown" >:: test_pacman_frozen_when_cooldown;
         "pac moves after cooldown" >:: test_pacman_moves_after_cooldown_expires;
         "ghosts frozen cooldown" >:: test_ghosts_frozen_when_cooldown;
         (* Eating Pellets Tests *)
         "pac eats pellet" >:: test_pacman_eats_pellet;
         (* Level Control Tests *)
         "level complete" >:: test_level_complete;
         (* Death logic Tests *)
         "pac dead" >:: test_pac_dead_transition;
         "game over when no lives" >:: test_game_over_when_no_lives_left;
         "pacdead timer counts" >:: test_pacdead_timer_counts_down;
         "pac frozen during pacdead" >:: test_pac_is_frozen_during_pacdead;
         "ghosts frozen during pacdead" >:: test_ghosts_frozen_during_pacdead;
         "respawn after timer" >:: test_respawn_after_pacdead_timer;
         "ghost fallback on wall" >:: test_ghost_fallback_on_wall;
         "ghost stuck surrounded" >:: test_ghost_stuck_if_surrounded;
         "ghost random fallback" >:: test_ghost_random_fallback;
         "speedup timer triggers" >:: test_speedup_timer_trigger;
         "eat power pellet" >:: test_eat_power_pellet;
         "eat cherry" >:: test_eat_cherry;
       ]

let _ = run_test_tt_main suite
