open OUnit2
open Paclib.Game_engine_interface
open Paclib.Game_state

(* ------------------------------------------------------------- *)
(*  STUB MODULES                                                 *)
(* ------------------------------------------------------------- *)

module StubMaze = struct
  type t = unit

  let is_wall _ _ _ = false
  let pellet_at _ _ _ = false
  let eat_pellet m _ _ = m
  let pellets_remaining _ = 1
  let width _ = 40
  let height _ = 30
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

  let create x y = { x; y }
  let position g = (g.x, g.y)
  let next_position g = (g.x, g.y)
  let move_to g _ _ = g
end

module StubConstants = struct
  let pacman_start_pos = (5, 5)
  let ghost_start_positions = [ (10, 10) ]
  let starting_lives = 3
  let pellet_score = 10
end

module Engine =
  Paclib.Game_engine.Make (StubMaze) (StubPacman) (StubGhost) (StubConstants)

(* Convenience world builder *)
let mk_world () =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 10 10 in
  Engine.initial_world () pac [ ghost ]

(* ------------------------------------------------------------- *)
(*  BASIC TESTS                                                  *)
(* ------------------------------------------------------------- *)

let test_intro_no_update _ =
  let w = mk_world () in
  let w' = Engine.update_world w in
  assert_equal w w'

let test_start_enters_playing _ =
  let w = mk_world () in
  let started = Engine.start w in
  assert_equal Playing started.state

let test_playing_no_pellets_no_collision _ =
  let w = Engine.start (mk_world ()) in
  let w' = Engine.update_world w in
  assert_equal Playing w'.state;
  assert_equal w.score w'.score

let test_pac_dead_transition _ =
  let maze = () in
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 5 5 in
  let w = Engine.initial_world maze pac [ ghost ] |> Engine.start in
  let w' = Engine.update_world w in
  assert_equal PacDead w'.state

let test_game_over_when_no_lives_left _ =
  let w = mk_world () in
  let w = { w with state = PacDead; lives = 1 } in
  let w' = Engine.update_world w in
  assert_equal GameOver w'.state

(* ------------------------------------------------------------- *)
(*  ADDITIONAL TESTS                                             *)
(* ------------------------------------------------------------- *)

let test_pacman_moves _ =
  let w = Engine.start (mk_world ()) in
  let w' = Engine.update_world w in
  assert_equal (6, 5) (StubPacman.position w'.pac)

module WallMaze = struct
  type t = unit

  let is_wall _ x y = (x, y) = (6, 5)
  let pellet_at _ _ _ = false
  let eat_pellet m _ _ = m
  let pellets_remaining _ = 10
  let width _ = 40
  let height _ = 30
end

module EngineWall =
  Paclib.Game_engine.Make (WallMaze) (StubPacman) (StubGhost) (StubConstants)

let test_wall_blocks_pacman _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 10 10 in
  let w = EngineWall.initial_world () pac [ ghost ] |> EngineWall.start in
  let w' = EngineWall.update_world w in
  assert_equal (5, 5) (StubPacman.position w'.pac)

(* Pellets ------------------------------------------------------ *)

module PelletMaze = struct
  type t = unit

  let is_wall _ _ _ = false
  let pellet_at _ x y = (x, y) = (6, 5)
  let eat_pellet m _ _ = m
  let pellets_remaining _ = 10
  let width _ = 40
  let height _ = 30
end

module EnginePellet =
  Paclib.Game_engine.Make (PelletMaze) (StubPacman) (StubGhost) (StubConstants)

let test_pacman_eats_pellet _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 10 10 in
  let w = EnginePellet.initial_world () pac [ ghost ] |> EnginePellet.start in
  let w' = EnginePellet.update_world w in
  assert_equal StubConstants.pellet_score w'.score

(* Level Complete ------------------------------------------------ *)

module EmptyMaze = struct
  type t = unit

  let is_wall _ _ _ = false
  let pellet_at _ _ _ = false
  let eat_pellet m _ _ = m
  let pellets_remaining _ = 0
  let width _ = 40
  let height _ = 30
end

module EngineEmpty =
  Paclib.Game_engine.Make (EmptyMaze) (StubPacman) (StubGhost) (StubConstants)

let test_level_complete _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 10 10 in
  let w = EngineEmpty.initial_world () pac [ ghost ] |> EngineEmpty.start in
  let w' = EngineEmpty.update_world w in
  assert_equal LevelComplete w'.state

(* Ghost moves into Pac-Man ------------------------------------- *)

module MovingGhost = struct
  type t = {
    x : int;
    y : int;
  }

  let create x y = { x; y }
  let position g = (g.x, g.y)
  let next_position _ = (5, 5)
  let move_to g nx ny = { x = nx; y = ny }
end

module EngineMG =
  Paclib.Game_engine.Make (StubMaze) (StubPacman) (MovingGhost) (StubConstants)

let test_pac_dead_after_movement _ =
  let pac = StubPacman.create 4 5 in
  let ghost = MovingGhost.create 5 6 in
  let w = EngineMG.initial_world () pac [ ghost ] |> EngineMG.start in
  let w' = EngineMG.update_world w in
  assert_equal PacDead w'.state

(* Intro/GameOver stable ---------------------------------------- *)

let test_intro_stable _ =
  let w = mk_world () in
  let w' = Engine.update_world w in
  assert_equal w w'

let test_gameover_stable _ =
  let w = mk_world () in
  let w = { w with state = GameOver } in
  let w' = Engine.update_world w in
  assert_equal w w'

(* ------------------------------------------------------------- *)
(*  SUITE                                                        *)
(* ------------------------------------------------------------- *)

let suite =
  "game_engine tests"
  >::: [
         "intro no update" >:: test_intro_no_update;
         "start enters playing" >:: test_start_enters_playing;
         "playing no pellets" >:: test_playing_no_pellets_no_collision;
         "pac dead transition" >:: test_pac_dead_transition;
         "game over" >:: test_game_over_when_no_lives_left;
         "pac moves" >:: test_pacman_moves;
         "wall blocks pacman" >:: test_wall_blocks_pacman;
         "pac eats pellet" >:: test_pacman_eats_pellet;
         "level complete" >:: test_level_complete;
         "death after movement" >:: test_pac_dead_after_movement;
         "intro stable" >:: test_intro_stable;
         "gameover stable" >:: test_gameover_stable;
       ]

let _ = run_test_tt_main suite
