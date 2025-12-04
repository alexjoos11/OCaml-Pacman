open OUnit2
open Paclib.Game_engine_interface
open Paclib.Game_state
open Paclib.Maze
open Paclib.Ai

(* ------------------------------------------------------------- *)
(* STUB MODULES                                                 *)
(* ------------------------------------------------------------- *)

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
  let next_position g ~pac_pos:_ = (g.x, g.y) (* frozen ghost behavior *)
  let move_to g nx ny = { x = nx; y = ny }
  let is_frightened _ = false
  let is_eaten _ = false
  let color _ = Raylib.Color.red
  let respawn g = g
  let set_frightened g _ = g
  let set_eaten g _ = g
  let is_at_home _ = false

  type speed =
    | Fast
    | Regular
    | Slow
    | Paused

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

(* ------------------------------------------------------------- *)
(* EXTRA STUBS FOR GHOST MOVEMENT SCHEDULER TESTS               *)
(* ------------------------------------------------------------- *)

module MovingGhost : GHOST = struct
  type t = {
    x : int;
    y : int;
  }

  type speed =
    | Fast
    | Regular
    | Slow
    | Paused

  let create x y _ = { x; y }
  let position g = (g.x, g.y)

  (* Always wants to move one tile to the right when allowed *)
  let next_position g ~pac_pos:_ = (g.x + 1, g.y)
  let move_to _ nx ny = { x = nx; y = ny }

  (* Ignore duration for these tests *)
  let update_duration g ~time:_ = g

  (* 1.0 “speed point” per frame *)
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
  let ghost_move_cooldown = 2 (* threshold for scheduler *)
  let power_pellet_score = 50
  let fps = 60
  let power_pellet_duration_frames = 600
  let cherry_score = 100
  let ghost_eaten_score = 200
end

module EngineGhostMove =
  Paclib.Game_engine.Make (OpenMaze) (StubPacman) (MovingGhost)
    (GhostMoveConstants)

(* ------------------------------------------------------------- *)
(* WORLD HELPER                                                 *)
(* ------------------------------------------------------------- *)

let mk_world () =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 10 10 Paclib.Ai.defaulty in
  Engine.initial_world () pac [ ghost ]

(* ------------------------------------------------------------- *)
(* BASIC / INTRO / GAMEOVER                                     *)
(* ------------------------------------------------------------- *)

let test_intro_no_update _ =
  let w = mk_world () in
  assert_equal w (Engine.update_world w)

let test_start_enters_playing _ =
  let w = mk_world () in
  let started = Engine.start w in
  assert_equal Playing started.state

let test_intro_stable _ =
  let w = mk_world () in
  assert_equal w (Engine.update_world w)

let test_gameover_stable _ =
  let w = { (mk_world ()) with state = GameOver } in
  assert_equal w (Engine.update_world w)

(* ------------------------------------------------------------- *)
(* MOVEMENT                                                      *)
(* ------------------------------------------------------------- *)

let test_pacman_moves _ =
  let w = Engine.start (mk_world ()) in
  let w' = Engine.update_world w in
  assert_equal (6, 5) (StubPacman.position w'.pac)

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
  let w' = EngineWall.update_world w in
  assert_equal (5, 5) (StubPacman.position w'.pac)

(* ------------------------------------------------------------- *)
(* GHOST MOVEMENT SCHEDULER                                     *)
(* ------------------------------------------------------------- *)

let test_ghost_moves_when_accumulator_reaches_threshold _ =
  let pac = StubPacman.create 1 1 in
  let ghost = MovingGhost.create 0 5 Paclib.Ai.defaulty in
  let w =
    EngineGhostMove.initial_world () pac [ ghost ] |> EngineGhostMove.start
  in

  (* Frame 1: accumulator = 1.0 < 2.0 → no move yet *)
  let w1 = EngineGhostMove.update_world w in
  assert_equal (0, 5) (MovingGhost.position (List.hd w1.ghosts));

  (* Frame 2: accumulator = 2.0 → ghost should move right by 1 *)
  let w2 = EngineGhostMove.update_world w1 in
  assert_equal (1, 5) (MovingGhost.position (List.hd w2.ghosts))

let test_ghost_moves_every_second_frame _ =
  let pac = StubPacman.create 1 1 in
  let ghost = MovingGhost.create 0 5 Paclib.Ai.defaulty in
  let w =
    EngineGhostMove.initial_world () pac [ ghost ] |> EngineGhostMove.start
  in

  (* Frame 1: no move *)
  let w1 = EngineGhostMove.update_world w in
  assert_equal (0, 5) (MovingGhost.position (List.hd w1.ghosts));

  (* Frame 2: move -> (1,5) *)
  let w2 = EngineGhostMove.update_world w1 in
  assert_equal (1, 5) (MovingGhost.position (List.hd w2.ghosts));

  (* Frame 3: no move -> still (1,5) *)
  let w3 = EngineGhostMove.update_world w2 in
  assert_equal (1, 5) (MovingGhost.position (List.hd w3.ghosts));

  (* Frame 4: move -> (2,5) *)
  let w4 = EngineGhostMove.update_world w3 in
  assert_equal (2, 5) (MovingGhost.position (List.hd w4.ghosts))

(* ------------------------------------------------------------- *)
(* MOVEMENT COOL-DOWN                                           *)
(* ------------------------------------------------------------- *)

let test_pacman_frozen_when_cooldown _ =
  let w = Engine.start (mk_world ()) in
  let w = { w with move_cooldown = 3 } in
  let w' = Engine.update_world w in

  assert_equal (StubPacman.position w.pac) (StubPacman.position w'.pac);
  assert_equal 2 w'.move_cooldown

let test_pacman_moves_after_cooldown_expires _ =
  let w = Engine.start (mk_world ()) in
  let w = { w with move_cooldown = 1 } in
  let w1 = Engine.update_world w in

  assert_equal (StubPacman.position w.pac) (StubPacman.position w1.pac);
  assert_equal 0 w1.move_cooldown;

  let w2 = Engine.update_world w1 in
  assert_equal (6, 5) (StubPacman.position w2.pac)

let test_ghosts_frozen_when_cooldown _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 3 3 Paclib.Ai.defaulty in
  let w = Engine.initial_world () pac [ ghost ] |> Engine.start in
  let w = { w with move_cooldown = 2 } in
  let w' = Engine.update_world w in

  assert_equal (3, 3) (StubGhost.position (List.hd w'.ghosts));
  assert_equal 1 w'.move_cooldown

(* ------------------------------------------------------------- *)
(* PELLETS                                                      *)
(* ------------------------------------------------------------- *)

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
  assert_equal StubConstants.pellet_score w'.score

(* ------------------------------------------------------------- *)
(* LEVEL COMPLETE                                               *)
(* ------------------------------------------------------------- *)

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
  let pac = StubPacman.create 6 6 in
  let ghost = StubGhost.create 1 1 Paclib.Ai.defaulty in
  let maze = EmptyMaze.create_for_tests () in
  let w = EngineEmpty.initial_world maze pac [ ghost ] |> EngineEmpty.start in
  let w' = EngineEmpty.update_world w in
  assert_equal LevelComplete w'.state

(* ------------------------------------------------------------- *)
(* PAC-DEAD + TIMER                                             *)
(* ------------------------------------------------------------- *)

let test_pac_dead_transition _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 5 5 Paclib.Ai.defaulty in
  let w = Engine.initial_world () pac [ ghost ] |> Engine.start in
  let w' = Engine.update_world w in
  assert_equal PacDead w'.state

let test_game_over_when_no_lives_left _ =
  let w = mk_world () in
  let w = { w with state = PacDead; lives = 1 } in
  let w' = Engine.update_world w in
  assert_equal GameOver w'.state

let test_pacdead_timer_counts_down _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 5 5 Paclib.Ai.defaulty in
  let w =
    Engine.initial_world () pac [ ghost ] |> Engine.start |> Engine.update_world
  in
  assert_equal PacDead w.state;
  assert_equal StubConstants.pacdead_pause_frames w.pacdead_timer;

  let w2 = Engine.update_world w in
  assert_equal (StubConstants.pacdead_pause_frames - 1) w2.pacdead_timer

let test_pac_is_frozen_during_pacdead _ =
  let w = { (mk_world ()) with state = PacDead; pacdead_timer = 10 } in
  let w' = Engine.update_world w in
  assert_equal (StubPacman.position w.pac) (StubPacman.position w'.pac);
  assert_equal 9 w'.pacdead_timer

let test_ghosts_frozen_during_pacdead _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 3 3 Paclib.Ai.defaulty in
  let w =
    {
      (Engine.initial_world () pac [ ghost ]) with
      state = PacDead;
      pacdead_timer = 5;
    }
  in
  let w' = Engine.update_world w in
  assert_equal (3, 3) (StubGhost.position (List.hd w'.ghosts))

let test_respawn_after_pacdead_timer _ =
  let w =
    { (mk_world () |> Engine.start) with state = PacDead; pacdead_timer = 0 }
  in
  let w' = Engine.update_world w in

  assert_equal Playing w'.state;
  assert_equal (StubConstants.starting_lives - 1) w'.lives;
  assert_equal StubConstants.pacman_start_pos (StubPacman.position w'.pac)

(* ------------------------------------------------------------- *)
(* SUITE                                                        *)
(* ------------------------------------------------------------- *)

let suite =
  "game_engine tests"
  >::: [
         (* Basic *)
         "intro no update" >:: test_intro_no_update;
         "start enters playing" >:: test_start_enters_playing;
         "intro stable" >:: test_intro_stable;
         "gameover stable" >:: test_gameover_stable;
         (* Movement *)
         "pac moves" >:: test_pacman_moves;
         "wall blocks pacman" >:: test_wall_blocks_pacman;
         "ghost moves when accumulator reaches threshold"
         >:: test_ghost_moves_when_accumulator_reaches_threshold;
         "ghost moves every second frame"
         >:: test_ghost_moves_every_second_frame;
         (* Cooldown *)
         "pac frozen when cooldown > 0" >:: test_pacman_frozen_when_cooldown;
         "pac moves when cooldown expires"
         >:: test_pacman_moves_after_cooldown_expires;
         "ghosts frozen when cooldown > 0" >:: test_ghosts_frozen_when_cooldown;
         (* Pellets *)
         "pac eats pellet" >:: test_pacman_eats_pellet;
         (* Level control *)
         "level complete" >:: test_level_complete;
         (* Death logic *)
         "pac dead immediate" >:: test_pac_dead_transition;
         "pacdead timer counts down" >:: test_pacdead_timer_counts_down;
         "pac frozen during pacdead" >:: test_pac_is_frozen_during_pacdead;
         "ghosts frozen during pacdead" >:: test_ghosts_frozen_during_pacdead;
         "respawn after timer" >:: test_respawn_after_pacdead_timer;
         "game over when no lives" >:: test_game_over_when_no_lives_left;
       ]

let _ = run_test_tt_main suite
