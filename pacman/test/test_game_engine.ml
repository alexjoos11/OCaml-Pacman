
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
  let pellets_exist _ = true
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
  let next_position g ~pac_pos:_ = (g.x, g.y) (* frozen ghost behavior *)
  let move_to g nx ny = { x = nx; y = ny }
end

module StubConstants = struct
  let pacman_start_pos = (5, 5)
  let ghost_start_positions = [ (10, 10) ]
  let starting_lives = 3
  let pellet_score = 10
  let pacdead_pause_frames = 50
  let movement_delay = 5
  let ghost_move_cooldown = 12
end

module Engine =
  Paclib.Game_engine.Make (StubMaze) (StubPacman) (StubGhost) (StubConstants)

(* ------------------------------------------------------------- *)
(*  WORLD HELPER                                                 *)
(* ------------------------------------------------------------- *)

let mk_world () =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 10 10 in
  Engine.initial_world () pac [ ghost ]

(* ------------------------------------------------------------- *)
(*  BASIC / INTRO / GAMEOVER                                     *)
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
(*  MOVEMENT                                                      *)
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
  let pellets_exist _ = true
end

module EngineWall =
  Paclib.Game_engine.Make (WallMaze) (StubPacman) (StubGhost) (StubConstants)

let test_wall_blocks_pacman _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 10 10 in
  let w = EngineWall.initial_world () pac [ ghost ] |> EngineWall.start in
  let w' = EngineWall.update_world w in
  assert_equal (5, 5) (StubPacman.position w'.pac)

(* ------------------------------------------------------------- *)
(*  MOVEMENT COOL-DOWN                                           *)
(* ------------------------------------------------------------- *)

(* Pac-Man should stay frozen when cooldown > 0 *)
let test_pacman_frozen_when_cooldown _ =
  let w = Engine.start (mk_world ()) in
  let w = { w with move_cooldown = 3 } in
  let w' = Engine.update_world w in

  (* Pac-Man must NOT move *)
  assert_equal (StubPacman.position w.pac) (StubPacman.position w'.pac);
  (* Cooldown decreases *)
  assert_equal 2 w'.move_cooldown

(* When cooldown hits 0, movement resumes *)
let test_pacman_moves_after_cooldown_expires _ =
  let w = Engine.start (mk_world ()) in
  let w = { w with move_cooldown = 1 } in
  let w1 = Engine.update_world w in

  (* After update: cooldown hits 0, still frozen this frame *)
  assert_equal (StubPacman.position w.pac) (StubPacman.position w1.pac);
  assert_equal 0 w1.move_cooldown;

  (* Next frame: movement should happen *)
  let w2 = Engine.update_world w1 in
  assert_equal (6, 5) (StubPacman.position w2.pac)

(* Ghosts should also not move during cooldown *)
let test_ghosts_frozen_when_cooldown _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 3 3 in
  let w = Engine.initial_world () pac [ ghost ] |> Engine.start in
  let w = { w with move_cooldown = 2 } in
  let w' = Engine.update_world w in

  assert_equal (3, 3) (StubGhost.position (List.hd w'.ghosts));
  assert_equal 1 w'.move_cooldown

(* ------------------------------------------------------------- *)
(*  PELLETS                                                      *)
(* ------------------------------------------------------------- *)

module PelletMaze = struct
  type t = unit

  let is_wall _ _ _ = false
  let pellet_at _ x y = (x, y) = (6, 5)
  let eat_pellet m _ _ = m
  let pellets_exist _ = true
end

module EnginePellet =
  Paclib.Game_engine.Make (PelletMaze) (StubPacman) (StubGhost) (StubConstants)

let test_pacman_eats_pellet _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 10 10 in
  let w = EnginePellet.initial_world () pac [ ghost ] |> EnginePellet.start in
  let w' = EnginePellet.update_world w in
  assert_equal StubConstants.pellet_score w'.score

(* ------------------------------------------------------------- *)
(*  LEVEL COMPLETE                                               *)
(* ------------------------------------------------------------- *)

module EmptyMaze = struct
  type t = unit

  let is_wall _ _ _ = false
  let pellet_at _ _ _ = false
  let eat_pellet m _ _ = m
  let pellets_exist _ = false
end

module EngineEmpty =
  Paclib.Game_engine.Make (EmptyMaze) (StubPacman) (StubGhost) (StubConstants)

let test_level_complete _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 10 10 in
  let w = EngineEmpty.initial_world () pac [ ghost ] |> EngineEmpty.start in
  let w' = EngineEmpty.update_world w in
  assert_equal LevelComplete w'.state

(* ------------------------------------------------------------- *)
(*  PAC-DEAD + TIMER                                             *)
(* ------------------------------------------------------------- *)

let test_pac_dead_transition _ =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 5 5 in
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
  let ghost = StubGhost.create 5 5 in
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
  let ghost = StubGhost.create 3 3 in
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
(*  SUITE                                                        *)
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
