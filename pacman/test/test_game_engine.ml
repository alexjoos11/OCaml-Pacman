open OUnit2
open Paclib.Game_engine_interface

(* ------------------------------------------------------------- *)
(*  STUB MODULES                                                 *)
(* ------------------------------------------------------------- *)

module StubMaze = struct
  type t = unit

  let is_wall _ _ _ = false
  let pellet_at _ _ _ = false
  let eat_pellet m _ _ = m
  let pellets_remaining _ = 1
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

  (* Ghost never moves in stub *)
  let next_position g = (g.x, g.y)
  let move_to g nx ny = g
end

module StubConstants : CONSTANTS = struct
  let pacman_start_pos = (5, 5)
  let ghost_start_positions = [ (10, 10) ]
  let starting_lives = 3
  let pellet_score = 10
end

(* Instantiate the engine functor *)
module Engine =
  Paclib.Game_engine.Make (StubMaze) (StubPacman) (StubGhost) (StubConstants)

let make_world () =
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 10 10 in
  Engine.initial_world () pac [ ghost ]

(* ------------------------------------------------------------- *)
(*  TESTS                                                        *)
(* ------------------------------------------------------------- *)

let test_intro_no_update _ =
  let w = make_world () in
  let w' = Engine.update_world w in
  assert_equal w w'

let test_playing_no_pellets_no_collision _ =
  let w = { (make_world ()) with state = Playing } in
  let w' = Engine.update_world w in
  assert_equal Engine.Playing w'.state;
  assert_equal w.score w'.score

let test_pac_dead_transition _ =
  let maze = () in
  let pac = StubPacman.create 5 5 in
  let ghost = StubGhost.create 5 5 in
  let base = Engine.initial_world maze pac [ ghost ] in
  let w = { base with state = Playing } in
  let w' = Engine.update_world w in
  assert_equal Engine.PacDead w'.state

let test_game_over_when_no_lives_left _ =
  let w = { (make_world ()) with state = PacDead; lives = 1 } in
  let w' = Engine.update_world w in
  assert_equal Engine.GameOver w'.state

(* ------------------------------------------------------------- *)
(*  Test suite                                                   *)
(* ------------------------------------------------------------- *)

let suite =
  "game_engine test suite"
  >::: [
         "intro no update" >:: test_intro_no_update;
         "playing no pellets no collision"
         >:: test_playing_no_pellets_no_collision;
         "pac dead transition" >:: test_pac_dead_transition;
         "game over when no lives left" >:: test_game_over_when_no_lives_left;
       ]

let _ = run_test_tt_main suite
