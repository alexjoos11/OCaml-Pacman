open OUnit2
open Paclib.Ai
module Color = Raylib.Color

(* Helper function to use printer for Color tests *)
let string_of_color c =
  Printf.sprintf "(r:%d, g:%d, b:%d, a:%d)" (Color.r c) (Color.g c) (Color.b c)
    (Color.a c)

(* Testing printer help *)
let assert_move msg expected f =
  assert_equal ~msg
    ~printer:(fun (x, y) -> Printf.sprintf "(%d,%d)" x y)
    expected f

(* Tests using defaulty (avoid exposing fns) *)
let test_default_ghosts _ =
  assert_move "defaulty.attack uses standard_attack" (1, 0)
    (defaulty.attack ~x:0 ~y:0 ~pac_pos:(5, 1));
  assert_move "orangefaulty.attack uses standard_attack" (1, 0)
    (orangefaulty.attack ~x:0 ~y:0 ~pac_pos:(5, 1));
  assert_move "cyanfaulty.attack uses standard_attack" (1, 0)
    (cyanfaulty.attack ~x:0 ~y:0 ~pac_pos:(5, 1));
  assert_move "pinkfaulty.attack uses standard_attack" (1, 0)
    (pinkfaulty.attack ~x:0 ~y:0 ~pac_pos:(5, 1))

let test_ai_colors _ =
  assert_equal ~msg:"defaulty should be red" ~printer:string_of_color Color.red
    defaulty.color;

  assert_equal ~msg:"orangefaulty should be orange" ~printer:string_of_color
    Color.orange orangefaulty.color;

  assert_equal ~msg:"pinkfaulty should be pink" ~printer:string_of_color
    Color.pink pinkfaulty.color

(* Tests to check attack functionality *)
let test_standard_attack_horizontal_right _ =
  assert_move "attack horizontal right" (1, 0)
    (defaulty.attack ~x:0 ~y:0 ~pac_pos:(5, 1))

let test_standard_attack_horizontal_left _ =
  assert_move "attack horizontal left" (-1, 0)
    (defaulty.attack ~x:0 ~y:0 ~pac_pos:(-4, 1))

let test_standard_attack_vertical_down _ =
  assert_move "attack vertical down" (0, 1)
    (defaulty.attack ~x:0 ~y:0 ~pac_pos:(0, 3))

let test_standard_attack_vertical_up _ =
  assert_move "attack vertical up" (0, -1)
    (defaulty.attack ~x:0 ~y:0 ~pac_pos:(0, -2))

let test_standard_attack_same_tile _ =
  assert_move "attack same tile" (3, 4)
    (defaulty.attack ~x:3 ~y:4 ~pac_pos:(3, 4))

(* Tests to check ghosts runaway behavior *)
let test_standard_runaway_horizontal_from_right _ =
  assert_move "runaway from right" (-1, 0)
    (defaulty.runaway ~x:0 ~y:0 ~pac_pos:(5, 1))

let test_standard_runaway_horizontal_from_left _ =
  assert_move "runaway from left" (1, 0)
    (defaulty.runaway ~x:0 ~y:0 ~pac_pos:(-4, 1))

let test_standard_runaway_vertical_up _ =
  assert_move "runaway up" (0, -1) (defaulty.runaway ~x:0 ~y:0 ~pac_pos:(0, 3))

let test_standard_runaway_vertical_down _ =
  assert_move "runaway down" (0, 1)
    (defaulty.runaway ~x:0 ~y:0 ~pac_pos:(0, -2))

let test_standard_runaway_same_tile _ =
  assert_move "runaway same tile" (3, 4)
    (defaulty.runaway ~x:3 ~y:4 ~pac_pos:(3, 4))

(* Tests to check the "go home" behavior of ghosts to go back to initial spot *)
let test_standard_go_home_horizontal _ =
  assert_move "go_home horizontal right" (1, 0)
    (defaulty.go_home ~x:0 ~y:0 ~home:(5, 1))

let test_standard_go_home_horizontal_left _ =
  assert_move "go_home horizontal left" (-1, 0)
    (defaulty.go_home ~x:0 ~y:0 ~home:(-4, 1))

let test_standard_go_home_vertical_down _ =
  assert_move "go_home vertical down" (0, 1)
    (defaulty.go_home ~x:0 ~y:0 ~home:(0, 3))

let test_standard_go_home_vertical_up _ =
  assert_move "go_home vertical up" (0, -1)
    (defaulty.go_home ~x:0 ~y:0 ~home:(0, -2))

let test_standard_go_home_same_tile _ =
  assert_move "go_home same tile" (3, 4)
    (defaulty.go_home ~x:3 ~y:4 ~home:(3, 4))

let suite =
  "ai tests"
  >::: [
         "default ghosts attack" >:: test_default_ghosts;
         "ai colors" >:: test_ai_colors;
         "standard attack horizontal right"
         >:: test_standard_attack_horizontal_right;
         "standard attack horizontal left"
         >:: test_standard_attack_horizontal_left;
         "standard attack vertical down" >:: test_standard_attack_vertical_down;
         "standard attack vertical up" >:: test_standard_attack_vertical_up;
         "standard attack same tile" >:: test_standard_attack_same_tile;
         "standard runaway horizontal from right"
         >:: test_standard_runaway_horizontal_from_right;
         "standard runaway horizontal from left"
         >:: test_standard_runaway_horizontal_from_left;
         "standard runaway vertical up" >:: test_standard_runaway_vertical_up;
         "standard runaway vertical down"
         >:: test_standard_runaway_vertical_down;
         "standard runaway same tile" >:: test_standard_runaway_same_tile;
         "standard go_home horizontal" >:: test_standard_go_home_horizontal;
         "standard go_home horizontal left"
         >:: test_standard_go_home_horizontal_left;
         "standard go_home vertical down"
         >:: test_standard_go_home_vertical_down;
         "standard go_home vertical up" >:: test_standard_go_home_vertical_up;
         "standard go_home same tile" >:: test_standard_go_home_same_tile;
       ]

let () = run_test_tt_main suite
