open OUnit2
open Paclib.Ai
module Color = Raylib.Color

(* ------------------------------------------------------------- *)
(*  Helpers                                                      *)
(* ------------------------------------------------------------- *)

let assert_move msg expected f =
  assert_equal ~msg
    ~printer:(fun (x, y) -> Printf.sprintf "(%d,%d)" x y)
    expected f

(* ------------------------------------------------------------- *)
(*  Tests                                                        *)
(* ------------------------------------------------------------- *)

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
  assert_equal ~msg:"defaulty should be red" Color.red defaulty.color;

  assert_equal ~msg:"orangefaulty should be orange" Color.orange
    orangefaulty.color;

  assert_equal ~msg:"pinkfaulty should be pink" Color.pink pinkfaulty.color

let suite =
  "ai tests"
  >::: [
         "default ghosts attack" >:: test_default_ghosts;
         "ai colors" >:: test_ai_colors;
       ]

let () = run_test_tt_main suite
