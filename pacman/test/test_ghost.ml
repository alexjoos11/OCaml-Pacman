open OUnit2
module G = Paclib.Ghost

(* ------------------------------------------------------------- *)
(*  Tests                                                        *)
(* ------------------------------------------------------------- *)

let test_create_and_position _ =
  let g = G.create 3 4 in
  assert_equal (3, 4) (G.position g)

let test_move_to _ =
  let g = G.create 1 2 in
  let g2 = G.move_to g 5 7 in
  (* ensure new coords *)
  assert_equal (5, 7) (G.position g2);
  (* ensure immutability *)
  assert_equal (1, 2) (G.position g)

(* ---- next_position behavior ---- *)

let test_chase_horizontal_priority _ =
  (* dx > dy: should move horizontally toward Pac-Man *)
  let g = G.create 2 5 in
  let pac = (10, 6) in
  let nx, ny = G.next_position g ~pac_pos:pac in
  assert_equal (3, 5) (nx, ny)

let test_chase_horizontal_left _ =
  let g = G.create 8 5 in
  let pac = (3, 5) in
  let nx, ny = G.next_position g ~pac_pos:pac in
  assert_equal (7, 5) (nx, ny)

let test_chase_vertical_priority _ =
  (* dy > dx: should move vertically *)
  let g = G.create 5 2 in
  let pac = (6, 10) in
  let nx, ny = G.next_position g ~pac_pos:pac in
  assert_equal (5, 3) (nx, ny)

let test_chase_vertical_up _ =
  let g = G.create 5 9 in
  let pac = (5, 2) in
  let nx, ny = G.next_position g ~pac_pos:pac in
  assert_equal (5, 8) (nx, ny)

let test_chase_equal_dist_vertical_preferred _ =
  (* abs dx = abs dy → vertical movement chosen *)
  let g = G.create 5 5 in
  let pac = (7, 7) in
  (* dx = 2, dy = 2 *)
  let nx, ny = G.next_position g ~pac_pos:pac in
  assert_equal (5, 6) (nx, ny)

let test_chase_same_tile _ =
  (* Pac-Man and ghost overlap *)
  let g = G.create 4 4 in
  let pac = (4, 4) in
  let nx, ny = G.next_position g ~pac_pos:pac in
  assert_equal (4, 4) (nx, ny)

(* ------------------------------------------------------------- *)
(*  Suite                                                        *)
(* ------------------------------------------------------------- *)

let suite =
  "ghost tests"
  >::: [
         "create/position" >:: test_create_and_position;
         "move_to" >:: test_move_to;
         "horizontal priority" >:: test_chase_horizontal_priority;
         "horizontal left" >:: test_chase_horizontal_left;
         "vertical priority" >:: test_chase_vertical_priority;
         "vertical up" >:: test_chase_vertical_up;
         "equal dist vertical first"
         >:: test_chase_equal_dist_vertical_preferred;
         "same tile" >:: test_chase_same_tile;
       ]

let () = run_test_tt_main suite
