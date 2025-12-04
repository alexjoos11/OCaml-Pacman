open OUnit2
module M = Paclib.Maze (* alias the Maze module properly *)

(* A tiny 3×3 maze for predictable testing *)
let m = M.create_for_tests "../data/test_tiny.txt"

let test_width_height _ =
  assert_equal 3 (M.width m);
  assert_equal 3 (M.height m)

let test_is_wall _ =
  assert_bool "0,0 should be wall" (M.is_wall m 0 0);
  assert_bool "1,0 should be empty/pellet" (not (M.is_wall m 1 0));
  assert_bool "outside bounds should be wall" (M.is_wall m 3 3)

let test_item_at _ =
  (* row 1, col 0 = '.' *)
  assert_equal (Some M.Pellet) (M.item_at m 0 1);
  (* row 0, col 0 = '#' *)
  assert_equal None (M.item_at m 0 0)

let test_eat_pellet _ =
  let m2 = M.eat_item m 0 1 in
  (* Pellet removed? *)
  assert_equal None (M.item_at m2 0 1);

  (* Original maze should be unchanged — functional update check *)
  assert_equal (Some M.Pellet) (M.item_at m 0 1)

let test_no_eat_when_no_pellet _ =
  let m2 = M.eat_item m 0 0 in
  (* No pellet: maze should be physically identical *)
  assert_equal m m2

let test_pellets_exist _ =
  assert_bool "pellets exist initially" (M.items_exist m);

  (* Eat all 4 pellets in the test maze *)
  let m1 = M.eat_item m 1 0 in
  let m2 = M.eat_item m1 0 1 in
  let m3 = M.eat_item m2 2 1 in
  let m4 = M.eat_item m3 1 2 in

  assert_bool "no pellets left" (not (M.items_exist m4))

let suite =
  "maze tests"
  >::: [
         "width/height" >:: test_width_height;
         "is_wall" >:: test_is_wall;
         "pellet_at" >:: test_item_at;
         "eat_pellet" >:: test_eat_pellet;
         "eat_no_pellet" >:: test_no_eat_when_no_pellet;
         "pellets_exist" >:: test_pellets_exist;
       ]

let () = run_test_tt_main suite
