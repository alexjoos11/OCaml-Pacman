open OUnit2
module G = Paclib.Ghost

let assert_float_equal ~msg expected actual =
  let eps = 1e-6 in
  assert_bool msg (abs_float (expected -. actual) < eps)

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
(*  Duration / mode timer                                       *)
(* ------------------------------------------------------------- *)

let test_update_duration_decrements_timer _ =
  let g = G.create 0 0 in
  (* initial timer = 5.0 from create *)
  let g' = G.update_duration g ~time:1.0 in
  assert_equal G.Regular (G.get_speed g');
  assert_float_equal ~msg:"timer should decrement by time" 4.0 (G.get_time g')

let test_update_duration_switches_mode_on_expiry_exact _ =
  let g = G.create 0 0 in
  (* new_timer = 5.0 - 5.0 = 0.0 → switch_mode *)
  let g' = G.update_duration g ~time:5.0 in
  (* next_mode Regular = Fast *)
  assert_equal G.Fast (G.get_speed g');
  (* timer is reset to 5.0 in switch_mode *)
  assert_float_equal ~msg:"timer should reset to 5.0 after mode switch" 5.0
    (G.get_time g')

let test_update_duration_switches_mode_on_expiry_overflow _ =
  let g = G.create 0 0 in
  (* new_timer = 5.0 - 6.0 = -1.0 → still switches mode, resets timer *)
  let g' = G.update_duration g ~time:6.0 in
  assert_equal G.Fast (G.get_speed g');
  assert_float_equal ~msg:"timer should reset to 5.0 even if time overshoots"
    5.0 (G.get_time g')

let test_update_duration_chains_modes _ =
  let g0 = G.create 0 0 in
  (* Regular -> Fast *)
  let g1 = G.update_duration g0 ~time:5.0 in
  (* Fast -> Slow *)
  let g2 = G.update_duration g1 ~time:5.0 in
  (* Slow -> Paused *)
  let g3 = G.update_duration g2 ~time:5.0 in
  (* Paused -> Regular *)
  let g4 = G.update_duration g3 ~time:5.0 in
  assert_equal G.Fast (G.get_speed g1);
  assert_equal G.Slow (G.get_speed g2);
  assert_equal G.Paused (G.get_speed g3);
  assert_equal G.Regular (G.get_speed g4)

(* ------------------------------------------------------------- *)
(*  speed_factor mapping                                        *)
(* ------------------------------------------------------------- *)

let test_speed_factor_by_mode _ =
  let base = G.create 0 0 in

  let g_fast = G.set_speed base G.Fast 5.0 in
  let g_regular = G.set_speed base G.Regular 5.0 in
  let g_slow = G.set_speed base G.Slow 5.0 in
  let g_paused = G.set_speed base G.Paused 5.0 in

  assert_float_equal ~msg:"Fast should map to factor 2.0" 2.0
    (G.speed_factor g_fast);

  assert_float_equal ~msg:"Regular should map to factor 1.0" 1.0
    (G.speed_factor g_regular);

  assert_float_equal ~msg:"Slow should map to factor 0.5" 0.5
    (G.speed_factor g_slow);

  assert_float_equal ~msg:"Paused should map to factor 0.0" 0.0
    (G.speed_factor g_paused)

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
         (* duration / mode timer *)
         "update_duration decrements timer"
         >:: test_update_duration_decrements_timer;
         "update_duration switches mode on expiry (exact)"
         >:: test_update_duration_switches_mode_on_expiry_exact;
         "update_duration switches mode on expiry (overflow)"
         >:: test_update_duration_switches_mode_on_expiry_overflow;
         "update_duration chains modes" >:: test_update_duration_chains_modes;
         (* speed_factor *)
         "speed_factor by mode" >:: test_speed_factor_by_mode;
       ]

let () = run_test_tt_main suite
