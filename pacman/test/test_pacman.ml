open OUnit2
module P = Paclib.Pacman

let test_create _ =
  let p = P.create 5 7 in
  assert_equal (5, 7) (P.position p)
    ~msg:"Pac-Man should start at the given position";
  (* Initial direction is Right *)
  let nx, ny = P.next_position p in
  assert_equal (6, 7) (nx, ny) ~msg:"Pac-Man should initially face Right"

let test_set_direction _ =
  let p = P.create 0 0 in
  let p2 = P.set_direction p P.Up in
  (* Position unchanged *)
  assert_equal (0, 0) (P.position p2);
  (* Direction changed *)
  let nx, ny = P.next_position p2 in
  assert_equal (0, -1) (nx, ny) ~msg:"Next position should match direction Up"

let test_next_position _ =
  let p = P.create 3 3 in
  let check dir expected =
    let p2 = P.set_direction p dir in
    assert_equal expected (P.next_position p2)
      ~msg:"next_position should follow the direction"
  in
  check P.Up (3, 2);
  check P.Down (3, 4);
  check P.Left (2, 3);
  check P.Right (4, 3)

let test_move_to _ =
  let p = P.create 5 5 in
  let p2 = P.move_to p 10 20 in
  assert_equal (10, 20) (P.position p2)
    ~msg:"move_to should set new coordinates";
  (* Direction should remain unchanged *)
  let nx, ny = P.next_position p2 in
  assert_equal (11, 20) (nx, ny)
    ~msg:"Direction should remain Right after move_to"

let suite =
  "pacman tests"
  >::: [
         "create" >:: test_create;
         "set_direction" >:: test_set_direction;
         "next_position" >:: test_next_position;
         "move_to" >:: test_move_to;
       ]

let () = run_test_tt_main suite
