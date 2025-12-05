open OUnit2
open Paclib.High_score

let filename = "high_score.dat"
let remove_score_file () = if Sys.file_exists filename then Sys.remove filename

let with_clean_file test_fun _ctxt =
  remove_score_file ();
  try
    test_fun ();
    remove_score_file ()
  with e ->
    remove_score_file ();
    raise e

let test_load_no_file () = assert_equal 0 (load_score ())

let test_save_and_load () =
  let score = 500 in
  save_score score;
  assert_equal score (load_score ())

let test_overwrite_score () =
  save_score 100;
  Unix.sleepf 0.05;
  save_score 9999;
  assert_equal ~printer:string_of_int 9999 (load_score ())

let test_corrupt_file () =
  let output = open_out filename in
  output_string output "Not a number";
  close_out output;
  assert_equal 0 (load_score ())

let test_load_empty_file () =
  let output = open_out filename in
  close_out output;
  assert_equal 0 (load_score ())

let suite =
  "high_score tests"
  >::: [
         "load default (no file)" >:: with_clean_file test_load_no_file;
         "save and load" >:: with_clean_file test_save_and_load;
         "overwrite score" >:: with_clean_file test_overwrite_score;
         "handle corrupt file" >:: with_clean_file test_corrupt_file;
         "handle empty file" >:: with_clean_file test_load_empty_file;
       ]

let _ = run_test_tt_main suite
