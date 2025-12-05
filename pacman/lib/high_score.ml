let file = "high_score.dat"
(* The file that stores the highest score. *)

let load_score () =
  try
    In_channel.with_open_text file (fun input ->
        match In_channel.input_line input with
        | Some line -> (
            match int_of_string_opt line with
            | Some score -> score
            | None -> 0)
        | None -> 0)
  with Sys_error _ -> 0

let save_score score =
  try
    Out_channel.with_open_text file (fun output ->
        Out_channel.output_string output (string_of_int score ^ "\n"))
  with Sys_error message ->
    print_endline ("Error occurred while saving new high score: " ^ message)
