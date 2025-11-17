open Raylib

let () =
  init_window 800 600 "Raylib Test";

  while not (window_should_close ()) do
    begin_drawing ();
    clear_background Color.raywhite;
    draw_text "Hello World!" 350 280 20 Color.black;
    end_drawing ()
  done;

  close_window ();

