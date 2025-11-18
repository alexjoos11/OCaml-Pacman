(* Commenting out so it doesn't conflict with the minimal ball design

   open Raylib

   let () = init_window 800 600 "Raylib Test";

   while not (window_should_close ()) do begin_drawing (); clear_background
   Color.raywhite; draw_text "Hello World!" 350 280 20 Color.black; end_drawing
   () done;

   close_window () *)

(*moving piece for MS2*)

open Tsdl

let window_width = 800
let window_height = 600
let ball_radius = 20
let move = 3
let ball_x = ref (window_width / 2)
let ball_y = ref (window_height / 2)

(*keeps ball in window*)
let bound v min_v max_v =
  if v < min_v then min_v else if v > max_v then max_v else v

(*creates circle*)
let draw_circle renderer cx cy r =
  let r2 = r * r in
  for dx = -r to r do
    for dy = -r to r do
      if (dx * dx) + (dy * dy) <= r2 then
        ignore (Sdl.render_draw_point renderer (cx + dx) (cy + dy))
    done
  done

let () =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> failwith e
  | Ok () -> (
      match
        Sdl.create_window "PACMAN" ~x:Sdl.Window.pos_centered
          ~y:Sdl.Window.pos_centered ~w:window_width ~h:window_height
          Sdl.Window.windowed
      with
      | Error (`Msg e) -> failwith e
      | Ok window -> (
          match
            Sdl.create_renderer ~index:(-1)
              ~flags:Sdl.Renderer.(accelerated + presentvsync)
              window
          with
          | Error (`Msg e) -> failwith e
          | Ok renderer ->
              let event = Sdl.Event.create () in
              let running = ref true in
              while !running do
                while Sdl.poll_event (Some event) do
                  match Sdl.Event.(enum (get event typ)) with
                  | `Quit -> running := false
                  | `Key_down -> (
                      let press =
                        Sdl.Event.get event Sdl.Event.keyboard_scancode
                      in
                      match Sdl.Scancode.enum press with
                      | `Up -> ball_y := !ball_y - move
                      | `Down -> ball_y := !ball_y + move
                      | `Right -> ball_x := !ball_x + move
                      | `Left -> ball_x := !ball_x - move
                      | _ -> ())
                  | _ -> ()
                done;

                Sdl.set_render_draw_color renderer 0 0 0 255 |> ignore;
                Sdl.render_clear renderer |> ignore;

                Sdl.set_render_draw_color renderer 255 255 0 255 |> ignore;
                draw_circle renderer !ball_x !ball_y ball_radius;

                Sdl.render_present renderer
              done))
