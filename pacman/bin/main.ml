open Tsdl
open Paclib

let window_width = 800
let window_height = 600
let ball_radius = 20
let move = 3

(* keeps pacman in window*)
let clamp v min_v max_v =
  if v < min_v then min_v else if v > max_v then max_v else v

let tile_size = 20
let max_tile_x = (window_width / tile_size) - 1
let max_tile_y = (window_height / tile_size) - 1

(*changes pacman location into pixels*)
let to_pixel (tx, ty) =
  ((tx * tile_size) + (tile_size / 2), (ty * tile_size) + (tile_size / 2))

(* draws pacman character*)
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
              (*create pacman*)
              let pac = ref (Pacman.create 10 10) in

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
                      (*reminder: when ghosts are implemented, add function to
                        send acman direction*)
                      | `Up ->
                          pac := Pacman.set_direction !pac Pacman.Up;
                          let nx, ny = Pacman.next_position !pac in
                          let nx = clamp nx 0 max_tile_x in
                          let ny = clamp ny 0 max_tile_y in
                          pac := Pacman.move_to !pac nx ny
                      | `Down ->
                          pac := Pacman.set_direction !pac Pacman.Down;
                          let nx, ny = Pacman.next_position !pac in
                          let nx = clamp nx 0 max_tile_x in
                          let ny = clamp ny 0 max_tile_y in
                          pac := Pacman.move_to !pac nx ny
                      | `Right ->
                          pac := Pacman.set_direction !pac Pacman.Right;
                          let nx, ny = Pacman.next_position !pac in
                          let nx = clamp nx 0 max_tile_x in
                          let ny = clamp ny 0 max_tile_y in
                          pac := Pacman.move_to !pac nx ny
                      | `Left ->
                          pac := Pacman.set_direction !pac Pacman.Left;
                          let nx, ny = Pacman.next_position !pac in
                          let nx = clamp nx 0 max_tile_x in
                          let ny = clamp ny 0 max_tile_y in
                          pac := Pacman.move_to !pac nx ny
                      | _ -> ())
                  | _ -> ()
                done;

                (* erases old screen with new black background*)
                Sdl.set_render_draw_color renderer 0 0 0 255 |> ignore;
                Sdl.render_clear renderer |> ignore;

                let px, py = to_pixel (Pacman.position !pac) in
                (*pacman's new position*)
                Sdl.set_render_draw_color renderer 255 255 0 255 |> ignore;
                draw_circle renderer px py ball_radius;

                (*draw new circle*)
                Sdl.render_present renderer
              done))
