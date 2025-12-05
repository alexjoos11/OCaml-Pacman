open Game_engine_interface
open Pacman

module Make
    (Maze : MAZE)
    (Pacman : PACMAN)
    (Ghost : GHOST)
    (Constants : CONSTANTS) =
struct
  let move_pacman maze pac =
    let nx, ny = Pacman.next_position pac in
    if nx = 0 && ny = 14 then Pacman.move_to pac 28 14
    else if nx = 28 && ny = 14 then Pacman.move_to pac 0 14
    else if Maze.is_wall maze nx ny then pac
    else Pacman.move_to pac nx ny

  (** A list of directions a ghost may take when its intended chase direction is
      blocked by a wall. These represent the four cardinal moves: right, left,
      down, up. These directions are only used when the primary chase direction
      is invalid. A random valid fallback direction is chosen each frame. *)
  let fallback_dirs = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]

  let move_ghost maze g pac_pos =
    let gx, gy = Ghost.position g in
    let desired_x, desired_y = Ghost.next_position g ~pac_pos in

    if not (Maze.is_wall maze desired_x desired_y) then
      if pac_pos = (0, 15) then Ghost.move_to g 28 15
      else if pac_pos = (28, 15) then Ghost.move_to g 0 15
      else Ghost.move_to g desired_x desired_y
    else
      let valid =
        List.filter
          (fun (dx, dy) ->
            let tx, ty = (gx + dx, gy + dy) in
            not (Maze.is_wall maze tx ty))
          fallback_dirs
      in
      match valid with
      | [] -> g
      | dirs ->
          let dx, dy = List.nth dirs (Random.int (List.length dirs)) in
          Ghost.move_to g (gx + dx) (gy + dy)
end
