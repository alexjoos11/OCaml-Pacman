type item =
  | Pellet
  | PowerPellet
  | Cherry

type tile =
  | Wall
  | Item of item
  | Empty

(** The maze grid is stored as a 2D array for O(1) tile access, which is
    essential for constant-time wall checks and movement updates during
    gameplay. *)

type t = {
  grid : tile array array;  (** 2D array indexed as [grid.(y).(x)]. *)
  width : int;  (** Number of columns. *)
  height : int;  (** Number of rows. *)
}

(** [char_to_tile] converts a map character into a tile where ['#'] becomes
    [Wall], ['.'] becomes [Pellet], ['*'] becomes [PowerPellet], ['C'] becomes
    [Cherry], and any other character becomes [Empty] *)
let char_to_tile = function
  | '#' -> Wall
  | '.' -> Item Pellet
  | '*' -> Item PowerPellet
  | 'C' -> Item Cherry
  | _ -> Empty

(** [create_from_chars lines] constructs a maze from a text representation. Each
    string in [lines] represents one row of the maze. All strings must have
    equal length. *)
let create_from_chars (lines : string list) : t =
  let height = List.length lines in
  let width = String.length (List.hd lines) in
  let grid =
    Array.init height (fun y ->
        Array.init width (fun x ->
            char_to_tile (String.get (List.nth lines y) x)))
  in
  { grid; width; height }

(** [create ()] constructs the maze for this game. The maze layout is defined
    directly in this source file rather than being loaded from an external
    [maze.txt]. Keeping the maze inline has several advantages for this project
    including simplicity since no input or output file is needed, determinism
    since the maze is embedded in the executable, testing since unit tests can
    rely on a fixed maze structure, and scope. *)

let create (filename : string) =
  let ic = open_in filename in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  create_from_chars (loop [])

let width m = m.width
let height m = m.height

let is_wall m x y =
  if x < 0 || y < 0 || x >= m.width || y >= m.height then true
  else
    match m.grid.(y).(x) with
    | Wall -> true
    | _ -> false

let item_at m x y =
  if x < 0 || y < 0 || x >= m.width || y >= m.height then None
  else
    match m.grid.(y).(x) with
    | Item Pellet -> Some Pellet
    | Item PowerPellet -> Some PowerPellet
    | Item Cherry -> Some Cherry
    | _ -> None

let eat_item m x y =
  if item_at m x y <> None then (
    let new_grid = Array.map Array.copy m.grid in
    new_grid.(y).(x) <- Empty;
    { m with grid = new_grid })
  else m

let items_exist m =
  Array.exists
    (fun row ->
      Array.exists
        (function
          | Item _ -> true
          | _ -> false)
        row)
    m.grid

let create_for_tests = create
