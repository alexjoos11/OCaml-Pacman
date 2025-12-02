(** Representation of a single maze tile. *)
type tile =
  | Wall  (** Impassable wall tile. *)
  | Pellet  (** Pellet that Pac-Man can consume. *)
  | Empty  (** Passable empty tile. *)

(** The maze grid is stored as a 2D array for O(1) tile access, which is
    essential for constant-time wall checks and movement updates during
    gameplay. *)

type t = {
  grid : tile array array;  (** 2D array indexed as [grid.(y).(x)]. *)
  width : int;  (** Number of columns. *)
  height : int;  (** Number of rows. *)
}

(** [char_to_tile c] converts a map character into a tile:
    - ['#'] becomes [Wall]
    - ['.'] becomes [Pellet]
    - any other character becomes [Empty] *)
let char_to_tile = function
  | '#' -> Wall
  | '.' -> Pellet
  | _ -> Empty

(** [create_from_chars lines] constructs a maze from a textual representation.
    Each string in [lines] represents one row of the maze. All strings must have
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

(** [create ()] constructs the maze for this game. ... (omitted create comment
    for brevity) ... *)
let create () =
  create_from_chars
    [
      "############################";
      "#............##............#";
      "#.####.##.  .##.#####.    .#";
      "#.####.    #.##.#.   .# . .#";
      (* Power Pellet at (1, 3) *)
      "#.####.#.  #.##.#####.#. #.#";
      "#..........................#";
      "#.####.##.##.   ##.##.####.#";
      "#......##....##....##......#";
      "##. ##.##### ## #####.##.  #";
      "#####..##### ##  ####..#.  #";
      "#####.##     ##     ##.##. #";
      "#####.## ### ## ### ##.#####";
      "#........### ## ###........#";
      "#####.## ### ## ### ##.#####";
      "#####.##          ##.   .  #";
      "#####.## ##. #### ##.#######";
      "#............##.....  .....#";
      "#.######.       ###. #####.#";
      "#.###.  ##########.   ####.#";
      "#...............##.........#";
      "#.####.#. ##.##.  . #.## #.#";
      "#.####.      .##.   #.## #.#";
      "#............##............#";
      "#.###.    ###.     #######.#";
      "#.#######.      ##   #####.#";
      "#..........................#";
      (* Power Pellet at (1, 25) and (26, 25) *)
      "#.##.    ###.##.#.  #.####.#";
      "#.####.#.  #.##.##. #.####.#";
      "#............##............#";
      "#####.             #########";
      "############################";
    ]

(** [width m] returns the width in tiles. *)
let width m = m.width

(** [height m] returns the height in tiles. *)
let height m = m.height

(** [is_wall m x y] is true if [(x, y)] is outside bounds or a wall. *)
let is_wall m x y =
  if x < 0 || y < 0 || x >= m.width || y >= m.height then true
  else
    match m.grid.(y).(x) with
    | Wall -> true
    | _ -> false

(** [pellet_at m x y] is true if tile [(x, y)] contains a pellet. *)
let pellet_at m x y =
  if x < 0 || y < 0 || x >= m.width || y >= m.height then false
  else
    match m.grid.(y).(x) with
    | Pellet -> true
    | _ -> false

(** [eat_pellet m x y] returns a new maze identical to [m] except that if tile
    [(x, y)] contains a pellet, that pellet is removed. ... (omitted eat_pellet
    comment for brevity) ... *)
let eat_pellet m x y =
  if pellet_at m x y then (
    let new_grid = Array.map Array.copy m.grid in
    new_grid.(y).(x) <- Empty;
    { m with grid = new_grid })
  else m

(** [pellets_exist m] is [true] if any pellet exists in the maze. This function
    stops scanning immediately upon finding a pellet. *)
let pellets_exist m =
  Array.exists (fun row -> Array.exists (fun tile -> tile = Pellet) row) m.grid

(* --- ADDED FUNCTION --- *)

(** [is_power_pellet m x y] is true if the pellet at [(x, y)] is one of the four
    corner power pellets. *)
let is_power_pellet m x y =
  pellet_at m x y
  &&
  match (x, y) with
  | 1, 3 | 26, 3 | 1, 25 | 26, 25 -> true
  | _ -> false

(* Test-only helper (not exposed in the .mli documentation). Re-exports
   [create_from_chars] so unit tests can build custom mazes without relying on
   the default level design. *)
let create_from_chars_for_tests = create_from_chars
