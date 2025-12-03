type item =
  | Pellet  (** Pellet that Pac-Man can consume. *)
  | PowerPellet
      (** Power pellet that Pac-Man can consume to temporarily eat ghosts. *)
  | Cherry  (** Bonus cherry that Pac-Man can consume for extra points. *)

(** Representation of a single maze tile. *)
type tile =
  | Wall  (** Impassable wall tile. *)
  | Item of item  (** One of the optional item types*)
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
    - ['*'] becomes [PowerPellet]
    - ['C'] becomes [Cherry]
    - any other character becomes [Empty] *)
let char_to_tile = function
  | '#' -> Wall
  | '.' -> Item Pellet
  | '*' -> Item PowerPellet
  | 'C' -> Item Cherry
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

(** [create ()] constructs the maze for this game.

    The maze layout is defined directly in this source file rather than being
    loaded from an external [maze.txt]. Keeping the maze inline has several
    advantages for this project:

    - **Simplicity:** No file I/O is needed, avoiding path issues and dune
      configuration overhead.
    - **Determinism:** The maze is embedded in the executable, ensuring it is
      always available and consistent across environments.
    - **Testing:** Unit tests can rely on a fixed, compile-time maze structure
      without needing external resources.
    - **Scope:** This project uses a single level, so a data file adds
      complexity without providing meaningful benefit.

    The map uses ['#'] for walls, ['.'] for pellets, ['*'] for power pellets,
    and other characters for empty tiles. *)
let create () =
  create_from_chars
    [
      "############################";
      "#..*.........##............#";
      "#.####.##.  .##.#####.    .#";
      "#.####.    #.##.#.   .# . .#";
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
      "#............##..*.........#";
      "#.###.    ###.     #######.#";
      "#.#######.      ##   #####.#";
      "#..........................#";
      "#.##.    ###.##.#.  #.####.#";
      "#.####.#.  #.##.##. #.####.#";
      "#.....C......##............#";
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

(** [item_at m x y] is true if tile [(x, y)] contains an [item]. *)
let item_at m x y =
  if x < 0 || y < 0 || x >= m.width || y >= m.height then None
  else
    match m.grid.(y).(x) with
    | Item Pellet -> Some Pellet
    | Item PowerPellet -> Some PowerPellet
    | Item Cherry -> Some Cherry
    | _ -> None

(** [eat_item m x y] returns a new maze identical to [m] except that if tile
    [(x, y)] contains an item, that item is removed.

    This function does *not* mutate the existing maze. Instead it performs a
    persistent update:

    - A fresh copy of the entire grid is created using [Array.map Array.copy].
    - The copy of row [y] is updated at column [x] to [Empty].
    - A new maze record is returned with this modified grid.
    - The original maze [m] remains unchanged.

    This preserves immutability for the game engine: previous maze states remain
    valid, and [eat_item] behaves like a pure function even though arrays are
    used internally for efficiency. *)
let eat_item m x y =
  if item_at m x y <> None then (
    let new_grid = Array.map Array.copy m.grid in
    new_grid.(y).(x) <- Empty;
    { m with grid = new_grid })
  else m

(** [items_exist m] is [true] if any item exists in the maze. This function
    stops scanning immediately upon finding an item. *)
let items_exist m =
  Array.exists
    (fun row ->
      Array.exists
        (function
          | Item _ -> true
          | _ -> false)
        row)
    m.grid

(* Test-only helper (not exposed in the .mli documentation). Re-exports
   [create_from_chars] so unit tests can build custom mazes without relying on
   the default level design. *)
let create_from_chars_for_tests = create_from_chars
