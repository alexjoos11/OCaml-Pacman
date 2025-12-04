(** The mode type represents the behavioral state of a ghost in Pac-Man.
    - [Attack]: The ghost actively pursues Pac-Man according to its hunting
      strategy.
    - [Frightened]: The ghost is vulnerable and flees from Pac-Man after
      consuming a power pellet.
    - [Eaten]: The ghost has been consumed by Pac-Man and is regenerating at the
      ghost house. *)
type mode =
  | Attack
  | Frightened
  | Eaten

(** The speed type represents how fast the ghost is currently moving*)
type speed =
  | Fast
  | Regular
  | Slow
  | Paused

type t = {
  x : int;
  y : int;
  mode : mode;
  home_x : int;
  home_y : int;
  ai : Ai.ai;
  speed : speed;
  timer : float;
}

let create x y ai =
  {
    x;
    y;
    mode = Attack;
    home_x = x;
    home_y = y;
    ai;
    speed = Regular;
    timer = 5.0;
  }

let position g = (g.x, g.y)

let next_position g ~pac_pos =
  if g.speed = Paused then (g.x, g.y)
  else
    match g.mode with
    | Attack -> g.ai.attack ~x:g.x ~y:g.y ~pac_pos
    | Frightened -> g.ai.runaway ~x:g.x ~y:g.y ~pac_pos
    | Eaten -> g.ai.go_home ~x:g.x ~y:g.y ~home:(g.home_x, g.home_y)

let move_to g nx ny = { g with x = nx; y = ny }

let set_frightened g bool =
  if bool then { g with mode = Frightened }
  else
    match g.mode with
    | Eaten -> g
    | Attack | Frightened -> { g with mode = Attack }

let is_frightened g =
  match g.mode with
  | Frightened -> true
  | _ -> false

let set_eaten g eaten =
  if eaten then { g with mode = Eaten } else { g with mode = Attack }

let is_eaten g = g.mode = Eaten
let is_at_home g = (g.x, g.y) = (g.home_x, g.home_y)
let respawn g = { g with x = g.home_x; y = g.home_y; mode = Attack }
let color g = g.ai.color

let next_speed current =
  match current with
  | Regular -> Fast
  | Fast -> Slow
  | Slow -> Paused
  | Paused -> Regular

let get_time g = g.timer
let get_speed g = g.speed
let set_speed g speed duration = { g with speed; timer = duration }

(** HELPER: to switch modes**)
let switch_speed g = { g with speed = next_speed g.speed; timer = 5.0 }

let update_duration g ~time =
  let new_timer = g.timer -. time in
  if new_timer <= 0.0 then switch_speed g else { g with timer = new_timer }

let speed_factor g =
  match get_speed g with
  | Fast -> 2.0
  | Regular -> 1.0
  | Slow -> 0.5
  | Paused -> 0.0
