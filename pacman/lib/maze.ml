type t = { dummy : int (* replace later *) }

let load _ = { dummy = 0 }
let is_wall _ _ _ = false
let pellet_at _ _ _ = false
let eat_pellet t _ _ = t
let pellets_remaining _ = 0
