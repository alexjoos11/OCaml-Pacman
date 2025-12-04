val load_score : unit -> int
(** [load_score] goes into the file and reads the highest score saved. If the
    file doesn't exist or the high score is unable to be read 0 is returned. *)

val save_score : int -> unit
(** [save_score] requires the [score] which is of type int and then removes the
    high score in the file and replaces it with [score] which is the new high
    score. *)
