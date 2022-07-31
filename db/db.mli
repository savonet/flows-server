type db = (string, bool) Hashtbl.t PGOCaml.t

val setup : unit -> unit
val transaction : (db -> 'a) -> 'a
