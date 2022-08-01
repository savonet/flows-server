type db = Pgx_lwt_unix.t

val setup : unit -> unit Lwt.t
val transaction : (db -> 'a Lwt.t) -> 'a Lwt.t
