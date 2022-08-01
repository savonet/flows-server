type db = Pgx_lwt_unix.t

val db :
  ?host:string ->
  ?port:int ->
  ?user:string ->
  ?password:string ->
  ?database:string ->
  unit ->
  db Lwt.t

val setup : ?db:db -> unit -> unit Lwt.t
val transaction : (db -> 'a Lwt.t) -> 'a Lwt.t
