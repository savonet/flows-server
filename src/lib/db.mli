type db = Postgresql.connection

val db :
  ?host:string ->
  ?port:int ->
  ?user:string ->
  ?password:string ->
  ?database:string ->
  unit ->
  db

val setup : ?db:db -> unit -> unit
val transaction : (db -> 'a) -> 'a
