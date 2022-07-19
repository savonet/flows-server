(** Abstract database operations. *)

type 'a t =
  {
    name : string;
    to_string : string -> 'a -> string;
    of_string : string -> string * 'a;
    mutable table : (string * 'a) list;
    m : Mutex.t
  }

let filename db = db.name ^ ".db"

let mutexify db f x =
  Mutex.lock db.m;
  let y = f x in
  Mutex.unlock db.m;
  y

let create ~to_string ~of_string name =
  let db = { name; to_string; of_string; table = []; m = Mutex.create () } in
  let filename = filename db in
  if Sys.file_exists filename then
    let ic = open_in filename in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let table = String.split_on_char '\n' s |> List.filter (fun s -> s <> "") |> List.map db.of_string in
    { db with table }
  else db

let add db k v =
  mutexify db (fun () ->
      let table = List.remove_assoc k db.table in
      db.table <- (k,v)::table;
      let oc = open_out (filename db) in
      List.iter (fun (k,v) -> output_string oc (db.to_string k v); output_string oc "\n") db.table;
      close_out oc
    ) ()

let find_opt db k =
  mutexify db (fun k ->
      List.assoc_opt k db.table
    ) k

let to_seq db = List.to_seq db.table
