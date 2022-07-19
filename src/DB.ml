(** Abstract database operations. *)

type 'a t =
  {
    name : string;
    to_json : 'a -> JSON.t;
    of_json : JSON.t -> 'a;
    mutable table : (string * 'a) list;
    m : Mutex.t
  }

let filename db = db.name ^ ".db"

let mutexify db f x =
  Mutex.lock db.m;
  let y = f x in
  Mutex.unlock db.m;
  y

let create ~to_json ~of_json name =
  let db = { name; to_json; of_json; table = []; m = Mutex.create () } in
  let filename = filename db in
  if Sys.file_exists filename then
    let ic = open_in filename in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let table =
      let table = JSON.of_string s in
      match table with
      | `Assoc l -> List.map (fun (k,v) -> k, of_json v) l
      | _ -> assert false
    in
    { db with table }
  else db

let add db k v =
  mutexify db (fun () ->
      let table = List.remove_assoc k db.table in
      db.table <- (k,v)::table;
      let oc = open_out (filename db) in
      `Assoc (List.map (fun (k,v) -> k, db.to_json v) db.table) |> JSON.to_string |> output_string oc;
      close_out oc
    ) ()

let find_opt db k =
  mutexify db (fun k ->
      List.assoc_opt k db.table
    ) k

let to_seq db = List.to_seq db.table
