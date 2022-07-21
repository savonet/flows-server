(** Abstract database operations. *)

(** A database. *)
type 'a t = {
  name : string;
  to_json : 'a -> JSON.t;
  of_json : JSON.t -> 'a;
  mutable table : (string * 'a) list;
  save_every : float; (** wait at least for this number of seconds before saving *)
  mutable save_last : float; (** when we last saved *)
  mutex_table : Mutex.t;
  mutex_file : Mutex.t;
}

let filename db = db.name ^ ".db"

(** Create a database. *)
let create ~to_json ~of_json ?(every=0.) name =
  let db = { name; to_json; of_json; table = []; save_every = every; save_last = 0.; mutex_table = Mutex.create (); mutex_file = Mutex.create () } in
  let filename = filename db in
  if Sys.file_exists filename then (
    let ic = open_in filename in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let table =
      let table = JSON.of_string s in
      match table with
        | `Assoc l -> List.map (fun (k, v) -> (k, of_json v)) l
        | _ -> assert false
    in
    { db with table })
  else db

(** Add an entry to the database. Any previous value associated to the key is
    removed. *)
let add db k v =
  let table =
    Mutex.lock db.mutex_table;
    let table = List.remove_assoc k db.table in
    db.table <- (k, v) :: table;
    db.table
  in
  Mutex.unlock db.mutex_table;
  let now = Unix.time () in
  Mutex.lock db.mutex_file;
  if now -. db.save_last >= db.save_every then
    (
      let oc = open_out (filename db) in
      `Assoc (List.map (fun (k, v) -> (k, db.to_json v)) table)
      |> JSON.to_string |> output_string oc;
      close_out oc;
      db.save_last <- now;
      Mutex.unlock db.mutex_file
    )
  else
    Mutex.unlock db.mutex_file

(** Find an entry. *)
let find_opt db k = List.assoc_opt k db.table

(** Iterator over database. *)
let to_seq db = List.to_seq db.table
