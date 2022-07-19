(** Abstract database operations. *)

type 'a t =
  {
    to_string : string -> 'a -> string;
    of_string : string -> string * 'a;
    mutable table : (string * 'a) list;
    m : Mutex.t
  }

let mutexify db f x =
  Mutex.lock db.m;
  let y = f x in
  Mutex.unlock db.m;
  y

let create ~to_string ~of_string (_name : string) : 'a t =
  (* TODO: read here *)
  { to_string; of_string; table = []; m = Mutex.create () }

let add (db : 'a t) k v =
  (* TODO: save after *)
  mutexify db (fun () ->
      let table = List.remove_assoc k db.table in
      db.table <- (k,v)::table
    ) ()

let find_opt (db : 'a t) k =
  mutexify db (fun k ->
      List.assoc_opt k db.table
    ) k
