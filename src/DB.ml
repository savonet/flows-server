let () =
  let host, port, user, password, database =
    match Sys.getenv_opt "DATABASE_URL" with
      | None -> (None, None, None, None, None)
      | Some uri ->
          let uri = Uri.of_string uri in
          let path =
            match Uri.path uri with
              | "" -> None
              | s -> Some (String.sub s 1 (String.length s))
          in
          (Uri.host uri, Uri.port uri, Uri.user uri, Uri.password uri, path)
  in
  let dbh = PGOCaml.connect ?host ?port ?user ?password ?database () in

  let () =
    [%pgsql
      dbh "execute"
        "CREATE TABLE IF NOT EXISTS flows_user (\n\
        \      id SERIAL PRIMARY KEY,\n\
        \      name TEXT NOT NULL,\n\
        \      email TEXT,\n\
        \      password VARCHAR NOT NULL,\n\
        \      last_sign_in_at TIMESTAMP WITH TIME ZONE NOT NULL,\n\
        \      created_at TIMESTAMP WITH TIME ZONE NOT NULL,\n\
        \      updated_at TIMESTAMP WITH TIME ZONE NOT NULL)"]
  in

  let () =
    [%pgsql
      dbh "execute"
        "CREATE TABLE IF NOT EXISTS radio (\n\
        \      id SERIAL PRIMARY KEY,\n\
        \      user_id INTEGER NOT NULL REFERENCES flows_user (id),\n\
        \      name TEXT NOT NULL,\n\
        \      website TEXT,\n\
        \      genre TEXT,\n\
        \      logo TEXT,\n\
        \      longiture NUMERIC,\n\
        \      latitude NUMERIC,\n\
        \      artist TEXT,\n\
        \      title TEXT,\n\
        \      created_at TIMESTAMP WITH TIME ZONE NOT NULL,\n\
        \      updated_at TIMESTAMP WITH TIME ZONE NOT NULL)"]
  in

  [%pgsql
    dbh "execute"
      "CREATE TABLE IF NOT EXISTS stream (\n\
      \      id SERIAL PRIMARY KEY,\n\
      \      format TEXT NOT NULL,\n\
      \      url TEXT NOT NULL,\n\
      \      radio_id INTEGER NOT NULL REFERENCES radio (id),\n\
      \      created_at TIMESTAMP WITH TIME ZONE NOT NULL,\n\
      \      updated_at TIMESTAMP WITH TIME ZONE NOT NULL)"]

module M = Map.Make (struct
  type t = string

  let compare (x : string) (y : string) = compare x y
end)

(** A database. *)
type 'a t = {
  name : string;
  to_json : 'a -> JSON.t;
  of_json : JSON.t -> 'a;
  mutable table : 'a M.t;
  save_every : float;
      (** wait at least for this number of seconds before saving *)
  mutable save_last : float;  (** when we last saved *)
  mutex_table : Mutex.t;
  mutex_file : Mutex.t;
}

let filename db = db.name ^ ".db"

(** Create a database. *)
let create ~to_json ~of_json ?(every = 0.) name =
  let db =
    {
      name;
      to_json;
      of_json;
      table = M.empty;
      save_every = every;
      save_last = 0.;
      mutex_table = Mutex.create ();
      mutex_file = Mutex.create ();
    }
  in
  let filename = filename db in
  if Sys.file_exists filename then (
    let ic = open_in filename in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let table =
      let table = JSON.of_string s in
      match table with
        | `Assoc l ->
            l |> List.to_seq
            |> Seq.map (fun (k, v) -> (k, of_json v))
            |> M.of_seq
        | _ -> assert false
    in
    { db with table })
  else db

(** Add an entry to the database. Any previous value associated to the key is
    removed. *)
let add db k v =
  let table =
    Mutex.lock db.mutex_table;
    db.table <- M.add k v db.table;
    db.table
  in
  Mutex.unlock db.mutex_table;
  let now = Unix.time () in
  Mutex.lock db.mutex_file;
  if now -. db.save_last >= db.save_every then (
    let oc = open_out (filename db) in
    table |> M.to_seq
    |> Seq.map (fun (k, v) -> (k, db.to_json v))
    |> List.of_seq
    |> (fun l -> `Assoc l)
    |> JSON.to_string |> output_string oc;
    close_out oc;
    db.save_last <- now;
    Mutex.unlock db.mutex_file)
  else Mutex.unlock db.mutex_file

(** Find an entry. *)
let find_opt db k = M.find_opt k db.table

let iter db f = M.iter f db.table

(** Iterator over database. *)
let to_seq db = M.to_seq db.table
