type db = (string, bool) Hashtbl.t PGOCaml.t

let db () =
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

  PGOCaml.connect ?host ?port ?user ?password ?database ()

let transaction fn =
  let db = db () in
  try PGOCaml.transact db fn
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    PGOCaml.close db;
    Printexc.raise_with_backtrace exn bt

let setup () =
  transaction (fun db ->
      PGOCaml.prepare db
        ~query:
          "CREATE TABLE IF NOT EXISTS flows_user (
              id SERIAL PRIMARY KEY,
              name TEXT NOT NULL UNIQUE,
              email TEXT,
              password VARCHAR NOT NULL,
              last_sign_in_at TIMESTAMP WITH TIME ZONE,
              created_at TIMESTAMP WITH TIME ZONE NOT NULL,
              updated_at TIMESTAMP WITH TIME ZONE NOT NULL)"
        ();
      ignore (PGOCaml.execute db ~params:[] ());

      PGOCaml.prepare db
        ~query:
          "CREATE TABLE IF NOT EXISTS radio (
              id SERIAL PRIMARY KEY,
              user_id INTEGER NOT NULL REFERENCES flows_user (id),
              name TEXT NOT NULL UNIQUE,
              description TEXT,
              website TEXT,
              genre TEXT,
              logo TEXT,
              longitude DOUBLE PRECISION,
              latitude DOUBLE PRECISION,
              artist TEXT,
              title TEXT,
              created_at TIMESTAMP WITH TIME ZONE NOT NULL,
              updated_at TIMESTAMP WITH TIME ZONE NOT NULL)"
        ();
      ignore (PGOCaml.execute db ~params:[] ());

      PGOCaml.prepare db
        ~query:
          "CREATE TABLE IF NOT EXISTS stream (
              id SERIAL PRIMARY KEY,
              format TEXT NOT NULL,
              url TEXT NOT NULL,
              radio_id INTEGER NOT NULL REFERENCES radio (id),
              created_at TIMESTAMP WITH TIME ZONE NOT NULL,
              updated_at TIMESTAMP WITH TIME ZONE NOT NULL)"
        ();
      ignore (PGOCaml.execute db ~params:[] ()))
