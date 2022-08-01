type db = Pgx_lwt_unix.t

let db () =
  let host, port, user, password, database =
    match Sys.getenv_opt "DATABASE_URL" with
      | None ->
          ( Sys.getenv_opt "PGHOST",
            Option.map int_of_string (Sys.getenv_opt "PGPORT"),
            Sys.getenv_opt "PGUSER",
            Sys.getenv_opt "PGPASSWORD",
            Sys.getenv_opt "PGDATABASE" )
      | Some uri ->
          let uri = Uri.of_string uri in
          let path =
            match Uri.path uri with
              | "" -> None
              | s -> Some (String.sub s 1 (String.length s - 1))
          in
          (Uri.host uri, Uri.port uri, Uri.user uri, Uri.password uri, path)
  in

  Printf.printf
    {|Detected database config:
Host:     %s
Port:     %s
User:     %s
Password: %s
Database: %s
|}
    (Option.value ~default:"(default)" host)
    (Option.value ~default:"(default)" (Option.map string_of_int port))
    (Option.value ~default:"(default)" user)
    (Option.value ~default:"" password)
    (Option.value ~default:"(default)" database);

  Pgx_lwt_unix.connect ?host ?port ?user ?password ?database ()

let transaction fn =
  let%lwt db = db () in
  (Pgx_lwt_unix.with_transaction db fn) [%lwt.finally Pgx_lwt_unix.close db]

let execute ~db query = Pgx_lwt_unix.execute_unit ~params:[] db query

let table_queries =
  [
    "CREATE TABLE IF NOT EXISTS flows_user (
      id SERIAL PRIMARY KEY,
      name TEXT NOT NULL UNIQUE,
      email TEXT,
      password VARCHAR NOT NULL,
      last_sign_in_at TIMESTAMP WITH TIME ZONE,
      created_at TIMESTAMP WITH TIME ZONE NOT NULL,
      updated_at TIMESTAMP WITH TIME ZONE NOT NULL)";
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
      updated_at TIMESTAMP WITH TIME ZONE NOT NULL)";
    "CREATE TABLE IF NOT EXISTS stream (
      id SERIAL PRIMARY KEY,
      format TEXT NOT NULL,
      url TEXT NOT NULL,
      radio_id INTEGER NOT NULL REFERENCES radio (id),
      created_at TIMESTAMP WITH TIME ZONE NOT NULL,
      updated_at TIMESTAMP WITH TIME ZONE NOT NULL)";
  ]

let setup ?db () =
  let exec db = Lwt_list.iter_s (execute ~db) table_queries in
  match db with
    | Some db -> exec db
    | None -> transaction exec
