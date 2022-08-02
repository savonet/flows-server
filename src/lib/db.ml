type db = Postgresql.connection

let db ?host ?port ?user ?password ?database () =
  let port = Option.map string_of_int port in
  let d_host, d_port, d_user, d_password, d_database =
    match Sys.getenv_opt "DATABASE_URL" with
      | None ->
          ( Sys.getenv_opt "PGHOST",
            Sys.getenv_opt "PGPORT",
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
          ( Uri.host uri,
            Option.map string_of_int (Uri.port uri),
            Uri.user uri,
            Uri.password uri,
            path )
  in
  let f v d = match (v, d) with Some v, _ -> Some v | None, d -> d in
  let host, port, user, password, database =
    ( f host d_host,
      f port d_port,
      f user d_user,
      f password d_password,
      f database d_database )
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
    (Option.value ~default:"(default)" port)
    (Option.value ~default:"(default)" user)
    (Option.value ~default:"" password)
    (Option.value ~default:"(default)" database);

  new Postgresql.connection ?host ?port ?user ?password ?dbname:database ()

let transaction fn =
  let db = db () in
  ignore (db#exec "BEGIN");
  try
    let ret = fn db in
    ignore (db#exec "COMMIT");
    ret
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    ignore (db#exec "ROLLBACK");
    db#finish;
    Printexc.raise_with_backtrace exn bt

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
  let exec (db : db) =
    List.iter (fun query -> ignore (db#exec query)) table_queries
  in
  match db with Some db -> exec db | None -> transaction exec
