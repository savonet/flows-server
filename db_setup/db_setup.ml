let db =
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

  let () =
  [%pgsql
    dbh "execute"
      "CREATE TABLE IF NOT EXISTS stream (\n\
      \      id SERIAL PRIMARY KEY,\n\
      \      format TEXT NOT NULL,\n\
      \      url TEXT NOT NULL,\n\
      \      radio_id INTEGER NOT NULL REFERENCES radio (id),\n\
      \      created_at TIMESTAMP WITH TIME ZONE NOT NULL,\n\
      \      updated_at TIMESTAMP WITH TIME ZONE NOT NULL)"]
  in

  dbh
