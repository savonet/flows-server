(** Users. *)

(** An user. *)
type t = {
  id : int;
  name : string;
  password : Sha256.t;
  email : string option;
  last_sign_in_at : float option;
  created_at : float;
  updated_at : float;
}

let populate = function
  | [id; name; password; email; last_sign_in_at; created_at; updated_at] ->
      {
        id = Pgx.Value.to_int_exn id;
        name = Pgx.Value.to_string_exn name;
        password = Sha256.of_hex (Pgx.Value.to_string_exn password);
        email = Pgx.Value.to_string email;
        last_sign_in_at = Pgx.Value.to_float last_sign_in_at;
        created_at = Pgx.Value.to_float_exn created_at;
        updated_at = Pgx.Value.to_float_exn updated_at;
      }
  | _ -> assert false

let select_query =
  "SELECT
     id,
     name,
     password,
     email,
     extract(epoch from last_sign_in_at),
     extract(epoch from created_at),
     extract(epoch from updated_at)
   FROM
     flows_user
   WHERE id = $1"

let fetch ~db id =
  match%lwt
    Pgx_lwt_unix.execute ~params:[Pgx.Value.of_int id] db select_query
  with
    | [row] -> Lwt.return (populate row)
    | _ -> assert false

let find_query =
  "SELECT
     id,
     name,
     password,
     email,
     extract(epoch from last_sign_in_at),
     extract(epoch from created_at),
     extract(epoch from updated_at)
   FROM
     flows_user
   WHERE name = $1"

let find_with_email_query =
  "SELECT
     id,
     name,
     password,
     email,
     extract(epoch from last_sign_in_at),
     extract(epoch from created_at),
     extract(epoch from updated_at)
   FROM
     flows_user
   WHERE name = $1
   AND email = $2"

let find ?email ~db name =
  let query, params =
    match email with
      | None -> (find_query, [Pgx.Value.of_string name])
      | Some email ->
          (find_with_email_query, Pgx.Value.[of_string name; of_string email])
  in
  match%lwt Pgx_lwt_unix.execute ~params db query with
    | row :: _ -> Lwt.return (Some (populate row))
    | _ -> Lwt.return None

let create_query =
  "INSERT INTO
      flows_user (name, password, email, last_sign_in_at,  created_at,       updated_at)
    VALUES
                 ($1,   $2,       $3,    to_timestamp($4), to_timestamp($5), to_timestamp($6))
    RETURNING id"

let create ?email ?last_sign_in_at ~db ~name ~password () =
  let password = Sha256.string password in
  let created_at = Unix.time () in
  let params =
    Pgx.Value.
      [
        of_string name;
        of_string (Sha256.to_hex password);
        opt of_string email;
        opt of_float last_sign_in_at;
        of_float created_at;
        of_float created_at;
      ]
  in
  match%lwt Pgx_lwt_unix.execute ~params db create_query with
    | [id] :: _ ->
        Lwt.return
          {
            id = Pgx.Value.to_int_exn id;
            name;
            password;
            email;
            last_sign_in_at;
            created_at;
            updated_at = created_at;
          }
    | _ -> assert false

let update_last_sign_at_query =
  "UPDATE flows_user SET update_last_sign_in_at = to_timestamp($1) WHERE id = $2"

let update_last_sign_at ~db user =
  let last_sign_in_at = Unix.time () in
  let%lwt () =
    Pgx_lwt_unix.execute_unit
      ~params:Pgx.Value.[of_float last_sign_in_at; of_int user.id]
      db update_last_sign_at_query
  in
  Lwt.return { user with last_sign_in_at = Some last_sign_in_at }

(** Test whether the user/pass combination is valid. Register it if the user
    does not already exist. *)
let valid_or_register ?email ~db ~user ~password () =
  match%lwt find ?email ~db user with
    | Some user when Sha256.equal (Sha256.string password) user.password ->
        let%lwt user = update_last_sign_at ~db user in
        Lwt.return (Some user)
    | Some _ -> Lwt.return None
    | None ->
        let%lwt user =
          create ~db ~name:user ~password ?email ~last_sign_in_at:(Unix.time ())
            ()
        in
        Lwt.return (Some user)
