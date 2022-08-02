(** Users. *)

open Utils

(** An user. *)
type t = {
  id : int;
  email : string;
  password : Sha256.t;
  last_sign_in_at : float option;
  created_at : float;
  updated_at : float;
}

let populate { int; string; float_opt; float; _ } =
  {
    id = int "id";
    email = string "email";
    password = Sha256.of_hex (string "password");
    last_sign_in_at = float_opt "last_sign_in_at_epoch";
    created_at = float "created_at_epoch";
    updated_at = float "updated_at_epoch";
  }

let select_query =
  "SELECT
    *,
    extract(epoch from last_sign_in_at) AS last_sign_in_at_epoch,
    extract(epoch from updated_at) AS updated_at_epoch,
    extract(epoch from created_at) AS created_at_epoch
  FROM flows_user WHERE id = $1"

let fetch ~(db : Db.db) id =
  match
    list_of_result
      (db#exec
         ~expect:[Postgresql.Tuples_ok; Postgresql.Command_ok]
         ~params:[| string_of_int id |]
         select_query)
  with
    | [] -> raise Not_found
    | r :: _ -> populate r

let find_query =
  "SELECT
    *,
    extract(epoch from last_sign_in_at) AS last_sign_in_at_epoch,
    extract(epoch from updated_at) AS updated_at_epoch,
    extract(epoch from created_at) AS created_at_epoch
  FROM flows_user WHERE email = $1"

let find ~(db : Db.db) email =
  match
    list_of_result
      (db#exec ~expect:[Postgresql.Tuples_ok] ~params:[| email |] find_query)
  with
    | [] -> None
    | r :: _ -> Some (populate r)

let create_query =
  "INSERT INTO
      flows_user (email, password, last_sign_in_at,  created_at,       updated_at)
    VALUES
                 ($1,   $2,        to_timestamp($3), to_timestamp($4), to_timestamp($5))
    RETURNING id"

let create ?last_sign_in_at ~(db : Db.db) ~email ~password () =
  let password = Sha256.string password in
  let created_at = Unix.time () in
  let params =
    [|
      email;
      Sha256.to_hex password;
      opt (Option.map string_of_float last_sign_in_at);
      string_of_float created_at;
      string_of_float created_at;
    |]
  in
  match
    list_of_result (db#exec ~expect:[Postgresql.Tuples_ok] ~params create_query)
  with
    | [{ int; _ }] ->
        {
          id = int "id";
          email;
          password;
          last_sign_in_at;
          created_at;
          updated_at = created_at;
        }
    | _ -> assert false

let update_last_sign_at_query =
  "UPDATE flows_user SET last_sign_in_at = to_timestamp($1) WHERE id = $2"

let update_last_sign_at ~(db : Db.db) user =
  let last_sign_in_at = Unix.time () in
  ignore
    (db#exec
       ~params:[| string_of_float last_sign_in_at; string_of_int user.id |]
       update_last_sign_at_query);
  { user with last_sign_in_at = Some last_sign_in_at }

(** Test whether the user/pass combination is valid. Register it if the user
    does not already exist. *)
let valid_or_register ~(db : Db.db) ~email ~password () =
  match find ~db email with
    | Some user when Sha256.equal (Sha256.string password) user.password ->
        Some (update_last_sign_at ~db user)
    | Some _ -> None
    | None ->
        Some (create ~db ~password ~email ~last_sign_in_at:(Unix.time ()) ())
