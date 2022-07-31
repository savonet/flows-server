(** Users. *)

type sha256 = Sha256.t

(** An user. *)
type t = {
  id : int;
  name : string;
  password : sha256;
  email : string option;
  last_sign_in_at : float option;
  created_at : float;
  updated_at : float;
}

let populate user =
  {
    id = user#id;
    name = user#name;
    password = user#password;
    email = user#email;
    last_sign_in_at = user#last_sign_in_at;
    created_at = user#created_at;
    updated_at = user#updated_at;
  }

let fetch ~db id =
  let id = Int32.of_int id in
  populate
    (List.hd
       [%pgsql.object
         db "load_custom_from=src/db_config.sexp" "show=pp"
           "SELECT * FROM flows_user WHERE id = $id"])

let find ?email ~db name =
  let req =
    match email with
      | Some email ->
          [%pgsql.object
            db "load_custom_from=src/db_config.sexp" "show=pp"
              "SELECT * FROM flows_user WHERE name = $name AND email = $email"]
      | None ->
          [%pgsql.object
            db "load_custom_from=src/db_config.sexp" "show=pp"
              "SELECT * FROM flows_user WHERE name = $name"]
  in
  match req with [] -> None | user :: _ -> Some (populate user)

let update ~db user =
  let { id; name; password; email; last_sign_in_at; created_at; _ } = user in
  let updated_at = Unix.time () in
  let id = Int32.of_int id in
  let password = Sha256.to_hex password in
  let () =
    [%pgsql
      db "load_custom_from=src/db_config.sexp" "show=pp"
        "UPDATE flows_user SET
       name = $name,
       password = $password,
       email = $?email,
       last_sign_in_at = $?last_sign_in_at,
       created_at = $created_at,
       updated_at = $updated_at
    WHERE id = $id"]
  in
  { user with updated_at }

let create ?email ?last_sign_in_at ~db ~name ~password () =
  let password = Sha256.to_hex password in
  let created_at = Unix.time () in
  let id =
    List.hd
      [%pgsql
        db "load_custom_from=src/db_config.sexp" "show=pp"
          "INSERT INTO
       flows_user (name, password, email, last_sign_in_at, created_at, updated_at)
     VALUES ($name, $password, $?email, $?last_sign_in_at, $created_at, $created_at)
     RETURNING id"]
  in
  {
    id;
    name;
    password = Sha256.of_hex password;
    email;
    last_sign_in_at;
    created_at;
    updated_at = created_at;
  }

(** Test whether the user/pass combination is valid. Register it if the user
    does not already exist. *)
let valid_or_register ?email ~db ~user ~password () =
  let password = Sha256.string password in
  match find ?email ~db user with
    | Some user when Sha256.equal password user.password ->
        Some (update ~db { user with last_sign_in_at = Some (Unix.time ()) })
    | Some _ -> None
    | None ->
        Some
          (create ~db ~name:user ~password ?email
             ~last_sign_in_at:(Unix.time ()) ())
