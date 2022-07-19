(** Users. *)

(** An user. *)
type t = {
  user : string; (** user *)
  pass : Sha256.t; (** encrypted password *)
  mail : string;
  last : float; (** last login *)
}

let to_json u =
  `Assoc [
    "user", `String u.user;
    "pass", `String (Sha256.to_hex u.pass);
    "mail", `String u.mail;
    "last", `Float u.last
  ]

let of_json = function
  | `Assoc l ->
    {
      user = List.assoc "user" l |> JSON.string;
      pass = List.assoc "pass" l |> JSON.string |> Sha256.of_hex;
      mail = List.assoc "mail" l |> JSON.string;
      last = List.assoc "last" l |> JSON.float;
    }
  | _ -> assert false

let db = DB.create ~of_json ~to_json "users"

(** Find user with given username. *)
let find_opt user =
  DB.find_opt db user

(** Test whether the user/pass combination is valid. Register it if the user
    does not already exist. *)
let valid_or_register ~user ~pass ~mail =
  let pass = Sha256.string pass in
  match DB.find_opt db user with
  | Some u ->
    if Sha256.equal pass u.pass then
      (
        DB.add db user { u with last = Unix.time () };
        true
      )
    else false
  | None ->
    let u = { user; pass; mail; last = Unix.time () } in
    DB.add db user u;
    true
