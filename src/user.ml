(** Users. *)

type t = {
  user : string; (** user *)
  pass : Sha256.t; (** encrypted password *)
  last : float; (** last login *)
}

let to_string user u =
  assert (user = u.user);
  Printf.sprintf "%S,%S,%f" u.user (Sha256.to_hex u.pass) u.last

let of_string s =
  Scanf.sscanf s "%S,%S,%f" (fun user pass last -> let pass = Sha256.of_hex pass in user, {user; pass; last} )

let db = DB.create ~of_string ~to_string "users"

let find_opt user =
  DB.find_opt db user

(** Test whether the user/pass combination is valid. Register it if the user
    does not already exist. *)
let valid_or_register user pass =
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
    let u = { user; pass; last = Unix.time () } in
    DB.add db user u;
    true