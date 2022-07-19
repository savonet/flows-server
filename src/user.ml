(** Users. *)

type t = {
  user : string; (** user *)
  pass : Sha256.t; (** encrypted password *)
}

let to_string user u =
  assert (user = u.user);
  Printf.sprintf "%S,%S" u.user (Sha256.to_hex u.pass)

let of_string s =
  Scanf.sscanf s "%S,%S" (fun user pass -> let pass = Sha256.of_hex pass in user, {user; pass} )

let db = DB.create ~of_string ~to_string "users"

let find_opt user =
  DB.find_opt db user

(** Test whether the user/pass combination is valid. Register it if the user
    does not already exist. *)
let valid_or_register user pass =
  let pass = Sha256.string pass in
  match DB.find_opt db user with
  | Some u -> Sha256.equal pass u.pass
  | None ->
    let u = {user; pass} in
    DB.add db user u;
    true
