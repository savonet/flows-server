(** Users. *)

(** An user. *)
type t = {
  user : string;  (** user *)
  pass : Sha256.t;  (** encrypted password *)
  mail : string;
  last : float;  (** last login *)
}

(* See: https://github.com/janestreet/ppx_yojson_conv/issues/10 *)
type _t = { _user : string; _pass : string; _mail : string; _last : float }
[@@deriving yojson]

let to_json { user; pass; mail; last } =
  yojson_of__t
    { _user = user; _pass = Sha256.to_hex pass; _mail = mail; _last = last }

let of_json json =
  let { _user; _pass; _mail; _last } = _t_of_yojson json in
  { user = _user; pass = Sha256.of_hex _pass; mail = _mail; last = _last }

let db = DB.create ~of_json ~to_json "users"

(** Find user with given username. *)
let find_opt user = DB.find_opt db user

(** Test whether the user/pass combination is valid. Register it if the user
    does not already exist. *)
let valid_or_register ~user ~pass ~mail =
  let pass = Sha256.string pass in
  match DB.find_opt db user with
    | Some u ->
        if Sha256.equal pass u.pass then (
          DB.add db user { u with last = Unix.time () };
          true)
        else false
    | None ->
        let u = { user; pass; mail; last = Unix.time () } in
        DB.add db user u;
        true
