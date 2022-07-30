(** Users. *)

(* See: https://github.com/janestreet/ppx_yojson_conv/issues/10 *)
module Wrapped = struct
  type t = {
    user : string;  (** user *)
    pass : string;  (** encrypted password *)
    email : string;
    last : float;  (** last login *)
  }
  [@@deriving yojson]
end

(** An user. *)
type t = {
  user : string;  (** user *)
  pass : Sha256.t;  (** encrypted password *)
  email : string;
  last : float;  (** last login *)
}
[@@deriving stable_record ~version:Wrapped.t ~modify:[pass]]

let to_json user =
  Wrapped.yojson_of_t (to_Wrapped_t user ~modify_pass:Sha256.to_hex)

let of_json json =
  of_Wrapped_t (Wrapped.t_of_yojson json) ~modify_pass:Sha256.of_hex

let db = DB.create ~of_json ~to_json "users"

(** Find user with given username. *)
let find_opt user = DB.find_opt db user

(** Test whether the user/pass combination is valid. Register it if the user
    does not already exist. *)
let valid_or_register ~user ~pass ~email =
  let pass = Sha256.string pass in
  match DB.find_opt db user with
    | Some u ->
        if Sha256.equal pass u.pass then (
          DB.add db user { u with last = Unix.time () };
          true)
        else false
    | None ->
        let u = { user; pass; email; last = Unix.time () } in
        DB.add db user u;
        true
