(** Operations on radios. *)

open Utils

(** A stream. *)
type stream = { format : string; url : string } [@@deriving yojson]

(* Public radio payload *)
module Public = struct
  type t = {
    id : int;
    name : string;
    website : string option;
    description : string option;
    genre : string option;
    logo : string option;
    longitude : float option;
    latitude : float option;
    artist : string option;
    title : string option;
    streams : stream list;
  }
  [@@deriving yojson]
end

(** A radio. *)
type t = {
  id : int;
  name : string;
  user : User.t;
  website : string option;
  description : string option;
  genre : string option;
  logo : string option;  (** url of the logo *)
  longitude : float option;
  latitude : float option;
  artist : string option;
  title : string option;
  streams : stream list;
  created_at : float;
  updated_at : float;
}
[@@deriving
  stable_record ~version:Public.t ~remove:[user; created_at; updated_at]]

let streams_query = "SELECT * FROM stream WHERE radio_id = $1"

let get_streams ~(db : Db.db) id =
  List.map
    (fun { string; _ } -> { format = string "format"; url = string "url" })
    (list_of_result
       (db#exec ~expect:[Postgresql.Tuples_ok]
          ~params:[| string_of_int id |]
          streams_query))

let populate ~(db : Db.db) { int; string; string_opt; float; float_opt; _ } =
  let id = int "id" in
  let streams = get_streams ~db id in
  let user = User.fetch ~db (int "user_id") in
  {
    id;
    user;
    website = string_opt "website";
    name = string "name";
    logo = string_opt "logo";
    description = string_opt "description";
    genre = string_opt "genre";
    longitude = float_opt "longitude";
    latitude = float_opt "latitude";
    artist = string_opt "artist";
    title = string_opt "title";
    streams;
    updated_at = float "updated_at_epoch";
    created_at = float "created_at_epoch";
  }

let select_query =
  "SELECT
    *,
    extract(epoch from updated_at) AS updated_at_epoch,
    extract(epoch from created_at) AS created_at_epoch
  FROM radio
  WHERE id = $1"

let fetch ~(db : Db.db) id =
  match
    list_of_result
      (db#exec ~expect:[Postgresql.Tuples_ok]
         ~params:[| string_of_int id |]
         select_query)
  with
    | [row] -> populate ~db row
    | _ -> assert false

let find_query =
  "SELECT
    *,
    extract(epoch from updated_at) AS updated_at_epoch,
    extract(epoch from created_at) AS created_at_epoch
  FROM radio
  WHERE name = $1
  AND user_id = $2"

let find ~(db : Db.db) ~user name =
  match
    list_of_result
      (db#exec
         ~expect:[Postgresql.Tuples_ok; Postgresql.Command_ok]
         ~params:[| name; string_of_int user.User.id |]
         find_query)
  with
    | row :: _ -> Some (populate ~db row)
    | _ -> None

let sync_streams ~(db : Db.db) ~streams id =
  ignore
    (db#exec ~expect:[Postgresql.Command_ok]
       ~params:[| string_of_int id |]
       "DELETE FROM stream WHERE radio_id = $1");
  let created_at = Unix.time () in
  List.iter
    (fun { format; url } ->
      ignore
        (db#exec ~expect:[Postgresql.Command_ok]
           ~params:
             [|
               string_of_int id;
               format;
               url;
               string_of_float created_at;
               string_of_float created_at;
             |]
           "INSERT INTO stream (radio_id, format, url, updated_at, created_at)
         VALUES ($1, $2, $3, to_timestamp($4), to_timestamp($5))"))
    streams

let update_query =
  "UPDATE
     radio
   SET
     name = $1,
     user_id = $2,
     website = $3,
     description = $4,
     genre = $5,
     logo = $6,
     longitude = $7,
     latitude = $8,
     artist = $9,
     title = $10,
     created_at = to_timestamp($11),
     updated_at = to_timestamp($12)
   WHERE id = $13"

let update ~(db : Db.db) radio =
  let {
    id;
    user;
    name;
    website;
    description;
    genre;
    logo;
    longitude;
    latitude;
    artist;
    title;
    streams;
    created_at;
    _;
  } =
    radio
  in
  let updated_at = Unix.time () in
  let params =
    [|
      name;
      string_of_int user.User.id;
      opt website;
      opt description;
      opt genre;
      opt logo;
      opt (Option.map string_of_float longitude);
      opt (Option.map string_of_float latitude);
      opt artist;
      opt title;
      string_of_float created_at;
      string_of_float updated_at;
      string_of_int id;
    |]
  in
  ignore (db#exec ~expect:[Postgresql.Command_ok] ~params update_query);
  sync_streams ~db ~streams id;
  { radio with updated_at }

let insert_query =
  "INSERT INTO
      radio (name, user_id, website, description, genre, logo, longitude, latitude, artist, title, created_at,        updated_at)
   VALUES   ($1,   $2,      $3,      $4,          $5,    $6,   $7,        $8,       $9,     $10,   to_timestamp($11), to_timestamp($12))
   RETURNING id"

let create ?website ?description ?genre ?logo ?longitude ?latitude ?artist
    ?title ~(db : Db.db) ~streams ~name ~user () =
  let created_at = Unix.time () in
  let params =
    [|
      name;
      string_of_int user.User.id;
      opt website;
      opt description;
      opt genre;
      opt logo;
      opt (Option.map string_of_float longitude);
      opt (Option.map string_of_float latitude);
      opt artist;
      opt title;
      string_of_float created_at;
      string_of_float created_at;
    |]
  in
  match
    list_of_result (db#exec ~expect:[Postgresql.Tuples_ok] ~params insert_query)
  with
    | [{ int; _ }] ->
        let id = int "id" in
        sync_streams ~db ~streams id;
        {
          id;
          user;
          name;
          website;
          description;
          genre;
          logo;
          longitude;
          latitude;
          artist;
          title;
          streams;
          updated_at = created_at;
          created_at;
        }
    | _ -> assert false

let to_json r = [%yojson_of: Public.t] (to_Public_t r)
let ping ~db r = ignore (update ~db { r with updated_at = Unix.time () })

(** Set metadata of the currently playing title. *)
let set_metadata ~db r ~artist ~title =
  ignore (update ~db { r with artist; title })

let count ~(db : Db.db) () =
  match
    list_of_result
      (db#exec ~expect:[Postgresql.Tuples_ok]
         "SELECT COUNT(id) AS count FROM radio")
  with
    | [{ int; _ }] -> int "count"
    | _ -> assert false

let page_query =
  "SELECT
    *,
    extract(epoch from updated_at) AS updated_at_epoch,
    extract(epoch from created_at) AS created_at_epoch
  FROM radio
  WHERE updated_at > NOW() - interval '1 day'
  OFFSET $1
  LIMIT $2"

(* Get one page of result. *)
let get_page ~(db : Db.db) ~page ~pp () =
  List.map (populate ~db)
    (list_of_result
       (db#exec ~expect:[Postgresql.Tuples_ok]
          ~params:[| string_of_int ((page - 1) * pp); string_of_int pp |]
          page_query))
