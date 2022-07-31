(** Operations on radios. *)

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

let populate ~db radio =
  let id = Int32.of_int radio#id in
  let streams =
    List.map
      (fun (format, url) -> { format; url })
      [%pgsql db "SELECT format, url FROM stream WHERE radio_id = $id"]
  in
  let user = User.fetch ~db (Int32.to_int radio#user_id) in
  {
    id = radio#id;
    user;
    website = radio#website;
    name = radio#name;
    logo = radio#logo;
    description = radio#description;
    genre = radio#genre;
    longitude = radio#longitude;
    latitude = radio#latitude;
    artist = radio#artist;
    title = radio#title;
    streams;
    updated_at = radio#updated_at;
    created_at = radio#created_at;
  }

let fetch ~db id =
  let id = Int32.of_int id in
  populate ~db
    (List.hd
       [%pgsql.object
         db "load_custom_from=src/db_config.sexp" "show=pp"
           "SELECT * FROM radio WHERE id = $id"])

let find ~db ~user name =
  let user_id = Int32.of_int user.User.id in
  let radio =
    [%pgsql.object
      db "load_custom_from=src/db_config.sexp" "show=pp"
        "SELECT * FROM radio WHERE name = $name AND user_id = $user_id"]
  in
  match radio with [] -> None | radio :: _ -> Some (populate ~db radio)

let sync_streams ~db ~streams id =
  let updated_at = Unix.time () in
  let () = [%pgsql db "DELETE FROM stream WHERE radio_id = $id"] in
  List.iter
    (fun { format; url } ->
      [%pgsql
        db "load_custom_from=src/db_config.sexp" "show=pp"
          "INSERT INTO
       stream (radio_id, format, url, updated_at, created_at)
     VALUES ($id, $format, $url, $updated_at, $updated_at)"])
    streams

let update ~db radio =
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
  let id = Int32.of_int id in
  let user_id = Int32.of_int user.id in
  let updated_at = Unix.time () in
  let () =
    [%pgsql
      db "load_custom_from=src/db_config.sexp" "show=pp"
        "UPDATE
       radio
     SET
       name = $name,
       user_id = $user_id,
       website = $?website,
       description = $?description,
       genre = $?genre,
       logo = $?logo,
       longitude = $?longitude,
       latitude = $?latitude,
       artist = $?artist,
       title = $?title,
       created_at = $created_at,
       updated_at = $updated_at
     WHERE id = $id"]
  in
  sync_streams ~db ~streams id;
  { radio with updated_at }

let create ?website ?description ?genre ?logo ?longitude ?latitude ?artist
    ?title ~db ~streams ~name ~user () =
  let created_at = Unix.time () in
  let user_id = Int32.of_int user.User.id in
  let id =
    List.hd
      [%pgsql
        db "load_custom_from=src/db_config.sexp" "show=pp"
          "INSERT INTO
  radio (name, user_id, website, description, genre, logo, longitude, latitude, artist, title, created_at, updated_at)
 VALUES ($name, $user_id, $?website, $?description, $?genre, $?logo, $?longitude, $?latitude, $?artist, $?title, $created_at, $created_at)
 RETURNING id"]
  in
  sync_streams ~db ~streams (Int32.of_int id);
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

let to_json r = [%yojson_of: Public.t] (to_Public_t r)
let ping ~db r = ignore (update ~db { r with updated_at = Unix.time () })

(** Set metadata of the currently playing title. *)
let set_metadata ~db r ~artist ~title =
  ignore (update ~db { r with artist; title })

let count ~db () =
  Int64.to_int
    (Option.get
       (List.hd
          [%pgsql
            db "load_custom_from=src/db_config.sexp" "show=pp"
              "SELECT COUNT(id) FROM radio"]))

(* Get one page of result. *)
let get_page ~db ~page ~pp () =
  let offset = Int64.of_int ((page - 1) * pp) in
  let limit = Int64.of_int pp in
  List.map (populate ~db)
    [%pgsql.object
      db "load_custom_from=src/db_config.sexp" "show=pp"
        "SELECT * FROM radio OFFSET $offset LIMIT $limit"]
