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

let streams_query = "SELECT format, url FROM stream WHERE radio_id = $1"

let get_streams ~db id =
  let%lwt streams =
    Pgx_lwt_unix.execute ~params:[Pgx.Value.of_int id] db streams_query
  in
  Lwt_list.map_p
    (function
      | [format; url] ->
          Lwt.return
            {
              format = Pgx.Value.to_string_exn format;
              url = Pgx.Value.to_string_exn url;
            }
      | _ -> assert false)
    streams

let populate ~db = function
  | [
      id;
      user_id;
      website;
      name;
      logo;
      description;
      genre;
      longitude;
      latitude;
      artist;
      title;
      updated_at;
      created_at;
    ] ->
      let id = Pgx.Value.to_int_exn id in
      let%lwt streams = get_streams ~db id in
      let%lwt user = User.fetch ~db (Pgx.Value.to_int_exn user_id) in
      Lwt.return
        {
          id;
          user;
          website = Pgx.Value.to_string website;
          name = Pgx.Value.to_string_exn name;
          logo = Pgx.Value.to_string logo;
          description = Pgx.Value.to_string description;
          genre = Pgx.Value.to_string genre;
          longitude = Pgx.Value.to_float longitude;
          latitude = Pgx.Value.to_float latitude;
          artist = Pgx.Value.to_string artist;
          title = Pgx.Value.to_string title;
          streams;
          updated_at = Pgx.Value.to_float_exn updated_at;
          created_at = Pgx.Value.to_float_exn created_at;
        }
  | _ -> assert false

let select_query =
  "SELECT
    id,
    user_id,
    website,
    name,
    logo,
    description,
    genre,
    longitude,
    latitude,
    artist,
    title,
    extract(epoch from updated_at),
    extract(epoch from created_at),
  FROM radio
  WHERE id = $1"

let fetch ~db id =
  match%lwt
    Pgx_lwt_unix.execute ~params:[Pgx.Value.of_int id] db select_query
  with
    | [row] -> populate ~db row
    | _ -> assert false

let find_query =
  "SELECT
    id,
    user_id,
    website,
    name,
    logo,
    description,
    genre,
    longitude,
    latitude,
    artist,
    title,
    extract(epoch from updated_at),
    extract(epoch from created_at),
  FROM radio
  WHERE name = $1
  AND user_id = $2"

let find ~db ~user name =
  match%lwt
    Pgx_lwt_unix.execute
      ~params:Pgx.Value.[of_string name; of_int user.User.id]
      db find_query
  with
    | row :: _ ->
        let%lwt radio = populate ~db row in
        Lwt.return (Some radio)
    | _ -> Lwt.return None

let sync_streams ~db ~streams id =
  let%lwt () =
    Pgx_lwt_unix.execute_unit ~params:[Pgx.Value.of_int id] db
      "DELETE FROM stream WHERE radio_id = $1"
  in
  let created_at = Unix.time () in
  Lwt_list.iter_s
    (fun { format; url } ->
      Pgx_lwt_unix.execute_unit
        ~params:
          Pgx.Value.
            [
              of_int id;
              of_string format;
              of_string url;
              of_float created_at;
              of_float created_at;
            ]
        db
        "INSERT INTO stream (radio_id, format, url, updated_at, created_at)
         VALUES ($1, $2, to_timestamp($3), to_timestamp($4))")
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
     updated_at = to_timestamp($12),
   WHERE id = $13"

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
  let updated_at = Unix.time () in
  let params =
    Pgx.Value.
      [
        of_string name;
        of_int user.User.id;
        opt of_string website;
        opt of_string description;
        opt of_string genre;
        opt of_string logo;
        opt of_float longitude;
        opt of_float latitude;
        opt of_string artist;
        opt of_string title;
        of_float created_at;
        of_float updated_at;
        of_int id;
      ]
  in
  let%lwt () = Pgx_lwt_unix.execute_unit ~params db update_query in
  let%lwt () = sync_streams ~db ~streams id in
  Lwt.return { radio with updated_at }

let insert_query =
  "INSERT INTO
      radio (name, user_id, website, description, genre, logo, longitude, latitude, artist, title, created_at,        updated_at)
   VALUES   ($1,   $2,      $3,      $4,          $5,    $6,   $7,        $8,       $9,     $10,   to_timestamp($11), to_timestamp($12))
   RETURNING id"

let create ?website ?description ?genre ?logo ?longitude ?latitude ?artist
    ?title ~db ~streams ~name ~user () =
  let created_at = Unix.time () in
  let params =
    Pgx.Value.
      [
        of_string name;
        of_int user.User.id;
        opt of_string website;
        opt of_string description;
        opt of_string genre;
        opt of_string logo;
        opt of_float longitude;
        opt of_float latitude;
        opt of_string artist;
        opt of_string title;
        of_float created_at;
        of_float created_at;
      ]
  in
  match%lwt Pgx_lwt_unix.execute ~params db insert_query with
    | [[id]] ->
        let id = Pgx.Value.to_int_exn id in
        let%lwt () = sync_streams ~db ~streams id in
        Lwt.return
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

let ping ~db r =
  let%lwt _ = update ~db { r with updated_at = Unix.time () } in
  Lwt.return ()

(** Set metadata of the currently playing title. *)
let set_metadata ~db r ~artist ~title =
  let%lwt _ = update ~db { r with artist; title } in
  Lwt.return ()

let count ~db () =
  match%lwt
    Pgx_lwt_unix.execute ~params:[] db "SELECT COUNT(id) FROM radio"
  with
    | [[count]] -> Lwt.return (Pgx.Value.to_int_exn count)
    | _ -> assert false

let page_query =
  "SELECT
    id,
    user_id,
    website,
    name,
    logo,
    description,
    genre,
    longitude,
    latitude,
    artist,
    title,
    updated_at,
    created_at
  FROM radio
  OFFSET $1
  LIMIT $2"

(* Get one page of result. *)
let get_page ~db ~page ~pp () =
  let params = Pgx.Value.[of_int ((page - 1) * pp); of_int pp] in
  let%lwt page = Pgx_lwt_unix.execute ~params db page_query in
  Lwt_list.map_p (populate ~db) page
