type stream =
  {
    format : string;
    url : string;
  }

type t = {
  name : string;
  user : string;
  website : string;
  description : string;
  genre : string;
  artist : string;
  title : string;
  streams : stream list;
  last : float; (** last update *)
}

let to_json r =
  `Assoc [
    "name", `String r.name;
    "user", `String r.user;
    "website", `String r.website;
    "description", `String r.description;
    "genre", `String r.genre;
    "artist", `String r.artist;
    "title", `String r.title;
    "streams", `List (List.map (fun s -> `Assoc ["format", `String s.format; "url", `String s.url]) r.streams);
    "last", `Float r.last
  ]

let of_json = function
  | `Assoc l ->
    let streams = function
      | `List l ->
        List.map
          (function
            | `Assoc l ->
              {
                format = List.assoc "format" l |> JSON.string;
                url = List.assoc "url" l |> JSON.string
              }
            | _ -> assert false
          ) l
      | _ -> assert false
    in
    {
      name = List.assoc "name" l |> JSON.string;
      user = List.assoc "user" l |> JSON.string;
      website = List.assoc "website" l |> JSON.string;
      description = List.assoc "description" l |> JSON.string;
      genre = List.assoc "genre" l |> JSON.string;
      artist = List.assoc "artist" l |> JSON.string;
      title = List.assoc "title" l |> JSON.string;
      streams = List.assoc "streams" l |> streams;
      last = List.assoc "last" l |> JSON.float
    }
  | _ -> assert false

let db = DB.create ~to_json ~of_json "radios"

let id ~radio ~user = user ^ "/" ^ radio

let register ~name ~user ~website ~description ~genre =
  let last = Unix.time () in
  let r = { name; user; website; description; genre; artist = "?"; title = "?"; streams = []; last } in
  DB.add db (id ~radio:name ~user) r

let find_opt ~user ~radio =
  DB.find_opt db (id ~user ~radio)

let set r =
  let radio_id r = id ~radio:r.name ~user:r.user in
  DB.add db (radio_id r) r

let ping r =
  let r = {r with last = Unix.time ()} in
  set r

let clear_streams r =
  set {r with streams = []}

let add_stream r ~format ~url =
  set {r with streams = {format;url}::r.streams}

let set_metadata r ~artist ~title =
  set {r with artist; title}

let all_to_json () =
  db
  |> DB.to_seq
  |> Seq.map (fun (id,r) ->
      (* Not using the default JSON exporter here, since we don't want to
         mistakenly leak private data. *)
      let streams = `List (List.map (fun s -> `Assoc ["format", `String s.format; "url", `String s.url]) r.streams) in
      `Assoc
        [
          "token", `String id;
          "name", `String r.name;
          "website", `String r.website;
          "description", `String r.description;
          "genre", `String r.genre;
          "streams", streams;
          "artist", `String r.artist;
          "title", `String r.title
        ]
    )
  |> List.of_seq
  |> fun l -> `List l
  |> JSON.to_string ~pretty:true
