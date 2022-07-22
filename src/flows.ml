open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix
open Extlib

let port = 8080

exception Invalid_password of string
exception Invalid_radio of string
exception Missing_parameter of string
exception Invalid_parameter of string

let server =
  let callback conn req body =
    let ip =
      match fst conn with
      | Conduit_lwt_unix.TCP tcp -> Ipaddr.to_string tcp.ip
      | _ -> assert false
    in
    let uri = req |> Request.uri in
    let path = Uri.path uri in
    let query =
      Uri.query uri
      |> List.filter_map (fun (k, v) ->
          match v with [v] -> Some (k, v) | _ -> None)
    in
    let meth = req |> Request.meth in
    let _headers = req |> Request.headers |> Header.to_list in

    Printf.printf "Serving %s %s for %s.\n%!"
      (Code.string_of_method meth)
      path ip;
    let* body = Cohttp_lwt.Body.to_string body in
    Printf.printf "  body: %s\n%!" body;

    Printf.printf "  query: %s\n%!"
      (query |> List.map (fun (k, v) -> k ^ "=" ^ v) |> String.concat "&");

    let ok () = Server.respond_string ~status:`OK ~body:"Done." () in
    try
      match path with
      | "/" ->
        (
          match meth with
          | `POST ->
            (
              match JSON.of_string body with
              | `Assoc params ->
                let get_param_opt k = List.assoc_opt k params in
                let get_param_string k =
                  match get_param_opt k with
                  | Some p -> JSON.string p
                  | None -> raise (Missing_parameter k)
                in
                let user =
                  match get_param_opt "user" with
                  | Some (`String u) -> Some u
                  | Some `Null -> None
                  | Some _ -> raise (Invalid_parameter "user")
                  | None -> None
                in
                if user <> None then
                  (
                    let user = Option.get user in
                    let pass = get_param_string "password" in
                    let mail = get_param_string "mail" in
                    if not (User.valid_or_register ~user ~pass ~mail) then
                      raise (Invalid_password user)
                  );
                let get_radio () =
                  let radio = get_param_string "radio" in
                  match Radio.find_opt ~user ~radio with
                  | Some r -> r
                  | None -> raise (Invalid_radio radio)
                in
                let command = get_param_string "command" in
                (
                  match command with
                  | "ping radio" ->
                    let radio = get_radio () in
                    Radio.ping radio;
                    Server.respond_string ~status:`OK ~body:"pong radio" ()
                  | "add radio" ->
                    let name = get_param_string "radio" in
                    let website = get_param_string "website" in
                    let description = get_param_string "description" in
                    let genre = get_param_string "genre" in
                    let logo = get_param_string "logo" in
                    let geoip = GeoIP.lookup_opt ip in
                    let latitude, longitude =
                      match geoip with
                      | Some geoip -> (geoip.latitude, geoip.longitude)
                      | None -> (0., 0.)
                    in
                    let streams =
                      match get_param_opt "streams" with
                      | None -> raise (Missing_parameter "streams")
                      | Some (`List l) ->
                        List.map
                          (function
                            | `Assoc s ->
                              let format = List.assoc "format" s |> JSON.string in
                              let url = List.assoc "url" s |> JSON.string in
                              { Radio.format; url }
                            | _ -> raise (Invalid_parameter "streams")
                          ) l
                      | Some _ -> raise (Invalid_parameter "streams")
                    in
                    Radio.register ~name ~website ~user ~description ~genre ~logo ~longitude ~latitude ~streams ();
                    ok ()
                  | "metadata" ->
                    let radio = get_radio () in
                    let artist = get_param_string "artist" in
                    let title = get_param_string "title" in
                    Radio.set_metadata radio ~artist ~title;
                    ok ()
                  | _ ->
                    Server.respond_string ~status:(`Code 400)
                      ~body:(Printf.sprintf "Invalid command: %s." command)
                      ()
                )
              | _ -> failwith "Invalid JSON."
            )
          | `GET ->
            let h = HTML.create ~css:"flows.css" ~title:"Liquidsoap radios" () in
            HTML.h1 h "Liquidsoap radios";
            let radios =
              Radio.to_list ()
              (* |> List.sort (fun (_,r) (_,r') -> int_of_float (r'.Radio.last -. r.Radio.last)) *)
              |> List.shuffle
            in
            HTML.w h {|
<script type="text/javascript">
function play(stream)
{ 
  player = document.getElementById("player");
  player.pause();
  player.setAttribute('src', stream);
  player.load();
  player.play();
}
</script>
<div class="player">
<audio id="player" controls><source src="" type="audio/mpeg"></audio>
</div>
|};
            (* Nice *)
            HTML.h2 h "Your radios";
            List.iter
              (fun (_,(r:Radio.t)) ->
                 let logo = if r.logo = "" then "https://picsum.photos/id/1/250/250/" else r.logo in
                 let stream = if r.streams = [] then "" else (List.hd r.streams).url in
                 HTML.w ~nl:false h {|
<div class="radio">
<img src="%s" onclick='play("%s");'>
<div class="name">%s</div>
<div class="artist">%s</div>
<div class="title">%s</div>
</div>
|} logo stream r.name r.artist r.title
              ) radios;
            (* Basic *)
            HTML.h2 h "Details about streams";
            HTML.ul h
              (List.map
                 (fun (_,r) ->
                    let streams = List.map (fun s -> Printf.sprintf "📻 <a href=\"%s\">%s</a>" s.Radio.url s.format) r.Radio.streams |> String.concat ", " in
                    Printf.sprintf "<a href=\"%s\">%s</a>: %s (%s)<br/>▶ <em>%s</em> by %s" r.Radio.website r.name r.description streams r.title r.artist
                 ) radios);
            (* Map *)
            HTML.h2 h "The map";
            HTML.w h {|
<link rel="stylesheet" href="https://cdn.rawgit.com/openlayers/openlayers.github.io/master/en/v5.3.0/css/ol.css" type="text/css">
<script src="https://cdn.rawgit.com/openlayers/openlayers.github.io/master/en/v5.3.0/build/ol.js"></script>
<div id="map" style="width: 600px; height: 100%%;"></div>
<script>
 var attribution = new ol.control.Attribution({
     collapsible: false
 });

 var map = new ol.Map({
     controls: ol.control.defaults({attribution: false}).extend([attribution]),
     layers: [
         new ol.layer.Tile({
             source: new ol.source.OSM({
                 url: 'https://tile.openstreetmap.be/osmbe/{z}/{x}/{y}.png',
                 attributions: [ ol.source.OSM.ATTRIBUTION, 'Tiles courtesy of <a href="https://geo6.be/">GEO-6</a>' ],
                 maxZoom: 18
             })
         })
     ],
     target: 'map',
     view: new ol.View({
         center: ol.proj.fromLonLat([4.35247, 50.84673]),
         maxZoom: 18,
         zoom: 12
     })
 });
var layer = new ol.layer.Vector({
     source: new ol.source.Vector({
         features: [
             new ol.Feature({
                 geometry: new ol.geom.Point(ol.proj.fromLonLat([4.35247, 50.84673]))
             })
         ]
     })
 });
 map.addLayer(layer);
</script>
|};
            Server.respond_string ~status:`OK ~body:(HTML.to_string h) ()
          | _ -> failwith "Invalid method."
        )
      | "/flows.css" -> Server.respond_file ~fname:"flows.css" ()
      | "/radios" ->
        let body = Radio.all_to_json () in
        Server.respond_string ~status:`OK ~body ()
      | _ ->
        Server.respond_string ~status:(`Code 404)  ~body:(Printf.sprintf "Don't know how to serve %s." path) ()
    with
    | Invalid_password u ->
      Server.respond_string ~status:(`Code 401)
        ~body:(Printf.sprintf "Invalid password for user: %s." u)
        ()
    | Missing_parameter p ->
      Server.respond_string ~status:(`Code 400)
        ~body:(Printf.sprintf "Missing parameter: %s." p)
        ()
    | Invalid_parameter p ->
      Server.respond_string ~status:(`Code 400)
        ~body:(Printf.sprintf "Invalid parameter: %s." p)
        ()
    | Invalid_radio r ->
      Server.respond_string ~status:(`Code 400)
        ~body:(Printf.sprintf "Unknown radio: %s." r)
        ()
    | Failure s ->
      Server.respond_string ~status:(`Code 500)
        ~body:(Printf.sprintf "Failure: %s." s)
        ()
    | e ->
      Server.respond_string ~status:(`Code 500)
        ~body:
          (Printf.sprintf "Unexpected error: %s."
             (Printexc.to_string e))
        ()
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let () =
  Printexc.record_backtrace true;
  Lwt_main.run server
