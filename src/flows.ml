open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix
open Extlib

let port =
  Option.value ~default:8080 (Option.map int_of_string (Sys.getenv_opt "PORT"))

exception Invalid_password of string
exception Invalid_radio of string
exception Missing_parameter of string
exception Invalid_parameter of string

let cors_headers =
  Header.of_list
    [
      ("Access-Control-Allow-Origin", "*");
      ("Access-Control-Allow-Methods", "GET, PUT, POST, OPTIONS");
      ("Access-Control-Allow-Headers", "Content-Type");
    ]

let respond_string = Server.respond_string ~headers:cors_headers

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

    let ok () = respond_string ~status:`OK ~body:"Done." () in
    try
      match path with
        | "/" -> (
            match meth with
              | `POST -> (
                  match JSON.of_string body with
                    | `Assoc params -> (
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
                        if user <> None then (
                          let user = Option.get user in
                          let pass = get_param_string "password" in
                          let mail = get_param_string "mail" in
                          if not (User.valid_or_register ~user ~pass ~mail) then
                            raise (Invalid_password user));
                        let get_radio () =
                          let radio = get_param_string "radio" in
                          match Radio.find_opt ~user ~radio with
                            | Some r -> r
                            | None -> raise (Invalid_radio radio)
                        in
                        let command = get_param_string "command" in
                        match command with
                          | "ping radio" ->
                              let radio = get_radio () in
                              Radio.ping radio;
                              respond_string ~status:`OK ~body:"pong radio" ()
                          | "add radio" ->
                              let name = get_param_string "radio" in
                              let website = get_param_string "website" in
                              let description =
                                get_param_string "description"
                              in
                              let genre = get_param_string "genre" in
                              let logo = get_param_string "logo" in
                              let geoip = GeoIP.lookup_opt ip in
                              let latitude, longitude =
                                match geoip with
                                  | Some geoip ->
                                      (geoip.latitude, geoip.longitude)
                                  | None -> (0., 0.)
                              in
                              let streams =
                                match get_param_opt "streams" with
                                  | None -> raise (Missing_parameter "streams")
                                  | Some (`List l) ->
                                      List.map
                                        (function
                                          | `Assoc s ->
                                              let format =
                                                List.assoc "format" s
                                                |> JSON.string
                                              in
                                              let url =
                                                List.assoc "url" s
                                                |> JSON.string
                                              in
                                              { Radio.format; url }
                                          | _ ->
                                              raise
                                                (Invalid_parameter "streams"))
                                        l
                                  | Some _ ->
                                      raise (Invalid_parameter "streams")
                              in
                              Radio.register ~name ~website ~user ~description
                                ~genre ~logo ~longitude ~latitude ~streams ();
                              ok ()
                          | "metadata" ->
                              let radio = get_radio () in
                              let artist = get_param_string "artist" in
                              let title = get_param_string "title" in
                              Radio.set_metadata radio ~artist ~title;
                              ok ()
                          | _ ->
                              respond_string ~status:(`Code 400)
                                ~body:
                                  (Printf.sprintf "Invalid command: %s." command)
                                ())
                    | _ -> failwith "Invalid JSON."))
        | "/radios" ->
            let body = Radio.all_to_json () in
            respond_string ~status:`OK ~body ()
        | _ ->
            respond_string ~status:(`Code 404)
              ~body:(Printf.sprintf "Don't know how to serve %s." path)
              ()
    with
      | Invalid_password u ->
          respond_string ~status:(`Code 401)
            ~body:(Printf.sprintf "Invalid password for user: %s." u)
            ()
      | Missing_parameter p ->
          respond_string ~status:(`Code 400)
            ~body:(Printf.sprintf "Missing parameter: %s." p)
            ()
      | Invalid_parameter p ->
          respond_string ~status:(`Code 400)
            ~body:(Printf.sprintf "Invalid parameter: %s." p)
            ()
      | Invalid_radio r ->
          respond_string ~status:(`Code 400)
            ~body:(Printf.sprintf "Unknown radio: %s." r)
            ()
      | Failure s ->
          respond_string ~status:(`Code 500)
            ~body:(Printf.sprintf "Failure: %s." s)
            ()
      | e ->
          respond_string ~status:(`Code 500)
            ~body:
              (Printf.sprintf "Unexpected error: %s." (Printexc.to_string e))
            ()
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let () =
  Printexc.record_backtrace true;
  Lwt_main.run server
