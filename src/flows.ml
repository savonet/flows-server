open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix

let port = 8080

exception Invalid_password of string
exception Invalid_radio of string
exception Missing_parameter of string

let server =
  let callback _conn req body =
    let uri = req |> Request.uri in
    let path = Uri.path uri in
    let query = Uri.query uri |> List.filter_map (fun (k,v) -> match v with [v] -> Some (k,v) | _ -> None) in
    let meth = req |> Request.meth in

    let get_param ?default k =
      match List.assoc_opt k query with
      | Some v -> v
      | None ->
        match default with
        | Some v -> v
        | None -> raise (Missing_parameter k)
    in
    let get_radio ~user ~radio =
      match Radio.find_opt ~user ~radio with
      | Some r -> r
      | None -> raise (Invalid_radio radio)
    in
    let ok () =
      Server.respond_string ~status:`OK ~body:"Done." ()
    in

    Printf.printf "Serving %s for %s.\n%!" path (Code.string_of_method meth);
    let* body = Cohttp_lwt.Body.to_string body in Printf.printf "  body: %s\n%!" body;

    Printf.printf "  query: %s\n%!" (query |> List.map (fun (k,v) -> k ^ "=" ^ v) |> String.concat "&");

    match path with
    | "/" ->
      (
        try
          let user = get_param ~default:"default" "user" in
          let pass = get_param ~default:"default" "password" in
          let get_radio () = get_radio ~user ~radio:(get_param "radio") in
          if user <> "default" || pass <> "default" then
            if not (User.valid_or_register user pass) then raise (Invalid_password user);
          if not (List.mem_assoc "cmd" query) then Server.respond_string ~status:(`Code 400) ~body:"No command provided." () else
            let cmd = List.assoc "cmd" query in
            match cmd with
            | "ping radio" ->
              let radio = get_radio () in
              Radio.ping radio;
              Server.respond_string ~status:`OK ~body:"pong radio" ()
            | "add radio" ->
              let name = get_param "radio" in
              let website = get_param "website" in
              let description = get_param "description" in
              let genre = get_param "genre" in
              Radio.register ~name ~website ~user ~description ~genre;
              ok ()
            | "clear streams" ->
              let radio = get_radio () in
              Radio.clear_streams radio;
              ok ()
            | "add stream" ->
              let radio = get_radio () in
              let format = get_param "format" in
              let url = get_param "url" in
              Radio.add_stream radio ~format ~url;
              ok ()
            | _ -> Server.respond_string ~status:(`Code 400) ~body:(Printf.sprintf "Invalid command: %s." cmd) ()
        with
        | Invalid_password u -> Server.respond_string ~status:(`Code 401) ~body:(Printf.sprintf "Invalid password for user: %s." u) ()
        | Missing_parameter p -> Server.respond_string ~status:(`Code 400) ~body:(Printf.sprintf "Missing parameter: %s." p) ()
        | Invalid_radio r -> Server.respond_string ~status:(`Code 400) ~body:(Printf.sprintf "Unknown radio: %s." r) ()
      )
    | "/radios" ->
      let body = Radio.all_to_json () in
      Server.respond_string ~status:`OK ~body ()
    | _ -> Server.respond_string ~status:`OK ~body:(Printf.sprintf "This is the page for %s." path) ()
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let () = Lwt_main.run server
