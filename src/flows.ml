open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix

let port = 8080

exception Invalid_password of string
exception Missing_parameter of string

let server =
  let callback _conn req body =
    let uri = req |> Request.uri in
    let path = Uri.path uri in
    let query = Uri.query uri |> List.filter_map (fun (k,v) -> match v with [v] -> Some (k,v) | _ -> None) in
    let meth = req |> Request.meth in
    let headers = req |> Request.headers |> Header.to_list in

    (* Answer an authorization request. *)
    let unauthorized () =
      let headers = Header.of_list ["WWW-Authenticate", Auth.string_of_challenge (`Basic "Please enter your credentials")] in
      let body = "Please authenticate!" in
      Server.respond_string ~headers ~status:(`Code 401) ~body ()
    in

    (* Ensure that we are providing valid authentication. *)
    let _authorize f =
      if not (List.mem_assoc "Authorization" headers) then unauthorized ()
      else
        let authorization = List.assoc "Authorization" headers |> Auth.credential_of_string in
        match authorization with
        | `Basic (user, pass) ->
          (
            Printf.printf "Checking authorization for %s.\n%!" user;
            (* TODO: check auth *)
            if ignore pass; true then
              (* From there we are authorized. *)
              f user
            else
              Printf.printf "Authorization failed for %s.\n%!" user; unauthorized ()
          )
        | _ -> unauthorized ()
    in
    let get_param k =
      match List.assoc_opt k query with
      | Some v -> v
      | None -> raise (Missing_parameter k)
    in

    Printf.printf "Serving %s for %s.\n%!" path (Code.string_of_method meth);
    let* body = Cohttp_lwt.Body.to_string body in Printf.printf "  body: %s\n%!" body;

    Printf.printf "  query: %s\n%!" (query |> List.map (fun (k,v) -> k ^ "=" ^ v) |> String.concat "&");

    match path with
    | "/" ->
      (
        try
          let user = get_param "user" in
          let pass = get_param "password" in
          if user <> "default" || pass <> "default" then
            if not (User.valid_or_register user pass) then raise (Invalid_password user);
          if not (List.mem_assoc "cmd" query) then Server.respond_string ~status:(`Code 400) ~body:"No command provided." () else
            let cmd = List.assoc "cmd" query in
            match cmd with
            | "ping radio" -> Server.respond_string ~status:`OK ~body:"pong radio" ()
            | "add radio" ->
              let name = get_param "radio" in
              let website = get_param "radio_website" in
              let description = get_param "radio_description" in
              let genre = get_param "genre" in
              Radio.register ~name ~website ~user ~description ~genre;
              Server.respond_string ~status:`OK ~body:"Done." ()
            | "clear streams" ->
              Server.respond_string ~status:`OK ~body:"Done." ()
            | _ -> Server.respond_string ~status:(`Code 400) ~body:(Printf.sprintf "Invalid command: %s." cmd) ()
        with
        | Invalid_password u -> Server.respond_string ~status:(`Code 401) ~body:(Printf.sprintf "Invalid password for user: %s." u) ()
        | Missing_parameter p -> Server.respond_string ~status:(`Code 400) ~body:(Printf.sprintf "Missing parameter: %s." p) ()
      )
    (* | "/radios" -> *)
    | _ -> Server.respond_string ~status:`OK ~body:(Printf.sprintf "This is the page for %s." path) ()
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let () = Lwt_main.run server
