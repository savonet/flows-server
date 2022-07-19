open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix

let port = 8080

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

    let* body = Cohttp_lwt.Body.to_string body in
    Printf.printf "Serving %s for %s: %s\n%!" path (Code.string_of_method meth) body;

    Printf.printf "  query: %s\n%!" (query |> List.map (fun (k,v) -> k ^ "=" ^ v) |> String.concat "&");

    match path with
    | _ -> Server.respond_string ~status:`OK ~body:(Printf.sprintf "This is the page for %s." path) ()
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())


let () = Lwt_main.run server
