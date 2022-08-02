open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix
open Flows
open Commands

let port =
  Option.value ~default:8080 (Option.map int_of_string (Sys.getenv_opt "PORT"))

let cors_headers =
  Header.of_list
    [
      ("Access-Control-Allow-Origin", "*");
      ("Access-Control-Allow-Methods", "GET, PUT, POST, OPTIONS");
      ("Access-Control-Allow-Headers", "Content-Type");
    ]

let respond_string ~status ~body () =
  Server.respond_string ~headers:cors_headers ~status ~body ()

type 'a page = { pp : int; page : int; total : int; data : 'a list }
[@@deriving yojson]

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

    try
      match path with
        | "/" -> (
            match meth with
              | `POST -> (
                  match JSON.of_string body with
                    | `Assoc params ->
                        Commands.exec ~respond_string ~params ~ip ()
                    | _ -> failwith "Invalid JSON.")
              | _ -> failwith "Not implemented")
        | "/radios" ->
            let pp = try int_of_string (List.assoc "pp" query) with _ -> 10 in
            let page =
              try int_of_string (List.assoc "page" query) with _ -> 1
            in
            let total, data =
              Db.transaction (fun db ->
                  let count = Radio.count ~db () in
                  let data = Radio.get_page ~db ~pp ~page () in
                  (count / pp, data))
            in
            let body = yojson_of_page Radio.to_json { pp; page; total; data } in
            respond_string ~status:`OK
              ~body:(JSON.to_string ~pretty:true body)
              ()
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
  Db.setup ();
  Lwt_main.run server
