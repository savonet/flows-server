include Commands_base

module Ping = struct
  type payload = { name : string } [@@deriving yojson]
end

module Metadata = struct
  type payload = {
    name : string;
    artist : string option; [@yojson.option]
    title : string option; [@yojson.option]
  }
  [@@deriving yojson]
end

let exec ~respond_string ~params ~ip () =
  let ok () = respond_string ~status:`OK ~body:"Done." () in
  let params =
    {
      get = (fun k -> List.assoc k params);
      get_opt = (fun k -> List.assoc_opt k params);
      string =
        (fun k ->
          match List.assoc_opt k params with
            | Some (`String p) -> p
            | Some _ -> raise (Invalid_parameter k)
            | None -> raise (Missing_parameter k));
      string_opt =
        (fun k ->
          match List.assoc_opt k params with
            | Some (`String p) -> Some p
            | Some `Null | None -> None
            | _ -> raise (Invalid_parameter k));
    }
  in
  let password = params.string "password" in
  let email = params.string "email" in
  let get_user ~db () =
    match User.valid_or_register ~db ~password ~email () with
      | Some user -> user
      | None -> raise (Invalid_password email)
  in
  let get_radio ~db name =
    let user = get_user ~db () in
    match Radio.find ~db ~user name with
      | Some r -> r
      | None -> raise (Invalid_radio name)
  in
  let command = params.string "command" in
  match command with
    | "ping radio" ->
        Db.transaction (fun db ->
            let { Ping.name } =
              [%of_yojson: Ping.payload] (params.get "payload")
            in
            let radio = get_radio ~db name in
            Radio.ping ~db radio);
        respond_string ~status:`OK ~body:"pong radio" ()
    | "add radio" ->
        ignore (Add_radio.exec ~get_user ~params ~ip ());
        ok ()
    | "metadata" ->
        Db.transaction (fun db ->
            let { Metadata.name; artist; title } =
              [%of_yojson: Metadata.payload] (params.get "payload")
            in
            let radio = get_radio ~db name in
            Radio.set_metadata radio ~db ~artist ~title);
        ok ()
    | _ ->
        respond_string ~status:(`Code 400)
          ~body:(Printf.sprintf "Invalid command: %s." command)
          ()
