include Commands_base

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
  let user =
    match params.get_opt "user" with
      | Some (`String u) -> u
      | _ -> raise (Invalid_parameter "user")
  in
  let password = params.string "password" in
  let email = params.string_opt "email" in
  let radio = params.string "radio" in
  let get_user ~db () =
    match User.valid_or_register ~db ~user ~password ?email () with
      | Some user -> user
      | None -> raise (Invalid_password user)
  in
  let get_radio ~db () =
    let user = get_user ~db () in
    match Radio.find ~db ~user radio with
      | Some r -> r
      | None -> raise (Invalid_radio radio)
  in
  let command = params.string "command" in
  match command with
    | "ping radio" ->
        Db.transaction (fun db ->
            let radio = get_radio ~db () in
            Radio.ping ~db radio);
        respond_string ~status:`OK ~body:"pong radio" ()
    | "add radio" ->
        ignore (Add_radio.exec ~get_user ~params ~ip ());
        ok ()
    | "metadata" ->
        Db.transaction (fun db ->
            let radio = get_radio ~db () in
            let artist = Some (params.string "artist") in
            let title = Some (params.string "title") in
            Radio.set_metadata radio ~db ~artist ~title);
        ok ()
    | _ ->
        respond_string ~status:(`Code 400)
          ~body:(Printf.sprintf "Invalid command: %s." command)
          ()
