include Commands_base

let exec ~respond_string ~params ~ip () =
  let ok () = respond_string ~status:`OK ~body:"Done." () in
  let get_param_opt k = List.assoc_opt k params in
  let get_param_string_opt k =
    match get_param_opt k with Some p -> Some (JSON.string p) | None -> None
  in
  let get_param_string k =
    match get_param_opt k with
      | Some p -> JSON.string p
      | None -> raise (Missing_parameter k)
  in
  let user =
    match get_param_opt "user" with
      | Some (`String u) -> u
      | _ -> raise (Invalid_parameter "user")
  in
  let password = get_param_string "password" in
  let email = get_param_string_opt "email" in
  let radio = get_param_string "radio" in
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
  let command = get_param_string "command" in
  match command with
    | "ping radio" ->
        Db.transaction (fun db ->
            let radio = get_radio ~db () in
            Radio.ping ~db radio);
        respond_string ~status:`OK ~body:"pong radio" ()
    | "add radio" ->
        Add_radio.exec ~get_user ~get_param_opt ~get_param_string_opt
          ~get_param_string ~ip ();
        ok ()
    | "metadata" ->
        Db.transaction (fun db ->
            let radio = get_radio ~db () in
            let artist = Some (get_param_string "artist") in
            let title = Some (get_param_string "title") in
            Radio.set_metadata radio ~db ~artist ~title);
        ok ()
    | _ ->
        respond_string ~status:(`Code 400)
          ~body:(Printf.sprintf "Invalid command: %s." command)
          ()
