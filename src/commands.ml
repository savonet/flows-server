include Commands_base

let exec ~respond_string ~params ~ip () =
  let ok () = respond_string ~status:`OK ~body:"Done." () in
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
    let email = get_param_string "email" in
    if not (User.valid_or_register ~user ~pass ~email) then
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
        Add_radio.exec ~user ~get_param_opt ~get_param_string ~ip ();
        ok ()
    | "metadata" ->
        let radio = get_radio () in
        let artist = get_param_string "artist" in
        let title = get_param_string "title" in
        Radio.set_metadata radio ~artist ~title;
        ok ()
    | _ ->
        respond_string ~status:(`Code 400)
          ~body:(Printf.sprintf "Invalid command: %s." command)
          ()
