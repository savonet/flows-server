open Flows

let run_tests db =
  Db.setup ~db ();
  let user =
    User.create ~email:"foo@bar.com" ~last_sign_in_at:(Unix.time ()) ~db
      ~password:"bla" ()
  in
  let _ = User.update_last_sign_at ~db user in
  let () =
    match User.find ~db "gni" with Some _ -> assert false | None -> ()
  in
  let () =
    match
      User.valid_or_register ~db ~email:"foo@bar.com" ~password:"bla" ()
    with
      | Some u ->
          assert (u.User.id = user.User.id);
          ()
      | None -> assert false
  in
  assert (user.User.email = "foo@bar.com");
  assert (Sha256.equal user.User.password (Sha256.string "bla"));

  let count = Radio.count ~db () in

  let streams =
    [
      { Radio.format = "bla"; url = "gni" };
      { Radio.format = "blo"; url = "gno" };
    ]
  in

  let radio =
    Radio.create ~db ~user
      {
        Radio.Create.name = "Some radio";
        website = None;
        logo = None;
        latitude = Some 30.;
        longitude = Some (-90.);
        genre = None;
        description = None;
        streams;
      }
  in

  let radio =
    Radio.update ~db
      { radio with title = Some "Some title"; artist = Some "Some artist" }
  in

  let id = radio.Radio.id in
  let radio = Radio.fetch ~db id in
  assert (radio.Radio.id = id);
  assert (radio.Radio.artist = Some "Some artist");
  assert (radio.Radio.title = Some "Some title");
  assert (radio.Radio.latitude = Some 30.);
  assert (radio.Radio.longitude = Some (-90.));

  let radio = Radio.find ~db ~user "Some radio" in
  let radio = Option.get radio in
  assert (radio.Radio.id = id);
  assert (radio.Radio.artist = Some "Some artist");
  assert (radio.Radio.title = Some "Some title");
  assert (radio.Radio.latitude = Some 30.);
  assert (radio.Radio.longitude = Some (-90.));

  let new_count = Radio.count ~db () in
  assert (new_count = count + 1);

  let page = Radio.get_page ~page:1 ~pp:10 ~db () in
  assert ((List.hd (List.rev page)).Radio.id = id);

  Printf.printf "Created radio: %s\n%!" radio.Radio.name

let () =
  Printexc.record_backtrace true;
  let db = Db.db () in
  ignore (db#exec "BEGIN");
  let finalize () =
    try
      ignore (db#exec "ROLLBACK");
      db#finish
    with _ -> ()
  in
  try
    run_tests db;
    finalize ()
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    finalize ();
    Printexc.raise_with_backtrace exn bt
