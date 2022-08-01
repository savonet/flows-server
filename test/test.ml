open Flows

let run_tests db =
  let%lwt () = Db.setup ~db () in
  let%lwt user =
    User.create ~email:"foo@bar.com" ~last_sign_in_at:(Unix.time ()) ~db
      ~name:"Foo" ~password:"bla" ()
  in
  let%lwt _ = User.update_last_sign_at ~db user in
  let%lwt user =
    match%lwt User.find ~db "Foo" with
      | Some user -> Lwt.return user
      | None -> assert false
  in
  let%lwt () =
    match%lwt User.find ~db ~email:"gni" "Foo" with
      | Some _ -> assert false
      | None -> Lwt.return ()
  in
  let%lwt () =
    match%lwt
      User.valid_or_register ~db ~email:"foo@bar.com" ~user:"Foo"
        ~password:"bla" ()
    with
      | Some u ->
          assert (u.User.id = user.User.id);
          Lwt.return ()
      | None -> assert false
  in
  assert (user.User.name = "Foo");
  assert (user.User.email = Some "foo@bar.com");
  assert (Sha256.equal user.User.password (Sha256.string "bla"));

  let streams =
    [
      { Radio.format = "bla"; url = "gni" };
      { Radio.format = "blo"; url = "gno" };
    ]
  in

  let%lwt radio = Radio.create ~name:"Some radio" ~db ~streams ~user () in

  let%lwt radio =
    Radio.update ~db
      { radio with title = Some "Some title"; artist = Some "Some artist" }
  in

  let id = radio.Radio.id in
  let%lwt radio = Radio.fetch ~db id in
  assert (radio.Radio.id = id);
  assert (radio.Radio.artist = Some "Some artist");
  assert (radio.Radio.title = Some "Some title");

  let%lwt radio = Radio.find ~db ~user "Some radio" in
  let radio = Option.get radio in
  assert (radio.Radio.id = id);

  let%lwt count = Radio.count ~db () in
  assert (count = 1);

  let%lwt page = Radio.get_page ~page:1 ~pp:10 ~db () in
  assert ((List.hd page).Radio.id = id);

  Printf.printf "Created radio: %s\n%!" radio.Radio.name;
  Lwt.return ()

let runner () =
  let%lwt db = Db.db () in
  let%lwt db = Pgx_lwt_unix.begin_work db in
  let finalize () =
    let%lwt () = Pgx_lwt_unix.rollback db in
    Pgx_lwt_unix.close db
  in
  (run_tests db) [%lwt.finally finalize ()]

let () = Lwt_main.run (runner ())
