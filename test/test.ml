open Flows

let run_tests db =
  let%lwt () = Db.setup ~db () in
  let%lwt user = User.create ~email:"foo@bar.com" ~last_sign_in_at:(Unix.time ()) ~db ~name:"Foo" ~password:"bla" () in
  let%lwt radio = Radio.create ~name:"Some radio" ~db ~streams:[] ~user () in
  Printf.printf "Created radio: %s\n%!" radio.Radio.name;
  Lwt.return ()

let runner () =
  let%lwt db =  Pgx_lwt_unix.connect () in
  let%lwt () = Pgx_lwt_unix.execute_unit ~params:[] db
    "CREATE DATABASE flows_test"
  in
  let%lwt () = Pgx_lwt_unix.close db in
  let%lwt db =  Pgx_lwt_unix.connect ~database:"flows_test" () in
  let finalize () =
    let%lwt () = Pgx_lwt_unix.close db in
    let%lwt db =  Pgx_lwt_unix.connect () in
    let%lwt () = Pgx_lwt_unix.execute_unit ~params:[] db
      "DROP DATABASE flows_test"
    in
    Pgx_lwt_unix.close db
  in
  (run_tests db) [%lwt.finally (finalize ())]

let () = Lwt_main.run (runner ())
