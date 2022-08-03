open Commands_base

let exec ~get_user ~params ~ip () =
  let radio = [%of_yojson: Radio.Create.t] (params.get "payload") in
  let geoip = GeoIP.lookup_opt ip in
  let latitude, longitude =
    match geoip with
      | Some geoip -> (Some geoip.latitude, Some geoip.longitude)
      | None -> (None, None)
  in
  let radio = { radio with Radio.Create.latitude; longitude } in
  Db.transaction (fun db ->
      let user = get_user ~db () in
      Radio.create_or_update ~user ~db radio)
