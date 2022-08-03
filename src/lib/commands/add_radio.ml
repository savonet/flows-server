open Commands_base

let exec ~get_user ~params ~ip () =
  let radio = [%of_yojson: Radio.Create.payload] (params.get "payload") in
  let geoip = GeoIP.lookup_opt ip in
  let latitude, longitude =
    match geoip with
      | Some geoip -> (Some geoip.latitude, Some geoip.longitude)
      | None -> (None, None)
  in
  let radio = Radio.Create.of_payload ~latitude ~longitude radio in
  Db.transaction (fun db ->
      let user = get_user ~db () in
      Radio.create_or_update ~user ~db radio)
