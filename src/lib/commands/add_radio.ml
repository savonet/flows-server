open Commands_base

let exec ~get_user ~params ~ip () =
  let name = params.string "radio" in
  let website = params.string_opt "website" in
  let description = params.string_opt "description" in
  let genre = params.string_opt "genre" in
  let logo = params.string_opt "logo" in
  let geoip = GeoIP.lookup_opt ip in
  let latitude, longitude =
    match geoip with
      | Some geoip -> (Some geoip.latitude, Some geoip.longitude)
      | None -> (None, None)
  in
  let streams =
    match params.get_opt "streams" with
      | None -> raise (Missing_parameter "streams")
      | Some streams -> Radio.streams_of_yojson streams
  in
  Db.transaction (fun db ->
      let user = get_user ~db () in
      Radio.create ~name ?website ~user ?description ?genre ?logo ?longitude
        ?latitude ~db ~streams ())
