open Commands_base

let exec ~get_user ~get_param_opt ~get_param_string_opt ~get_param_string ~ip ()
    =
  let name = get_param_string "radio" in
  let website = get_param_string_opt "website" in
  let description = get_param_string_opt "description" in
  let genre = get_param_string_opt "genre" in
  let logo = get_param_string_opt "logo" in
  let geoip = GeoIP.lookup_opt ip in
  let latitude, longitude =
    match geoip with
      | Some geoip -> (Some geoip.latitude, Some geoip.longitude)
      | None -> (None, None)
  in
  let streams =
    match get_param_opt "streams" with
      | None -> raise (Missing_parameter "streams")
      | Some (`List l) ->
          List.map
            (function
              | `Assoc s ->
                  let format = List.assoc "format" s |> JSON.string in
                  let url = List.assoc "url" s |> JSON.string in
                  { Radio.format; url }
              | _ -> raise (Invalid_parameter "streams"))
            l
      | Some _ -> raise (Invalid_parameter "streams")
  in
  Db.transaction (fun db ->
      let user = get_user ~db () in
      ignore
        (Radio.create ~name ?website ~user ?description ?genre ?logo ?longitude
           ?latitude ~db ~streams ()))
