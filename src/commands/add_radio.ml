open Commands_base

let exec ~user ~get_param_opt ~get_param_string ~ip () =
  let name = get_param_string "radio" in
  let website = get_param_string "website" in
  let description = get_param_string "description" in
  let genre = get_param_string "genre" in
  let logo = get_param_string "logo" in
  let geoip = GeoIP.lookup_opt ip in
  let latitude, longitude =
    match geoip with
      | Some geoip -> (geoip.latitude, geoip.longitude)
      | None -> (0., 0.)
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
  Radio.register ~name ~website ~user ~description ~genre ~logo ~longitude
    ~latitude ~streams ()
