open Commands_base

let exec ~user ~get_param_opt ~get_param_string ~ip () =
  let get_param_string_opt name =
    match get_param_opt name with
      | None -> None
      | Some (`String s) -> Some s
      | _ -> raise (Invalid_parameter name)
  in
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
  Radio.register ~name ~website ~user ~description ~genre ~logo ~longitude
    ~latitude ~streams ()
