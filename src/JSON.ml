include Yojson.Safe

let of_string = from_string

let to_string ?(pretty = false) j =
  if pretty then pretty_to_string j else to_string j

let string = function `String s -> s | _ -> assert false
let float = function `Float x -> x | _ -> assert false
