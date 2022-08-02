type params = {
  get : string -> JSON.t;
  get_opt : string -> JSON.t option;
  string : string -> string;
  string_opt : string -> string option;
}

exception Invalid_password of string
exception Invalid_radio of string
exception Missing_parameter of string
exception Invalid_parameter of string
