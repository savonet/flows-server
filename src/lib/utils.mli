type result = {
  int : string -> int;
  int_opt : string -> int option;
  string : string -> string;
  string_opt : string -> string option;
  float : string -> float;
  float_opt : string -> float option;
}

val list_of_result : Postgresql.result -> result list
val opt : string option -> string
