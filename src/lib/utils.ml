type result = {
  int : string -> int;
  int_opt : string -> int option;
  string : string -> string;
  string_opt : string -> string option;
  float : string -> float;
  float_opt : string -> float option;
}

let list_of_result (result : Postgresql.result) =
  let names = result#get_fnames_lst in
  let nresults = result#ntuples in
  let mk_result r =
    let get_opt fn l = Option.map fn (List.assoc l r) in
    let get fn l = Option.get (get_opt fn l) in
    {
      int = get int_of_string;
      int_opt = get_opt int_of_string;
      string = get (fun x -> x);
      string_opt = get_opt (fun x -> x);
      float = get float_of_string;
      float_opt = get_opt float_of_string;
    }
  in
  let rec f cur ntuple =
    if ntuple = nresults then List.rev cur
    else (
      let entry =
        List.mapi
          (fun pos lbl ->
            ( lbl,
              if result#getisnull ntuple pos then None
              else Some (result#getvalue ntuple pos) ))
          names
      in
      f (mk_result entry :: cur) (ntuple + 1))
  in
  f [] 0

let opt = Option.value ~default:Postgresql.null
