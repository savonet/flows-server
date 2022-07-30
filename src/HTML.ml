type t = string ref

let w ?(nl = true) (h : t) =
  Printf.ksprintf (fun s ->
      let s = if nl then s ^ "\n" else s in
      h := !h ^ s)

let create ?title ?css () : t =
  let h = ref "" in
  w h "<html>";
  w h "<head>";
  w h "<meta charset=\"UTF-8\">";
  if css <> None then
    w h "<link rel=\"stylesheet\" href=\"%s\"/>" (Option.get css);
  if title <> None then w h "<title>%s</title>" (Option.get title);
  w h "</head>";
  w h "<body>";
  h

let div h ?cls s =
  let cls =
    match cls with Some c -> Printf.sprintf " class=\"%s\"" c | None -> ""
  in
  w h "<div%s>%s</div>" cls s

let h1 h = w h "<h1>%s</h1>"
let h2 h = w h "<h2>%s</h2>"

let ul h l =
  w h "<ul>";
  List.iter (w h "<li>%s</li>") l;
  w h "</ul>"

let p h = w h "<p>%s</p>"

let to_string h =
  w h "</html>";
  !h
