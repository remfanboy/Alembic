open Core

type hierarchy = 
| Dir of string * hierarchy list
| File of string * string

let create_file path data =
  let oc = Out_channel.create path in
  Printf.fprintf oc "%s" data;
  Out_channel.close oc

let rec create_from_hierarchy path tree =
  match tree with 
    | Dir(name, list) -> 
        let new_path = Filename.concat path name in
        Unix.mkdir new_path;
        List.iter list ~f:(fun expr -> create_from_hierarchy new_path expr)
    | File(name, template) -> 
        create_file (Filename.concat path name) template

let rec delete_dir path = match Sys.is_directory path with
  | `Yes ->
    Sys.readdir path |>
    Array.iter ~f:(fun name -> delete_dir (Filename.concat path name));
    Unix.rmdir path
  | _ -> Sys.remove path
  

let read_file path =
  let ch = In_channel.create ~binary:false path in
  let content =  (In_channel.input_all ch) in
  In_channel.close ch;
  content 