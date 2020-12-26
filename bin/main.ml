open Core

(* 
  Checks if the argument is an file. It will fail if the file not exists or 
  cannot be read
*)

let dirname = 
  Command.Arg_type.create (fun dirname ->
    match Sys.is_directory dirname with
      | `Yes -> failwith "This directory already exists"
      |  _ -> dirname)

(* Gets filenames and build it to a static website. *)
let build = 
  Command.basic
    ~summary:"Reads a book and spits html"
    Command.Let_syntax.(
      let%map_open _trial = flag "-t" no_arg ~doc:" i dont have a use for this flag..." in
      fun () -> Build.build () )

(* Inits a new project with a certain name *)
let init = 
  Command.basic
    ~summary:"Inits a new markcase book"
    Command.Let_syntax.(
      let%map_open dir = anon ("dirname" %: dirname) 
      and theme = anon ("theme" %: string)  in
      fun () -> Init.init dir theme)

    
let () = 
  let hash = String.Table.create () ~size:4 in
  let _ = String.Table.add hash ~key:"ata" ~data:"be" in
  print_endline (Alembic.Template.template "ata {{ ata }} be" hash);
  Command.run (
  Command.group ~summary:"Creates markdowns in ocaml" 
    ["build", build;
     "init", init
    ])