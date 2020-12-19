open Alembic

let () = 
  let text = "# *\"H1 Heading\"*" in
  match Compiler.compile text with
    | Ok ast ->  print_endline ast
    | _ -> print_endline "Err bro!" 