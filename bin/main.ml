open Alembic

let () = 
  let text = "# *\"H1 Heading\"*\n# _\"H1 undescore heading\"_ ata\n## \"H2 Heading\"\n*\"This is bold text\"* \"and this is regular text\"\n\"Following three items should be in a list, this is plaintext\"\n- `\"Plaintext list `tem\"\n- *\"Bold List item\"*\n- _*\"Underscore bold list item\"*_\n- _\"Underscore  list item\"_\n#\"This is heading again" in
  match Parser.parse text with
    | Ok x -> print_endline (Parser.show_expr x)
    | _ -> print_endline "Err bro!" 