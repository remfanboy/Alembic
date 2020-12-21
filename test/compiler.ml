open Alembic

let rec check_tokens ctx ~n:name ~e:eql =
  match eql with
    | h::tail -> 
      let tkn, ctx = Md.Scanner.scan_token ctx in
      Alcotest.(check bool) name true (tkn.token_type = h);
      check_tokens ctx ~n:name ~e:tail
    | [] -> ()

let test_scanner () =
  Md.Scanner.new_context "*oi\n#"
  |> check_tokens ~n:"first_token" ~e:[Md.Scanner.ASTERISK; Md.Scanner.TEXT("oi"); Md.Scanner.LINEBREAK]

let test_depth_parser () =
  let ctx = Md.Scanner.new_context "***\n#" in
  match Md.Parser.eat_depth ctx (function Md.Scanner.ASTERISK -> true | _ -> false) with
    | Ok (depth, _) -> 
      if depth <> 3 then
        Alcotest.fail ("Expected depth equals 3 but got "^(string_of_int depth));
        ()
    | _ -> 
      Alcotest.fail "Error on trying to eat asterisks"

let test_join_parser () =
  let ctx = Md.Scanner.new_context "# potato chips * _\n# today is gonna be the day that is gonna the day" in
  let pat ctx = 
    match Md.Parser.eat ctx Md.Scanner.LINEBREAK with
      | Ok(_, ctx) -> true,ctx
      | _ -> false,ctx
  in
  match Md.Parser.join_text_until_pat ctx pat with
    | Ok(text, _) -> 
      Alcotest.(check string) "Equality between strings" "# potato chips * _" text;
    | _ -> 
      Alcotest.fail "Error on trying to join text until linebreak"
 
let test_title_parser () =
  let ctx = Md.Scanner.new_context "## potato chips\n# today is gonna be the day that is gonna the day" in
  match Md.Parser.parse_entry ctx with
    | Ok(expr, _ctx) ->
        (match expr with
        | Md.Parser.Title([Md.Parser.Text(" potato chips")],2) -> 
          ()
        | _ -> 
          Alcotest.fail ("Error on ast "^Md.Parser.show_expr(expr)))
    | Error err ->
      Alcotest.fail ("Error on parsing "^Md.Parser.show_error(err))

let test_asterisk_parser () =
  let ctx = Md.Scanner.new_context "##*** potato chips ***\n# today is gonna be the day that is gonna the day" in
    match Md.Parser.parse_entry ctx with
      | Error err ->
        Alcotest.fail ("Error on parsing "^Md.Parser.show_error(err))
      | Ok(expr, _ctx) ->
        match expr with
        | Md.Parser.Title([Md.Parser.Italic [Md.Parser.Bold ([Md.Parser.Text(" potato chips ")])]],2) -> 
          ()
        | _ -> 
          Alcotest.fail ("Error on ast "^Md.Parser.show_expr(expr))

let test_code_parser () =
  let ctx = Md.Scanner.new_context "```ata\nde```" in
    match Md.Parser.parse_entry ctx with
      | Error err ->
        Alcotest.fail ("Error on parsing "^Md.Parser.show_error(err))
      | Ok(expr, _ctx) ->
        match expr with
        | Md.Parser.Codeblock("ata\nde") -> 
          ()
        | _ -> 
          Alcotest.fail ("Error on ast "^Md.Parser.show_expr(expr))

let test_list_parser () =
  let ctx = Md.Scanner.new_context "- ata\nde" in
    match Md.Parser.parse_entry ctx with
      | Error err ->
        Alcotest.fail ("Error on parsing "^Md.Parser.show_error(err))
      | Ok(expr, _ctx) ->
        match expr with
        | Md.Parser.Item(_) -> 
          ()
        | _ -> 
          Alcotest.fail ("Error on ast "^Md.Parser.show_expr(expr))


let test_list_compile () =
  match Md.Compiler.compile "-ata\n-b" with
    | Ok res ->
      Alcotest.(check string) "Equality" "<p>• ata</p><p>• b</p>" res
    | Error err -> 
      Alcotest.fail ("Error on parsing "^Md.Parser.show_error(err))

let () = 
  Alcotest.run "First" [
    ("Scanner",
      [
        ("simple scan", `Quick, test_scanner);
      ]
    ); 
    ("Parser",
      [
        ("Simple depth parse", `Quick, test_depth_parser);
        ("Simple text parse", `Quick, test_join_parser);
        ("Simple title parsing", `Quick, test_title_parser);
        ("Simple asterisk parsing", `Quick, test_asterisk_parser);
        ("Simple code parsing", `Quick, test_code_parser);
        ("Simple list parsing", `Quick, test_list_parser)
      ]
    );
    ("Compiler", [
        ("Simple list parse", `Quick, test_list_compile)
    ])
  ]