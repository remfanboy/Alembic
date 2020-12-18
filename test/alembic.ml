open Alembic

let rec check_tokens ctx ~n:name ~e:eql =
  match eql with
    | h::tail -> 
      let tkn, ctx = Scanner.scan_token ctx in
      Alcotest.(check bool) name true (tkn.token_type = h);
      check_tokens ctx ~n:name ~e:tail
    | [] -> ()

let test_scanner () =
  Scanner.new_context "*oi\n#"
  |> check_tokens ~n:"first_token" ~e:[Scanner.ASTERISK; Scanner.TEXT("oi"); Scanner.LINEBREAK]

let test_depth_parser () =
  let ctx = Scanner.new_context "***\n#" in
  match Parser.eat_depth ctx (function Scanner.ASTERISK -> true | _ -> false) with
    | Ok (depth, _) -> 
      if depth <> 3 then
        Alcotest.fail ("Expected depth equals 3 but got "^(string_of_int depth));
        ()
    | _ -> 
      Alcotest.fail "Error on trying to eat asterisks"

let test_join_parser () =
  let ctx = Scanner.new_context "# potato chips * _\n# today is gonna be the day that is gonna the day" in
  let pat ctx = 
    match Parser.eat ctx Scanner.LINEBREAK with
      | Ok(_, ctx) -> true,ctx
      | _ -> false,ctx
  in
  match Parser.join_text_until_pat ctx pat with
    | Ok(text, _) -> 
      Alcotest.(check string) "Equality between strings" "# potato chips * _" text;
    | _ -> 
      Alcotest.fail "Error on trying to join text until linebreak"
 
let test_title_parser () =
  let ctx = Scanner.new_context "## potato chips\n# today is gonna be the day that is gonna the day" in
  match Parser.parse_entry ctx with
    | Ok(expr, _ctx) ->
        (match expr with
        | Parser.Title(Parser.Group([Parser.Text(" potato chips")]),2) -> 
          ()
        | _ -> 
          Alcotest.fail ("Error on ast "^Parser.show_expr(expr)))
    | Error err ->
      Alcotest.fail ("Error on parsing "^Parser.show_syntax_error(err))
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
      ]
    )
  ]