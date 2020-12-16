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
  |> check_tokens ~n:"first_token" ~e:[Scanner.ASTERISK; Scanner.TEXT; Scanner.LINEBREAK]

let () = 
  Alcotest.run "First" [
    ("Scanner",
      [
        ("simple scan", `Quick, test_scanner);
      ]
    )
  ]