open Alembic

let test_helloworld () = 
  Alcotest.(check string) "same string" "Hello, World!" hello_world

let () = 
  Alcotest.run "First" [
    ("Hello World",
      [
        ("hellooo", `Quick, test_helloworld);
      ]
    )
  ]