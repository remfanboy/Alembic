open Alembic
open Core

let read_file path =
  let ch = In_channel.create ~binary:false path in
  let content =  (In_channel.input_all ch) in
  In_channel.close ch;
  content 

let create_file path data =
  let oc = Out_channel.create path in
  Printf.fprintf oc "%s" data;
  Out_channel.close oc
    

let () = 
  let text = read_file "./test_suite/one.md" in
  match Compiler.compile text with
    | Ok ast ->  create_file "./shelooks.html" ast
    | _ -> print_endline "Err bro!" 