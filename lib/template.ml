open Core

(*
  Module to replace all text inside "{{ key }}" to 
  the value of the "key" specified in the hashmap
*)

let temp_pat = Str.regexp "{{[^}]*}}" 

let strip str = 
  let str = Str.replace_first (Str.regexp "^ +") "" str in
  Str.replace_first (Str.regexp " +$") "" str

let replace text temp idx len =
  let idx_end = idx + len in
  let first = String.sub text ~pos:0 ~len:idx in
  let len = ((String.length text) - idx_end) in
  let end_str = String.sub text ~pos:idx_end ~len:len in 
  String.concat [first; temp ;end_str]


let rec substitute text table place = 
  try
    let idx = Str.search_forward temp_pat text place in 
    let raw = (Str.matched_string text) in
    let key = String.sub raw ~pos:2 ~len:((String.length raw) - 4) |> strip in
    (match Core.Hashtbl.find table key with 
      | Some res ->
        let replaced = replace text res idx (String.length raw) in
        substitute replaced table place
      | None -> 
        let jump = idx + 1 in
        substitute text table jump) 
  with _ ->
    text
    
let template temp table = 
  substitute temp table 0
