open Parser

let rec visit ast =
  match ast with 
    | Bold exprs          -> String.concat "" ["<b>"; visit_list exprs ;"</b>"]
    | Underline exprs     -> String.concat "" ["<u>"; visit_list exprs ;"</u>"]
    | Italic exprs        -> String.concat "" ["<i>"; visit_list exprs ;"</i>"]
    | Codeline exprs      -> String.concat "" ["<pre>"; visit_list exprs ;"</pre>"]
    | Item exprs          -> String.concat "" ["<p>• "; visit_list exprs ;"</p>"]
    | Codeblock str       -> String.concat "" ["<code>";  str ;"</code>"]
    | Text text           -> text
    | Linebreak           -> ""
    | Title (exprs, depth)   -> 
        let n = "h"^(string_of_int depth) in
        String.concat "" ["<"^n^">"; visit_list exprs ;"</"^n^">"]
    
and visit_list list = 
  String.concat "" (List.map visit list)

let compile content = 
  let ast = parse content in
  match ast with  
    | Ok ast -> Ok (visit_list ast)
    | Error err -> Error err
  