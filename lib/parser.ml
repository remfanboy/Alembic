open Scanner

type expr = 
  | Bold of expr list
  | Underline of expr list
  | Italic of expr list
  | Title of expr list * int
  | Codeline of expr list
  | Item of expr list
  | Codeblock of string
  | Text of string
  | Linebreak
  [@@deriving show]

type error = 
  | SyntaxError of token
  | NotImplemented of token
  [@@deriving show]

(* Just to make it faster *)
let next ctx = scan_token ctx

(* 
   it returns the token that it matched and advances to the next step in the Scanner 
   if it not matches the pattern it will returns an Error
*)

let eat_pat ctx pat =
  let token, new_ctx = next ctx in
  if (pat token.token_type) then
    Ok (token, new_ctx)
  else
    Error (SyntaxError token)

(* eats a single token with one variant *)
let eat ctx kind = eat_pat ctx (fun x -> x = kind)

let eat_depth ctx pat = 
  match eat_pat ctx pat with 
    | Ok(_, ctx) ->
      let rec loop_depth ctx =
        match eat_pat ctx pat with 
          | Ok (_, ctx) -> 
            let depth, ctx = (loop_depth ctx) in
            (1 + depth, ctx)   
          | _ -> 
            (1,ctx) in
      Ok (loop_depth ctx)
    | Error err -> Error err 

let rec eat_exact_depth ctx pat count = 
  match eat_pat ctx pat with 
    | Ok(_, ctx) ->
      if count > 1 then
        eat_exact_depth ctx pat (count-1)
      else
        Ok(ctx)
    | Error err -> 
      Error err 
    
(* Transforms a token in string again *)
let transform_to_text ctx =
  let (tkn, new_ctx) = next ctx in
    match tkn.token_type with 
      | SHARP -> Ok "#", new_ctx
      | MINUS -> Ok "-", new_ctx
      | TEXT str -> Ok str, new_ctx
      | UNDERSCORE -> Ok "_", new_ctx
      | ASTERISK -> Ok "*", new_ctx
      | ACUDE -> Ok "`", new_ctx
      | LINEBREAK -> Ok "\n", new_ctx
      | _ -> Error (SyntaxError tkn), ctx

(* Join all the tokens after it as text *)
let rec join_text_until_pat ctx pat = 
  let tkn, _ = next ctx in
  let matched, new_ctx = pat ctx in 
  if (not matched) && tkn.token_type != EOF then
    let text, next_ctx = transform_to_text new_ctx in
    match text with 
      | Error x -> Error x 
      | Ok text -> 
        match join_text_until_pat next_ctx pat with
          | Ok(other, next_ctx) -> 
            Ok (text^other, next_ctx)
          | err -> err
  else
    Ok("", new_ctx)

let rec parse_entry ctx =
    let (tkn, new_ctx) = next ctx in
      match tkn.token_type with 
        | SHARP -> parse_title ctx
        | ASTERISK -> parse_bold ctx
        | ACUDE -> parse_code ctx
        | MINUS -> parse_list ctx
        | UNDERSCORE -> parse_underscore ctx
        | LINEBREAK ->  Ok(Linebreak, new_ctx)
        | TEXT str -> Ok(Text(str), new_ctx)
        | _ -> Error (SyntaxError tkn)

and group_until_pat ctx pat =
  let tkn, _ = next ctx in
  if (not (pat tkn.token_type)) then
    match parse_entry ctx with
      | Ok (ast, new_ctx) ->
        let list, new_ctx = group_until_pat new_ctx pat in
        ast::list, new_ctx
      | _ -> 
        [], ctx
  else
    [], ctx

and parse_title ctx = 
    let pat = (function SHARP -> true | _ -> false) in
    match eat_depth ctx pat with
      | Error err -> Error err  
      | Ok(depth, ctx) ->
        let pat = (function LINEBREAK | EOF -> true | _ -> false) in
        let expr, ctx = (group_until_pat ctx pat) in
        match eat_pat ctx pat with
          | Ok(_, ctx) ->
             Ok (Title (expr, depth), ctx)
          | Error err -> Error err
          
and parse_bold ctx = 
  let pat = (function ASTERISK -> true | _ -> false) in
    match eat_depth ctx pat with
      | Error err -> Error err  
      | Ok(depth, ctx) ->
        let pat = (function ASTERISK | EOF -> true | _ -> false) in
        let expr, ctx = (group_until_pat ctx pat) in
        match eat_exact_depth ctx (function ASTERISK -> true | _ -> false) depth with
        | Error err -> Error err
        | Ok(ctx) -> 
          match depth with
            | 1 -> Ok (Italic expr, ctx)
            | 2 -> Ok (Bold expr, ctx) 
            | _ -> Ok (Italic [Bold expr], ctx) 

and parse_code ctx = 
  let pat = (function ACUDE -> true | _ -> false) in
    match eat_depth ctx pat with
      | Error err -> Error err 
      | Ok(depth, ctx) ->
        if depth >= 3 then
          let pat ctx = match eat_exact_depth ctx (function ACUDE | EOF -> true | _ -> false) depth with
            | Error _ -> false, ctx
            | Ok(ctx) -> true, ctx in
          match join_text_until_pat ctx pat with
            | Error err -> Error err
            | Ok(text, ctx) ->
              Ok (Codeblock text, ctx) 
        else
          let pat = (function ACUDE | EOF -> true | _ -> false) in
          let expr, ctx = (group_until_pat ctx pat) in
          match eat_exact_depth ctx (function ACUDE -> true | _ -> false) depth with
          | Error err -> Error err
          | Ok(ctx) -> Ok (Codeline expr, ctx) 

and parse_list ctx = 
  match eat ctx MINUS with
    | Error err -> Error err  
    | Ok(_depth, ctx) ->
      let pat = (function LINEBREAK | EOF -> true | _ -> false) in
      let expr, ctx = (group_until_pat ctx pat) in
      match eat_pat ctx pat with
        | Ok(_, ctx) -> Ok (Item expr, ctx)
        | Error err -> Error err

and parse_underscore ctx = 
  let pat = (function UNDERSCORE -> true | _ -> false) in
    match eat_depth ctx pat with
      | Error err -> Error err  
      | Ok(depth, ctx) ->
        let pat = (function UNDERSCORE | EOF -> true | _ -> false) in
        let expr, ctx = (group_until_pat ctx pat) in
        match eat_exact_depth ctx (function UNDERSCORE -> true | _ -> false) depth with
        | Error err -> Error err
        | Ok(ctx) -> 
          match depth with
            (* If i want to add something here in the future.... *)
            | _ -> Ok (Underline expr, ctx)

(* Parses a sequence of expressions *)
let rec parse_compound ctx = 
  let tkn, _ = next ctx in
    if tkn.token_type != EOF then
      match parse_entry ctx with
        | Error err -> Error err
        | Ok (expression, new_ctx) -> 
          match parse_compound new_ctx with
            | Ok(expr) -> Ok (expression::expr)
            | Error err -> Error err
    else
      Ok([])
(* Returns a group of parsed expressions *)
let parse text = 
  let res = Scanner.new_context text |> parse_compound in
  match res with
    | Error err -> Error err
    | Ok x -> Ok(x)
