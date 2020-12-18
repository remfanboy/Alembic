
type expr = 
  | Bold of expr
  | Underline of expr
  | Italic of expr
  | Title of expr * int
  | Codeline of expr
  | Item of expr * int
  | Codeblock of string
  | Text of string
  | Group of expr list
  [@@deriving show]

type syntax_error = {
  got: Scanner.token_type;
  line: int
} [@@deriving show]

(* Just to make it faster *)
let next ctx = Scanner.scan_token ctx

(* 
   it returns the token that it matched and advances to the next step in the Scanner 
   if it not matches the pattern it will raise an Error
*)

let eat_pat ctx pat =
  let token, new_ctx = next ctx in
  if (pat token.token_type) then
    Ok (token, new_ctx)
  else
    Error {got = token.token_type; line = token.line}

(* eats a single token with one variant *)
let eat ctx kind = eat_pat ctx (fun x -> x == kind)

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
      if count > 0 then
        eat_exact_depth ctx pat (count-1)
      else
        Ok(ctx)
    | Error err -> Error err 
    
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
      | _ -> Error "Not implemented", ctx

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
        | LINEBREAK ->  Ok(Text(""), new_ctx)
        | TEXT str -> Ok(Text(str), new_ctx)
        | _ -> Error {got = tkn.token_type; line = tkn.line}
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
    let pat = (function Scanner.SHARP -> true | _ -> false) in
    match eat_depth ctx pat with
      | Error err -> Error err  
      | Ok(depth, ctx) ->
        let pat = (function Scanner.LINEBREAK | Scanner.EOF -> true | _ -> false) in
        let expr, ctx = (group_until_pat ctx pat) in
        print_endline (show_expr (Group expr));
        match eat_pat ctx pat with
          | Ok(_, ctx) ->
             Ok (Title (Group expr, depth), ctx)
          | Error err -> Error err
          
        
