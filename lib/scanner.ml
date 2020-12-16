(**
  Scans a token from a string and returns a token from it.
*)

type token_type =
  | SHARP 
  | UNDERSCORE
  | ASTERISK
  | MINUS
  | TEXT
  | ACUDE
  | EOF
  | LINEBREAK

type token = {
    text: string option;
    line: int;
    token_type: token_type;
} 

(* This is used to locate where it'll read next *)
type scanner_context = {
  source: string;
  start: int;
  current: int;
  line: int;
}

let advance context = 
  {context with current = context.current + 1}

let advance_line context = 
  {context with line = context.line + 1}

let current_char context = 
  if context.current < String.length context.source then
    Some (context.source.[context.current])
  else
    None

let next_char context = 
  if context.current+1 < String.length context.source then
    Some (context.source.[context.current+1])
  else
    None
    
let new_context str = 
  { source = str; start = 0; current = -1; line = 1; }

(* Creates a token with literal meaning *)
let non_literal_tkn kind ctx = 
  { text = None; line = ctx.line; token_type = kind; }, ctx

(* converts char to token *)
let escaped_tkn chr ctx = 
  { text = Some(Core.Char.to_string chr); line = ctx.line; token_type = TEXT; }, ctx

let set_start ctx = 
  { ctx with start = ctx.current }

(* returns a token with text inside it *)
let literal text line = 
  { text = Some text; line = line; token_type = TEXT}

(*  creates a text literal by recursion *)
let rec add_text_literal context =
  let matching = function Some('#' | '_' | '*' | '-' | '$' | '`' | '\n' | '\\') | None -> true | _ -> false in
  if (not (matching (current_char context))) then
      add_text_literal (advance context)
  else
    let len = context.current - context.start in
    let text = (String.sub context.source context.start len) in
    (literal text context.line, {context with current = context.current - 1})

let scan_token context =
  let advanced_context = advance context in
      match (current_char advanced_context) with
      | Some '#' -> non_literal_tkn SHARP advanced_context
      | Some '_' -> non_literal_tkn UNDERSCORE advanced_context
      | Some '*' -> non_literal_tkn ASTERISK advanced_context
      | Some '-' -> non_literal_tkn MINUS advanced_context
      | Some '`' -> non_literal_tkn ACUDE advanced_context
      | Some '\n' -> non_literal_tkn LINEBREAK (advance_line advanced_context)
      | Some '\\' -> 
        let new_ctx = (advance advanced_context) in
        (match current_char new_ctx with 
            | Some 'n' -> escaped_tkn '\n' new_ctx
            | Some n -> escaped_tkn n new_ctx
            | _ -> escaped_tkn ' ' new_ctx)
      | Some _ -> add_text_literal (set_start advanced_context) 
      | None -> non_literal_tkn EOF advanced_context
  
