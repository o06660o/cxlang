{

open Parser
exception Lexer_error of string
let error fmt = Printf.ksprintf (fun msg -> raise (Lexer_error msg)) fmt

let reserved = Hashtbl.of_seq (List.to_seq [
  ("auto", AUTO);
  ("bool", BOOL);
  ("break", BREAK);
  ("continue", CONTINUE);
  ("extern", EXTERN);
  ("else", ELSE);
  ("false", FALSE);
  ("fn", FN);
  ("if", IF);
  ("loop", LOOP);
  ("ptr", PTR);
  ("return", RETURN);
  ("true", TRUE);
  ("(", LPAREN);
  (")", RPAREN);
  ("[", LBRACKET);
  ("]", RBRACKET);
  ("{", LBRACE);
  ("}", RBRACE);
  (":", COLON);
  (",", COMMA);
  (".", DOT);
  (";", SEMI);
  ("=", EQ);
  ("->", ARROW);
  ("~", TILDE);
  ("!", BANG);
  ("/", SLASH);
  ("*", STAR);
  ("%", PERC);
  ("+", PLUS);
  ("-", DASH);
  ("<<", LTLT);
  (">>", GTGT);
  ("<", LT);
  ("<=", LTEQ);
  (">", GT);
  (">=", GTEQ);
  ("==", EQEQ);
  ("!=", BANGEQ);
  ("&", AMP);
  ("^", CARET);
  ("|", BAR);
  ("&&", AMPAMP);
  ("||", BARBAR);
  ("#deref", DEREF);
  ("#addrof", ADDROF);
])

let reserved_or_id name =
  match Hashtbl.find_opt reserved name with
  | Some token -> token
  | None when String.starts_with ~prefix:"#" name -> error "unknown builtin %S" name
  | None -> ID name

}

let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let whitespace = [' ' '\t' '\n']

let symbol = "(" | ")" | "[" | "]" | "{" | "}" | ":" | "," | "." | ";" | "=" | "->" |  "~" | "!"
  | "/" | "*" | "%" | "+" | "-" | "<<" | ">>" | "<" | "<=" |  ">" | ">=" | "==" | "!=" | "&" | "^"
  | "|" | "&&" | "||"

rule token = parse
  | eof { EOF }
  | whitespace+ { token lexbuf }

  | 'i' (['0' - '9']+ as bits) { SIGNED_TY (int_of_string bits) }
  | 'u' (['0' - '9']+ as bits) { UNSIGNED_TY (int_of_string bits) }
  | ['0' - '9']+ as literal { INT (int_of_string literal) }
  | (['0' - '9']+ '_' ('i' | 'u') ['0' - '9']+) as literal { CONST_INT literal }

  | (('#' ident) | symbol | ident) as name { reserved_or_id name }
  | _ as c { error "unexpected character %C" c }
