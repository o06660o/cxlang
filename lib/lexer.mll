{

exception Lexer_error of string
let error fmt = Printf.ksprintf (fun msg -> raise (Lexer_error msg)) fmt

let reserved = Hashtbl.of_seq (List.to_seq [
  ("auto", Parser.AUTO);
  ("bool", Parser.BOOL);
  ("break", Parser.BREAK);
  ("continue", Parser.CONTINUE);
  ("extern", Parser.EXTERN);
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("fn", Parser.FN);
  ("if", Parser.IF);
  ("loop", Parser.LOOP);
  ("ptr", Parser.PTR);
  ("return", Parser.RETURN);
  ("true", Parser.TRUE);
  ("using", Parser.USING);
  ("(", Parser.LPAREN);
  (")", Parser.RPAREN);
  ("[", Parser.LBRACKET);
  ("]", Parser.RBRACKET);
  ("{", Parser.LBRACE);
  ("}", Parser.RBRACE);
  (":", Parser.COLON);
  (",", Parser.COMMA);
  (".", Parser.DOT);
  (";", Parser.SEMI);
  ("=", Parser.EQ);
  ("->", Parser.ARROW);
  ("~", Parser.TILDE);
  ("!", Parser.BANG);
  ("/", Parser.SLASH);
  ("*", Parser.STAR);
  ("%", Parser.PERC);
  ("+", Parser.PLUS);
  ("-", Parser.DASH);
  ("<<", Parser.LTLT);
  (">>", Parser.GTGT);
  ("<", Parser.LT);
  ("<=", Parser.LTEQ);
  (">", Parser.GT);
  (">=", Parser.GTEQ);
  ("==", Parser.EQEQ);
  ("!=", Parser.BANGEQ);
  ("&", Parser.AMP);
  ("^", Parser.CARET);
  ("|", Parser.BAR);
  ("&&", Parser.AMPAMP);
  ("||", Parser.BARBAR);
  ("#cast", Parser.CAST);
  ("#deref", Parser.DEREF);
  ("#addrof", Parser.ADDROF);
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
  | eof { Parser.EOF }
  | whitespace+ { token lexbuf }

  | 'i' (['0' - '9']+ as bits) { Parser.INT_TY (int_of_string bits, true) }
  | 'u' (['0' - '9']+ as bits) { Parser.INT_TY (int_of_string bits, false) }
  | ['0' - '9']+ as literal { Parser.INT (int_of_string literal) }
  | (['0' - '9']+ '_' ('i' | 'u') ['0' - '9']+) as literal { Parser.CONST_INT literal }

  | (('#' ident) | symbol | ident) as name { reserved_or_id name }
  | _ as c { error "unexpected character %C" c }
