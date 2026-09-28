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

let escaped_char = function
  | '\\' -> '\\'
  | '"' -> '"'
  | '\'' -> '\''
  | 'n' -> '\n'
  | 'r' -> '\r'
  | 't' -> '\t'
  | c -> error "unknown escape sequence \\%c" c

let char_value bytes =
  if String.length bytes = 0 then error "empty character literal";
  let decoded = String.get_utf_8_uchar bytes 0 in
  if not (Uchar.utf_decode_is_valid decoded) then error "invalid UTF-8 in character literal";
  if Uchar.utf_decode_length decoded <> String.length bytes then
    error "character literal must contain exactly one Unicode character";
  let value = Uchar.to_int (Uchar.utf_decode_uchar decoded) in
  if value >= 256 then error "character literal exceeds u8 range";
  value

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

  | '"' { string_literal (Buffer.create 32) lexbuf }
  | '\'' { char_literal (Buffer.create 4) lexbuf }
  | (('#' ident) | symbol | ident) as name { reserved_or_id name }
  | _ as c { error "unexpected character %C" c }

and string_literal buffer = parse
  | '"' { Parser.CONST_STRING (Buffer.contents buffer) }
  | '\\' (_ as c) { Buffer.add_char buffer (escaped_char c); string_literal buffer lexbuf }
  | '\n' | '\r' { error "newline in string literal" }
  | eof { error "unterminated string literal" }
  | _ as c { Buffer.add_char buffer c; string_literal buffer lexbuf }

and char_literal buffer = parse
  | '\'' { Parser.CONST_CHAR (char_value (Buffer.contents buffer)) }
  | '\\' (_ as c) { Buffer.add_char buffer (escaped_char c); char_literal buffer lexbuf }
  | '\n' | '\r' { error "newline in character literal" }
  | eof { error "unterminated character literal" }
  | _ as c { Buffer.add_char buffer c; char_literal buffer lexbuf }
