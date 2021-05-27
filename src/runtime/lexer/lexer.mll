{
  open Bardparser.Parser
  exception Error of string
  let error lexbuf msg =
    let position = Lexing.lexeme_start_p lexbuf in
    let err_str = Printf.sprintf "Lexing error in file %s at position %d:%d\n"
                  position.pos_fname position.pos_lnum (position.pos_cnum - position.pos_bol + 1)
                  ^ msg ^ "\n" in
    raise (Error err_str)
}

let digit = ['0'-'9']
(* let digit3 = digit digit digit *)
let digits = digit+
let letter = ['a'-'z' 'A'-'Z']
let idn_sym = ['_']
let idnEnd = (digit | letter | idn_sym)*
let idn = (letter | idn_sym) idnEnd
let invalidIdn = digit idnEnd
let notQuoteBackslash = [' '-'!' '#'-'[' ']'-'~']

rule token = parse
  [' ' '\t' '\r' ]    { token lexbuf }     (* skip blanks *)
| '\n'                { Lexing.new_line lexbuf; token lexbuf }
| "/*"                { comment 0 lexbuf }
| ','                 { COMMA }
| ':'                 { COLON }
| '('                 { LPAREN }
| ')'                 { RPAREN }
| '{'                 { LBRACE }
| '}'                 { RBRACE }
| "raisedTo"          { RAISEDTO }
| "send"              { SEND }
| "receive"           { RECEIVE }
| '+'                 { PLUS }
| '-'                 { MINUS }
| '*'                 { TIMES }
| '/'                 { DIVIDE }
| '<'                 { LT }
| "<="                { LE }
| '>'                 { GT }
| ">="                { GE }
| "=="                { EEQ }
| "<>"                { NEQ }
| '&'                 { AND }
| '|'                 { OR }
| '~'                 { NOT }
| '^'                 { CARET }
| "let"               { LET }
| "val"               { VAL }
| "fun"               { FUN }
| '='                 { EQ }
| "in"                { IN }
| "end"               { END }
| "if"                { IF }
| "then"              { THEN }
| "else"              { ELSE }
| digits as i         { match int_of_string_opt i with
                        | Some n ->  INT n
                        | None -> error lexbuf ("Number too large '" ^ i ^ "'.") }
| "true"              { TRUE }
| "false"             { FALSE }
| '\"'                { string_parse "" lexbuf }
| "Int"               { INT_TYPE}
| "Bool"              { BOOL_TYPE}
| "String"            { STRING_TYPE}
| "Unit"              { UNIT_TYPE}
| "=>"                { ARROW }
| idn as s            { ID s }
| invalidIdn as s     { error lexbuf ("Invalid identifier '" ^ s ^ "'. Cannot start with a number.") }
| _ as t              { error lexbuf ("Invalid character '" ^ (String.make 1 t) ^ "'") }
| eof                 { EOF }


and comment comment_level = parse
  "/*"                { comment (comment_level + 1) lexbuf }
| "*/"                { (if comment_level == 0 then token else comment (comment_level - 1)) lexbuf }
| '\n'                { Lexing.new_line lexbuf; comment comment_level lexbuf }
| _                   { comment comment_level lexbuf }
| eof                 { error lexbuf ("EOF found while lexing comment") }


and string_parse acc = parse
  '\"'                         { STRING acc }
| notQuoteBackslash* as s      { string_parse (acc ^ s) lexbuf }
| "\\n"                        { string_parse (acc ^ "\n") lexbuf }
| _ as t                       { error lexbuf ("Invalid character in string '" ^ (String.make 1 t) ^ "'") }
| eof                          { error lexbuf ("EOF found while lexing string") }
