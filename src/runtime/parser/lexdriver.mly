%start <(string * Lexing.position) list> lexdriver
%%


anytoken:
  | i = INT { "INT " ^ (string_of_int i) , $startpos }
  | TRUE { "TRUE" , $startpos }
  | FALSE { "FALSE" , $startpos }
  | s = STRING { "STRING \"" ^ String.escaped s ^"\"", $startpos }
  | x = ID { "ID " ^ x, $startpos }
  | LBRACE { "LBRACE" , $startpos }
  | RBRACE { "RBRACE" , $startpos }
  | RAISEDTO { "RAISEDTO" , $startpos }
  | SEND { "SEND" , $startpos }
  | RECEIVE { "RECEIVE" , $startpos }
  | BLOCKDECL { "BLOCKDECL" , $startpos }
  | COMMA { "COMMA" , $startpos }
  | COLON { "COLON" , $startpos }
  | LPAREN { "LPAREN" , $startpos }
  | RPAREN { "RPAREN" , $startpos }
  | PLUS { "PLUS" , $startpos } 
  | MINUS { "MINUS" , $startpos } 
  | TIMES { "TIMES" , $startpos } 
  | DIVIDE { "DIVIDE" , $startpos } 
  | LT { "LT" , $startpos } 
  | LE { "LE" , $startpos } 
  | GT { "GT" , $startpos } 
  | GE { "GE" , $startpos } 
  | EEQ { "EEQ" , $startpos }
  | NEQ { "NEQ" , $startpos }
  | AND { "AND" , $startpos }
  | OR { "OR" , $startpos }
  | NOT { "NOT" , $startpos }
  | CARET { "CARET" , $startpos }
  | IF { "IF" , $startpos }
  | THEN { "THEN" , $startpos }
  | ELSE { "ELSE", $startpos }
  | LET { "LET" , $startpos }
  | IN { "IN" , $startpos }
  | END { "END" , $startpos }
  | VAL { "VAL" , $startpos }
  | FUN { "FUN" , $startpos }
  | EQ { "EQ" , $startpos }
  | INT_TYPE {"INT_TYPE" , $startpos }
  | BOOL_TYPE {"BOOL_TYPE" , $startpos }
  | STRING_TYPE {"STRING_TYPE" , $startpos }
  | UNIT_TYPE {"UNIT_TYPE" , $startpos }
  | ARROW {"ARROW" , $startpos }

lexdriver: 
  ls = list (anytoken) EOF { ls }

