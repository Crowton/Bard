(**************************************************************************)
(* AU Compilation. Assignment submissions must not modify this file       *)
(* Do not distribute                                                      *)
(**************************************************************************)


%start <(string * Lexing.position) list> lexdriver
%%
(* --- lexer driver --- *)


anytoken:
  | i = INT { "INT " ^ (string_of_int i) , $startpos}
  | s = STRING { "STRING \"" ^ String.escaped s ^"\"", $startpos}
  | x = ID { "ID " ^ x, $startpos } 
  | COMMA { "COMMA" , $startpos}
  | COLON { "COLON" , $startpos}
  | LPAREN { "LPAREN" , $startpos} 
  | RPAREN { "RPAREN" , $startpos} 
  | PLUS { "PLUS" , $startpos} 
  | MINUS { "MINUS" , $startpos } 
  | TIMES { "TIMES" , $startpos} 
  | DIVIDE { "DIVIDE" , $startpos} 
  | LT { "LT" , $startpos} 
  | LE { "LE" , $startpos} 
  | GT { "GT" , $startpos} 
  | GE { "GE" , $startpos} 
  | EEQ { "EEQ" , $startpos} 
  | NEQ { "NEQ" , $startpos} 
  | AND { "AND" , $startpos} 
  | OR { "OR" , $startpos} 
  | NOT { "NOT" , $startpos} 
  | CARET { "CARET" , $startpos} 
  | IF { "IF" , $startpos} 
  | THEN { "THEN" , $startpos} 
  | ELSE { "ELSE", $startpos } 
  | LET { "LET" , $startpos} 
  | IN { "IN" , $startpos} 
  | END { "END" , $startpos} 
  | VAL { "VAL" , $startpos} 
  | FUN { "FUN" , $startpos} 
  | EQ { "EQ" , $startpos} 

lexdriver: 
  ls = list (anytoken) EOF { ls }

