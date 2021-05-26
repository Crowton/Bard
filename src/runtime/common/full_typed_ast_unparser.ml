(** Typed AST full information printer implementation *)

open Typed_ast
open Full_unparser_common


let field (Field { name; typean; _ }, d) = 
    concat [ indent d; "("; name; ", "; string_of_typean typean; ")"]  


let as_string typ texp =
  let rec string_of_texp (texp, d) = match texp with 
    | IntLit i -> concat [indent d; "IntLit("; string_of_int i; ")"]
    | BoolLit b -> concat [indent d; "BoolLit("; string_of_bool b; ")"]
    | StringLit (s, _) -> concat [indent d; "StringLit(\""; String.escaped s ; "\")"]
    | VarExp (x, _) -> concat [ indent d; "VarExp("; x; ")" ]
    | RaisedToExp { texp; label; _ } -> concat [ indent d; "RaisedToExp(\n"; string_of_texp (texp, d + 1); ",\n"; indent (d + 1); string_of_label label; ")" ]
    | SendExp { texp; _ } -> concat [indent d; "SendExp("; string_of_texp (texp, d); ")"]
    | BinOpExp { left; oper; right; leftcanfail; rightcanfail; _ } ->
        concat [ indent d; "BinOpExp("; binopname oper; ",\n"; string_of_texp (left, d + 1); ",\n"; string_of_texp (right, d + 1);
        ",\n"; indent (d + 1); string_of_bool leftcanfail; ",\n"; indent (d + 1); string_of_bool rightcanfail; ")"]
    | UnOpExp { oper; texp; canfail; _ } ->
        concat [ indent d; "UnOpExp("; unopname oper; ",\n"; string_of_texp (texp, d + 1); ",\n"; indent (d + 1); string_of_bool canfail; ")"]
    | IfExp { test; thn; els; _ }    -> 
       concat [ indent d; "IfExp(\n"; string_of_texp (test, d + 1); ",\n"; string_of_texp (thn, d + 1);
                (match els with None -> "" | Some e -> ",\n" ^ string_of_texp (e, d + 1)) ; ")"]
    | CallExp { func; args; _ } ->
        concat [ indent d
               ; "CallExp(\n"
               ; string_of_texp (func, d + 1)
               ; ", ["
               ; dolist d (fun ((a, b, _), d') -> concat ["("; string_of_texp (a, d'); ", "; string_of_bool b; ")"]) args
               ; "])"
               ]
    | LambdaExp { params: fielddata list; body: texp; _ } ->
        concat [indent d; "LambdaExp(["; dolist d field params; "], "; string_of_texp (body, d + 1); ")"]
    | LetExp {decls; body; _} ->
        concat [indent d ; "LetExp(["; dolist d dec decls; "],\n"; string_of_texp (body, d + 1); ")"]

  and dec (theDec,d) = match theDec with 
    | FunDec l ->
       let f ( Fdecl {name; params; result; body; rescanfail; _}, d ) = 
         concat [ indent d; "("; name; ",["; dolist d field params; "],\n"
               ; indent (d + 1); string_of_typean result
               ; ",\n"
               ; string_of_texp (body, d + 1)
               ; ",\n"; indent (d + 1); string_of_bool rescanfail
               ; ")"] in
       concat [indent d; "FunctionDec["; dolist d f l; "]"]
    | ValDec { name; typean; init; canfail; _ } -> 
        concat [ indent d; "VarDec("; name; ", "
              ; string_of_typean typean
              ; ",\n"
              ; string_of_texp (init, d + 1)
              ; ",\n"
              ; indent (d + 1); string_of_bool canfail
              ; ")"]
  
  in string_of_typ typ ^ "\n" ^ string_of_texp (texp, 0)


let string_of_type_and_texp = as_string  
let print_exp out typ texp = Format.fprintf out "%s\n" (as_string typ texp)
