(** AST full information printer implementation *)

open Ast


let concat = String.concat ""

let rec indent i = match i with 
      | 0 -> "" 
      | _ -> indent (i - 1) ^ (if i mod 2 = 0 then "| " else "  ")

let rec dolist d f ls = match ls with 
  |  []     -> ""
  |  [a]    -> concat ["\n"; f (a, d + 1)]
  |  a :: r -> concat ["\n"; f (a, d + 1); ","; dolist d f r]


let rec string_of_typ t = match t with
  | Int -> "Int"
  | Bool -> "Bool"
  | String -> "String"
  | Unit -> "Unit"
  | FunType (typlist, rettyp) ->
      concat ["("; typlist |> List.map string_of_typ |> String.concat ","; ") => "; string_of_typ rettyp]
  | Any -> "_"

let string_of_typean t = match t with
  | None -> "NO_TY"
  | Some (typ, _) -> string_of_typ typ

let field (Field { name; typean; _ }, d) = 
    concat [ indent d; "("; name; ", "; string_of_typean typean; ")"]  


let string_of_label label =
  concat [ "{"; Label.label_to_list label |> String.concat ","; "}" ]


let binopname = function
| PlusBinOp  -> "PlusBinOp"
| MinusBinOp -> "MinusBinOp"
| TimesBinOp -> "TimesBinOp"
| DivideBinOp -> "DivideBinOp"
| LtBinOp  -> "LtBinOp"
| LeBinOp  -> "LeBinOp"
| GtBinOp  -> "GtBinOp"
| GeBinOp  -> "GeBinOp"
| EqBinOp  -> "EqBinOp"
| NeqBinOp -> "NeqBinOp"
| AndBinOp -> "AndBinOp" 
| OrBinOp -> "OrBinOp"
| ConcatBinOp -> "ConcatBinOp"

let unopname = function
| NegUnOp -> "NegUnOp"
| NotUnOp -> "NotUnOp" 


let as_string e0 =
  let rec string_of_exp (e, d) = match e with 
    | IntLit i -> concat [indent d; "IntLit("; string_of_int i; ")"]
    | BoolLit b -> concat [indent d; "BoolLit("; string_of_bool b; ")"]
    | StringLit (s, _) -> concat [indent d; "StringLit(\""; String.escaped s ; "\")"]
    | VarExp (x, _) -> concat [ indent d; "VarExp("; x; ")" ]
    | RaisedToExp { exp; label; _ } -> concat [ indent d; "RaisedToExp(\n"; string_of_exp (exp, d + 1); ",\n"; indent (d + 1); string_of_label label; ")" ]
    | BinOpExp { left; oper; right; _ } -> 
        concat [ indent d; "BinOpExp("; binopname oper; ",\n"; string_of_exp (left, d + 1); ",\n"; string_of_exp (right, d + 1); ")"]
    | UnOpExp { oper; exp; _ } ->
        concat [ indent d; "UnOpExp("; unopname oper; ",\n"; string_of_exp (exp, d + 1); ")"]
    | IfExp { test; thn; els; _ }    -> 
       concat [ indent d; "IfExp(\n"; string_of_exp (test, d + 1); ",\n"; string_of_exp (thn, d + 1);
                (match els with None -> "" | Some e -> ",\n" ^ string_of_exp (e, d + 1)) ; ")"]
    | CallExp { func; args; _ } -> 
        concat [ indent d
               ; "CallExp(\n"
               ; string_of_exp (func, d + 1)
               ; ",["
               ; dolist d string_of_exp (List.map fst args)
               ; "]"
               ]
    | LambdaExp { params: fielddata list ; body: exp ; _ } ->
        concat [indent d; "LambdaExp(["; dolist d field params; "], "; string_of_exp (body, d + 1); ")"]
    | LetExp {decls; body; _} -> 
        concat [indent d ; "LetExp(["; dolist d dec decls; "],\n"; string_of_exp (body, d + 1); ")"]

  and dec (theDec,d) = match theDec with 
    | FunDec l -> 
       let f ( Fdecl {name; params; result; body; _}, d ) = 
         concat [ indent d; "("; name; ",["; dolist d field params; "],\n"
               ; indent (d + 1)
               ; string_of_typean result
               ; ",\n"
               ; string_of_exp (body, d + 1)
               ; ")"] in
       concat [indent d; "FunctionDec["; dolist d f l; "]"]
    | ValDec { name; typean; init; _ } -> 
        concat [ indent d; "VarDec("; name; ", "
              ; string_of_typean typean
              ; ",\n"
              ; string_of_exp (init, d + 1)
              ; ")"]
  
  in string_of_exp (e0, 0)


let string_of_exp = as_string  
let print_exp out e = Format.fprintf out "%s\n" (as_string e)
