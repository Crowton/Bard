(** AST unparser printer implementation *)

open Ast


let concat = String.concat ""


let rec indent i = match i with 
      | 0 -> "" 
      | _ -> "    " ^ indent (i - 1)

let rec unparse_typ t = match t with
  | Int -> "Int"
  | Bool -> "Bool"
  | String -> "String"
  | Unit -> "Unit"
  | FunType (typlist, rettyp) ->
      concat ["("; typlist |> List.map unparse_typ |> String.concat ", "; ") => "; unparse_typ rettyp]
  | Any -> "Any"

let unparse_typean t = match t with
  | None -> ""
  | Some (typ, _) -> ": " ^ unparse_typ typ

let unparse_field (Field { name; typean; _ }) = 
    name ^ unparse_typean typean

let unparse_paramslist (params: fielddata list): string =
    params |> List.map unparse_field |> String.concat ", "

let unparse_binop = function
| PlusBinOp -> "+"
| MinusBinOp -> "-"
| TimesBinOp -> "*"
| DivideBinOp -> "*"
| LtBinOp -> "<"
| LeBinOp -> "<="
| GtBinOp -> ">"
| GeBinOp -> ">="
| EqBinOp -> "=="
| NeqBinOp -> "<>"
| AndBinOp -> "&" 
| OrBinOp -> "|"
| ConcatBinOp -> "^"

let unparse_unop = function
| NegUnOp -> "-"
| NotUnOp -> "~" 


let unparsed e0 =
  let rec unparse_exp (e, d) = match e with 
    | IntLit i -> string_of_int i
    | BoolLit b -> string_of_bool b
    | StringLit (s, _) -> "\"" ^ (String.escaped s) ^ "\""
    | VarExp (x, _) -> x
    | BinOpExp {left; oper; right; _ } -> 
        concat ["("; unparse_exp (left, d); " "; unparse_binop oper; " "; unparse_exp (right, d); ")"]
    | UnOpExp { oper; exp; _ } ->
        concat ["("; unparse_unop oper; unparse_exp (exp, d); ")"]
    | IfExp { test; thn; els; _ }    -> 
       concat ["if "; unparse_exp (test, d); " then "; unparse_exp (thn, d);
                (match els with None -> "" | Some e -> " else " ^ unparse_exp (e, d))]
    | CallExp { func; args; _ } -> 
        concat [ unparse_exp (func, d); "("; args |> List.map (fun a -> unparse_exp (fst a, d)) |> String.concat ", "; ")"]
    | LambdaExp { params: fielddata list ; body: exp ; _ } ->
        concat ["("; unparse_paramslist params; ") => "; unparse_exp (body, d)]
    | LetExp { decls; body; _ } ->
        concat ["let ";
                decls |> List.map (fun decl -> unparse_decl (decl, d + 1)) |> String.concat ("\n" ^ indent (d + 1));
                "\n"; indent d; "in\n"; indent (d + 1); unparse_exp (body, d + 1); "\n"; indent d; "end"]

  and unparse_decl (decl, d) = match decl with 
    | FunDec funcDecls ->
       funcDecls |> List.map (
          fun (Fdecl { name; params; result; body; _}) ->
              concat ["fun "; name; "("; unparse_paramslist params; ")"; unparse_typean result; " =\n"; indent (d + 1); unparse_exp (body, d + 1)]
       ) |> String.concat ("\n" ^ indent d)
    | ValDec { name; typean; init; _ } -> 
        concat ["val "; name; unparse_typean typean; " = "; unparse_exp (init, d)]
  
  in unparse_exp (e0, 0)


let unparse_exp = unparsed  
let print_exp out e = Format.fprintf out "%s\n" (unparsed e)
