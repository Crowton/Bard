
open Ast_common
open Label


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


let unparse_label label =
    concat [ "{ "; label_to_list label |> String.concat ", "; " }" ]

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
