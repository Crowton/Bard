open Ast_common

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
  | Any -> "Any"

let string_of_typean t = match t with
  | None -> "NO_TY"
  | Some (typ, _) -> string_of_typ typ

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
