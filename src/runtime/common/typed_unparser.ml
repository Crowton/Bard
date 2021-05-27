(** Typed AST unparser printer implementation *)

open Typed_ast
open Unparser_common



let unparse_field (Field { name; typean; _ }) = 
    name ^ unparse_typean typean

let unparse_paramslist (params: fielddata list): string =
    params |> List.map unparse_field |> String.concat ", "


let unparsed e0 =
  let rec unparse_texp (e, d) = match e with 
    | IntLit i -> string_of_int i
    | BoolLit b -> string_of_bool b
    | StringLit (s, _) -> "\"" ^ (String.escaped s) ^ "\""
    | VarExp (x, _) -> x
    | RaisedToExp { texp; label; _ } -> concat [ unparse_texp (texp, d); " raisedTo "; unparse_label label ]
    | SendExp { texp; _ } -> concat [ "send "; unparse_texp (texp, d) ]
    | ReceiveExp { typ; _ } -> concat [ "receive "; unparse_typean typ ]
    | BinOpExp { left; oper; right; _ } -> 
        concat ["("; unparse_texp (left, d); " "; unparse_binop oper; " "; unparse_texp (right, d); ")"]
    | UnOpExp { oper; texp; _ } ->
        concat ["("; unparse_unop oper; unparse_texp (texp, d); ")"]
    | IfExp { test; thn; els; _ }    -> 
       concat ["if "; unparse_texp (test, d); " then "; unparse_texp (thn, d);
                (match els with None -> "" | Some e -> " else " ^ unparse_texp (e, d))]
    | CallExp { func; args; _ } -> 
        concat [ unparse_texp (func, d); "("; args |> List.map (fun (arg, _, _) -> unparse_texp (arg, d)) |> String.concat ", "; ")"]
    | LambdaExp { params: fielddata list ; body: texp ; _ } ->
        concat ["("; unparse_paramslist params; ") => "; unparse_texp (body, d)]
    | LetExp { decls; body; _ } ->
        concat ["let ";
                decls |> List.map (fun decl -> unparse_decl (decl, d + 1)) |> String.concat ("\n" ^ indent (d + 1));
                "\n"; indent d; "in\n"; indent (d + 1); unparse_texp (body, d + 1); "\n"; indent d; "end"]

  and unparse_decl (decl, d) = match decl with 
    | FunDec funcDecls ->
       funcDecls |> List.map (
          fun (Fdecl { name; params; result; body; _}) ->
              concat ["fun "; name; "("; unparse_paramslist params; ")"; unparse_typean result; " =\n"; indent (d + 1); unparse_texp (body, d + 1)]
       ) |> String.concat ("\n" ^ indent d)
    | ValDec { name; typean; init; _ } -> 
        concat ["val "; name; unparse_typean typean; " = "; unparse_texp (init, d)]
  
  in unparse_texp (e0, 0)


let unparse_texp = unparsed  
let print_exp out e = Format.fprintf out "%s\n" (unparsed e)
