open Tigercommon.Ast
open Tigercommon.Pretty_ast

module S = Map.Make(String)

type value
  = IntVal of int
  | BoolVal of bool
  | StringVal of string
  | UnitVal
  | ClosureVal of { args: (id * typean) list; restyp: typean; body: exp; env: env; defs: fundecldata list }
and env = value S.t


let value_to_string v = match v with
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
  | StringVal s -> s
  | UnitVal -> "unit"
  | ClosureVal { args; restyp; body; _ } ->
      concat ["("; args |> List.map (fun (id, typean) -> id ^ ": " ^ string_of_typean typean) |> String.concat ",";
              "): "; string_of_typean restyp;
              string_of_exp body] 


let typ_of_typean t = match t with
  | None -> Any
  | Some (ty, _) -> ty

let type_of_val v = match v with
  | IntVal _ -> Int
  | BoolVal _ -> Bool
  | StringVal _ -> String
  | UnitVal -> Unit
  | ClosureVal { args; restyp; _ } ->
      FunType (
        args |> List.map (fun t -> typ_of_typean (snd t)),
        typ_of_typean restyp
      )

let type_string_of_value v =
  string_of_typ (type_of_val v)


exception InterpreterError of string * pos


let eval_binop_int leftVal rightVal pos f =
  match (leftVal, rightVal) with
    | (IntVal l, IntVal r) -> IntVal (f l r)
    | _ -> raise (InterpreterError ("Type mismatch at Plus. Expected (Int, Int), but got ("
                              ^ type_string_of_value leftVal ^ ", "
                              ^ type_string_of_value rightVal ^ ").", pos))


let rec eval (exp: exp) (env: env) : value = match exp with
  | IntLit i -> IntVal i
  | BoolLit b -> BoolVal b
  | StringLit (s, _) -> StringVal s
  | VarExp (x, p) ->
      (match env |> S.find_opt x with
        | None -> raise (InterpreterError ("Unbound Identifier " ^ x, p))
        | Some v -> v
      )
  | BinOpExp { left: exp; oper: binOp; right: exp; pos: pos } ->
      let leftVal = eval left env in
      let rightVal = eval right env in
      let int_eval = eval_binop_int leftVal rightVal pos in
      (match oper with
        | PlusBinOp -> int_eval (+)
        | MinusBinOp -> int_eval (-)
        | TimesBinOp -> int_eval ( * )
        | DivideBinOp -> if (rightVal = IntVal 0) then raise (InterpreterError ("ZeroDivisionError", pos)); int_eval (/)
      )
  | UnOpExp { oper: unOp; exp: exp; pos: pos } -> IntVal 0
  | IfExp { test: exp; thn: exp; els: exp option; pos: pos } -> IntVal 0
  | CallExp { func: exp; args: (exp * pos) list; pos: pos } -> IntVal 0
  | LetExp { decls: decl list; body: exp; pos: pos } -> IntVal 0


let eval_top exp =
  try (eval exp S.empty, None)
  with
    | (InterpreterError (msg, pos)) -> (UnitVal, Some (msg, pos))
