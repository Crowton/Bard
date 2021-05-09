open Bardcommon
open Ast
open Unparser

module S = Map.Make(String)

type value
  = IntVal of int
  | BoolVal of bool
  | StringVal of string
  | UnitVal
  | ClosureVal of { params: fielddata list; restyp: typean; body: exp; env: env; defs: fundecldata list }
and env = value S.t


let value_to_string (v: value) : string = match v with
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
  | StringVal s -> s
  | UnitVal -> "unit"
  | ClosureVal { params; restyp; body; _ } ->
      concat ["("; unparse_paramslist params; ")"; unparse_typean restyp; " => "; unparse_exp body] 


let typ_of_typean (t: typean) : typ = match t with
  | None -> Any
  | Some (ty, _) -> ty

let type_of_val (v: value) : typ = match v with
  | IntVal _ -> Int
  | BoolVal _ -> Bool
  | StringVal _ -> String
  | UnitVal -> Unit
  | ClosureVal { params; restyp; _ } ->
      FunType (
        params |> List.map (fun (Field { typean; _ }) -> typ_of_typean typean),
        typ_of_typean restyp
      )

let type_string_of_value (v: value) : string =
  unparse_typ (type_of_val v)


exception InterpreterError of string * pos


(* Binary operator execution helpers *)
let binop_err fName expType leftVal rightVal pos =
  raise (InterpreterError ("Type mismatch at " ^ fName ^ ". Expected (" ^ expType ^ ", " ^ expType ^ "), but got ("
                            ^ type_string_of_value leftVal ^ ", "
                            ^ type_string_of_value rightVal ^ ").", pos))

let eval_binop_int leftVal rightVal pos f fName =
  match (leftVal, rightVal) with
    | (IntVal l, IntVal r) -> IntVal (f l r)
    | _ -> binop_err fName "Int" leftVal rightVal pos

let eval_binop_int_to_bool leftVal rightVal pos f fName =
  match (leftVal, rightVal) with
    | (IntVal l, IntVal r) -> BoolVal (f l r)
    | _ -> binop_err fName "Int" leftVal rightVal pos

(* let eval_binop_compare leftVal rightVal pos (f: 'a -> 'a -> bool) fName =
  match (leftVal, rightVal) with
    | (IntVal l, IntVal r) -> BoolVal (f l r)
    | (BoolVal l, BoolVal r) -> BoolVal (f l r)
    | (StringVal l, StringVal r) -> BoolVal (f l r)
    | _ -> binop_err fName "Int/String/Bool" leftVal rightVal pos *)

let eval_binop_bool leftVal rightVal pos f fName =
  match (leftVal, rightVal) with
    | (BoolVal l, BoolVal r) -> BoolVal (f l r)
    | _ -> binop_err fName "Bool" leftVal rightVal pos

let eval_binop_string leftVal rightVal pos f fName =
  match (leftVal, rightVal) with
    | (StringVal l, StringVal r) -> StringVal (f l r)
    | _ -> binop_err fName "String" leftVal rightVal pos


(* Unary operator execution helpers *)
let unnop_err fName expType value pos =
  raise (InterpreterError ("Type mismatch at " ^ fName ^ ". Expected " ^ expType ^ ", but got "
                            ^ type_string_of_value value ^ ".", pos))

let eval_unop_int value pos f fName =
  match value with
    | IntVal i -> IntVal (f i)
    | _ -> unnop_err fName "Int" value pos

let eval_unop_bool value pos f fName =
  match value with
    | BoolVal b -> BoolVal (f b)
    | _ -> unnop_err fName "Bool" value pos


(* Dynamic type checks *)
let checkValueType (value: value) (typean: typean) (pos: pos) (fName: string) : unit =
  match typean with
    | None -> () (* Do nothing *)
    | Some (ty, _) ->
        let realTy = type_of_val value in
        if realTy <> ty
        then raise (InterpreterError ("Type mismatch at " ^ fName ^ ". Expected "
                                      ^ unparse_typ ty ^ ", but got "
                                      ^ unparse_typ realTy ^ ".", pos))


(* Binding ad rebinding functions *)
let bindDefs (defs: fundecldata list) (env: env) : env = 
  defs |> List.fold_left (
    fun env (Fdecl { name: id; params: fielddata list; result: typean; body: exp; _ }) ->
      S.add name (ClosureVal { params=params; restyp=result; body=body; env=env; defs=defs }) env
  ) env


(* Recursive evalueation function *)
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
      let int_bool_eval = eval_binop_int_to_bool leftVal rightVal pos in
      let bool_eval = eval_binop_bool leftVal rightVal pos in
      let string_eval = eval_binop_string leftVal rightVal pos in
      (match oper with
        | PlusBinOp -> int_eval (+) "Plus"
        | MinusBinOp -> int_eval (-) "Minus"
        | TimesBinOp -> int_eval ( * ) "Times"
        | DivideBinOp -> if rightVal = IntVal 0 then raise (InterpreterError ("ZeroDivisionError", pos)); int_eval (/) "Divide"
        | LtBinOp -> int_bool_eval (<) "Less"
        | LeBinOp -> int_bool_eval (<=) "Less equals"
        | GtBinOp -> int_bool_eval (>) "Greater"
        | GeBinOp -> int_bool_eval (>=) "Greater equals"
        | EqBinOp -> int_bool_eval (=) "Equals"
        | NeqBinOp -> int_bool_eval (<>) "Not equals"
        | AndBinOp -> bool_eval (&&) "And"
        | OrBinOp -> bool_eval (||) "Or"
        | ConcatBinOp -> string_eval (^) "Concatenation"
      )
  | UnOpExp { oper: unOp; exp: exp; pos: pos } ->
      let expVal = eval exp env in
      (match oper with
        | NegUnOp -> eval_unop_int expVal pos (~-) "Negation"
        | NotUnOp -> eval_unop_bool expVal pos (not) "Not"
      )
  
  | IfExp { test: exp; thn: exp; els: exp option; pos: pos } ->
      (match eval test env with
        | BoolVal true -> eval thn env
        | BoolVal false ->
            (match els with
              | None -> UnitVal
              | Some el -> eval el env
            )
        | v -> raise (InterpreterError ("Type mismatch at if condition. Expected Bool, but got " ^ type_string_of_value v ^ ".", pos))
      )

  | CallExp { func: exp; args: (exp * pos) list; pos: pos } ->
      (match eval func env with
        | ClosureVal { params; restyp; body; env=cenv; defs } ->
            (if List.length params != List.length args
              then raise (InterpreterError ("Wrong number of arguments supplied at function call. " ^
                                            "Expected " ^ string_of_int (List.length params) ^
                                            ", but got "^ string_of_int (List.length args) ^ ".", pos)));
            let env' = List.combine args params |> List.fold_left (
                fun env' ((exp, pos), Field { name; typean; _ }) ->
                  let res = eval exp env in
                  checkValueType res typean pos "function argument";
                  S.add name res env'
                ) cenv in
            let env'' = bindDefs defs env' in
            let res = eval body env'' in
            checkValueType res restyp pos "function return value";
            res
        | v -> raise (InterpreterError ("Calling non function type " ^ type_string_of_value v ^ ".", pos))
      )

  | LambdaExp { params: fielddata list ; body: exp ; _ } ->
      ClosureVal { params=params; restyp=None; body=body; env=env; defs=[] }

  | LetExp { decls: decl list; body: exp; _ } ->
      let env' = decls |> List.fold_left (fun env decl -> eval_decl decl env) env in
      eval body env'

(* Eval and update enviroment *)
and eval_decl (decl: decl) (env: env) : env = match decl with
  | FunDec defs ->
      bindDefs defs env
  | ValDec { name: id; typean: typean; init: exp; pos: pos } ->
      let res = eval init env in
      checkValueType res typean pos "ValDecl";
      S.add name res env


(* Main run function.
   Initiate with empty enviroment.
   Catch errors and propegate to Main program *)
let eval_top exp =
  try (eval exp S.empty, None)
  with
    | (InterpreterError (msg, pos)) -> (UnitVal, Some (msg, pos))
