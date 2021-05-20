open Bardcommon
open Ast
open Ast_common
open Unparser
open Label

module S = Map.Make(String)


type value
  = IntVal of int
  | BoolVal of bool
  | StringVal of string
  | UnitVal
  | ClosureVal of { params: fielddata list; restyp: typean; body: exp; env: env; defs: fundecldata list }
and env = (value * label) S.t


let value_to_string (v: value) : string = match v with
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
  | StringVal s -> s
  | UnitVal -> "unit"
  | ClosureVal { params; restyp; body; _ } ->
      concat ["("; unparse_paramslist params; ")"; unparse_typean restyp; " => "; unparse_exp body] 

let full_value_to_string (v: value * label * label) : string = match v with
  | (value, label, bl) -> concat [value_to_string value; "@"; unparse_label label; "%"; unparse_label bl]


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
let bindDefs (defs: fundecldata list) (env: env) (label: label) : env = 
  defs |> List.fold_left (
    fun env (Fdecl { name: id; params: fielddata list; result: typean; body: exp; _ }) ->
      S.add name (ClosureVal { params=params; restyp=result; body=body; env=env; defs=defs }, label) env
  ) env



(* Main run function.
   Initiate with empty enviroment.
   Catch errors and propegate to Main program *)
let eval_top exp out =

  (* Recursive evalueation function *)
  let rec eval (exp: exp) (env: env) (pc: label) (bl: label) : value * label * label = match exp with
    | IntLit i -> (IntVal i, pc, bl)
    | BoolLit b -> (BoolVal b, pc, bl)
    | StringLit (s, _) -> (StringVal s, pc, bl)
    | VarExp (x, p) ->
        (match env |> S.find_opt x with
          | None -> raise (InterpreterError ("Unbound Identifier " ^ x, p))
          | Some (v, l) -> (v, l, bl)
        )
    
    | RaisedToExp { exp: exp; label: label; _ } ->
        let v, baseLabel, bl' = eval exp env pc bl in
        (v, lub baseLabel label, bl')
    | SendExp { exp: exp; pos: pos } ->
        let v, l, bl' = eval exp env pc bl in
        if flows_to l bot && flows_to pc bot && flows_to bl' bot
          then (Format.fprintf out "%s\n" (full_value_to_string (v, l, bl'));
              (UnitVal, pc, lub bl' (lub l pc)))
          else raise (InterpreterError ("Cannot send at current label.", pos))
    
    | BinOpExp { left: exp; oper: binOp; right: exp; pos: pos } ->
        let leftVal, leftLabel, bl' = eval left env pc bl in
        let rightVal, rightLabel, bl'' = eval right env pc bl' in
        let int_eval = eval_binop_int leftVal rightVal pos in
        let int_bool_eval = eval_binop_int_to_bool leftVal rightVal pos in
        let bool_eval = eval_binop_bool leftVal rightVal pos in
        let string_eval = eval_binop_string leftVal rightVal pos in
        let resVal = match oper with
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
        in
        let resLabel = lub leftLabel rightLabel in
        (resVal, resLabel, lub bl'' resLabel)
    | UnOpExp { oper: unOp; exp: exp; pos: pos } ->
        let expVal, expLabel, bl' = eval exp env pc bl in
        let resVal = match oper with
          | NegUnOp -> eval_unop_int expVal pos (~-) "Negation"
          | NotUnOp -> eval_unop_bool expVal pos (not) "Not"
        in
        (resVal, expLabel, lub bl' expLabel)
    
    | IfExp { test: exp; thn: exp; els: exp option; pos: pos } ->
        (match eval test env pc bl with
          | (BoolVal true, l, bl') -> eval thn env (lub pc l) (lub bl' l)
          | (BoolVal false, l, bl') ->
              (match els with
                | None -> (UnitVal, lub pc l, (lub bl' l))
                | Some el -> eval el env (lub pc l) (lub bl' l)
              )
          | (v, _, _) -> raise (InterpreterError ("Type mismatch at if condition. Expected Bool, but got " ^ type_string_of_value v ^ ".", pos))
        )

    | CallExp { func: exp; args: (exp * pos) list; pos: pos } ->
        (match eval func env pc bl with
          | (ClosureVal { params; restyp; body; env=cenv; defs }, l, bl') ->
              (if List.length params != List.length args
                then raise (InterpreterError ("Wrong number of arguments supplied at function call. " ^
                                              "Expected " ^ string_of_int (List.length params) ^
                                              ", but got "^ string_of_int (List.length args) ^ ".", pos)));
              let env', bl'' = List.combine args params |> List.fold_left (
                  fun (env', bl') ((exp, pos), Field { name; typean; _ }) ->
                    let res, resLabel, bl'' = eval exp env pc bl' in
                    checkValueType res typean pos "function argument";
                    let resBl = match typean with
                      | None -> bl''
                      | Some _ -> lub bl'' resLabel
                    in
                    (S.add name (res, lub resLabel l) env', resBl)
                  ) (cenv, lub bl' l) in
              let env'' = bindDefs defs env' l in
              let res, resLabel, bl''' = eval body env'' (lub pc l) bl'' in
              checkValueType res restyp pos "function return value";
              let resBl = match restyp with
                | None -> bl'''
                | Some _ -> lub bl''' resLabel
              in
              (res, resLabel, resBl)
          | (v, _, _) -> raise (InterpreterError ("Calling non function type " ^ type_string_of_value v ^ ".", pos))
        )

    | LambdaExp { params: fielddata list ; body: exp ; _ } ->
        (ClosureVal { params=params; restyp=None; body=body; env=env; defs=[] }, pc, bl)

    | LetExp { decls: decl list; body: exp; _ } ->
        let (env', bl') = decls |> List.fold_left (fun (env, bl) decl -> eval_decl decl env pc bl) (env, bl) in
        eval body env' pc bl'

  (* Eval and update enviroment *)
  and eval_decl (decl: decl) (env: env) (pc: label) (bl: label) : env * label = match decl with
    | FunDec defs ->
        (bindDefs defs env pc, bl)
    | ValDec { name: id; typean: typean; init: exp; pos: pos } ->
        let res, resLabel, bl' = eval init env pc bl in
        checkValueType res typean pos "ValDecl";
        let resBl = match typean with
          | None -> bl'
          | Some _ -> lub bl' resLabel
        in
        (S.add name (res, resLabel) env, resBl)

  in

  (* Main body *)
  try (eval exp S.empty bot bot, None)
  with
    | (InterpreterError (msg, pos)) -> ((UnitVal, bot, bot), Some (msg, pos))
