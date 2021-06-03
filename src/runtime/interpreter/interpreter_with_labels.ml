open Bardcommon
open Ast
open Ast_common
open Unparser
open Unparser_common
open Label

module S = Map.Make(String)


type value
  = IntVal of int
  | BoolVal of bool
  | StringVal of string
  | UnitVal
  | ClosureVal of { params: fielddata list; restyp: typean; body: exp; env: env; defs: fundecldata list }
and env = (value * label * label) S.t


let value_to_string (v: value) : string = match v with
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
  | StringVal s -> s
  | UnitVal -> "unit"
  | ClosureVal { params; restyp; body; _ } ->
      concat ["("; unparse_paramslist params; ")"; unparse_typean restyp; " => "; unparse_exp body] 

let full_value_to_string (value: value) (label: label) (typelabel: label) : string = 
  concat [value_to_string value; "@"; unparse_label label; "%"; unparse_label typelabel]


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


let typ_match_value typ value: bool =
  let valtyp = type_of_val value in
  let rec check typ valtyp =
    match (typ, valtyp) with
    | (Any, _) -> true
    | (typ, valtyp) when typ = valtyp -> true
    | (FunType (paramtypes, rettype), FunType (valparamtypes, valrettype)) when List.length paramtypes != List.length valparamtypes ->
        (List.combine paramtypes valparamtypes |> List.for_all (fun (typ, valtyp) -> check typ valtyp)) && check rettype valrettype
    | _ -> false
  in
  check typ valtyp


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


(* Binding and rebinding functions *)
let bindDefs (defs: fundecldata list) (env: env) (label: label) : env = 
  defs |> List.fold_left (
    fun env (Fdecl { name: id; params: fielddata list; result: typean; body: exp; _ }) ->
      S.add name (ClosureVal { params=params; restyp=result; body=body; env=env; defs=defs }, label, bot) env
  ) env



(* Main run function.
   Initiate with empty enviroment.
   Catch errors and propegate to Main program *)
let eval_top exp mailbox out =

  (* Recursive evalueation function *)
  let rec eval (exp: exp) (env: env) (pc: label) (bl: label) : value * label * label * label = match exp with
    | IntLit i -> (IntVal i, pc, bot, bl)
    | BoolLit b -> (BoolVal b, pc, bot, bl)
    | StringLit (s, _) -> (StringVal s, pc, bot, bl)
    | VarExp (x, p) ->
        (match env |> S.find_opt x with
          | None -> raise (InterpreterError ("Unbound Identifier " ^ x, p))
          | Some (v, l, tl) -> (v, l, tl, bl)
        )
    
    | RaisedToExp { exp: exp; label: label; _ } ->
        let v, baseLabel, basetypelabel, bl' = eval exp env pc bl in
        (v, lub baseLabel label, basetypelabel, bl')
    | SendExp { exp: exp; pos: pos } ->
        let v, l, lt, bl' = eval exp env pc bl in
        if flows_to l bot && flows_to lt bot && flows_to pc bot && flows_to bl' bot
          then (Format.fprintf out "%s\n" (full_value_to_string v l lt);
              (UnitVal, pc, bot, bl'))
          else raise (InterpreterError ("Cannot send at current label.", pos))
    | ReceiveExp { typ; pos; } ->
        if flows_to pc bot && flows_to bl bot
          then let (value, label, typeLabel), raiseBy = mailbox#get typ pos in
               let actualRaiseBy = match typ with
                   | None -> bot
                   | Some _ -> raiseBy
               in
               (value, label, typeLabel, lub bl actualRaiseBy)
          else raise (InterpreterError ("Cannot receive at current label.", pos))
    | BlockDeclExp { label; pos } ->
        if flows_to pc label && flows_to bl label
          then (UnitVal, pc, bot, pc)
          else raise (InterpreterError ("Cannot declassify at current label.", pos))
    
    | BinOpExp { left: exp; oper: binOp; right: exp; pos: pos } ->
        let leftVal, leftLabel, leftTypeLabel, bl' = eval left env pc bl in
        let rightVal, rightLabel, rightTypeLabel, bl'' = eval right env pc bl' in
        let int_eval = eval_binop_int leftVal rightVal pos in
        let int_bool_eval = eval_binop_int_to_bool leftVal rightVal pos in
        let bool_eval = eval_binop_bool leftVal rightVal pos in
        let string_eval = eval_binop_string leftVal rightVal pos in
        let resVal, extraRaise = match oper with
          | PlusBinOp -> int_eval (+) "Plus", bot
          | MinusBinOp -> int_eval (-) "Minus", bot
          | TimesBinOp -> int_eval ( * ) "Times", bot
          | DivideBinOp -> if rightVal = IntVal 0 then raise (InterpreterError ("ZeroDivisionError", pos)); int_eval (/) "Divide", rightLabel
          | LtBinOp -> int_bool_eval (<) "Less", bot
          | LeBinOp -> int_bool_eval (<=) "Less equals", bot
          | GtBinOp -> int_bool_eval (>) "Greater", bot
          | GeBinOp -> int_bool_eval (>=) "Greater equals", bot
          | EqBinOp -> int_bool_eval (=) "Equals", bot
          | NeqBinOp -> int_bool_eval (<>) "Not equals", bot
          | AndBinOp -> bool_eval (&&) "And", bot
          | OrBinOp -> bool_eval (||) "Or", bot
          | ConcatBinOp -> string_eval (^) "Concatenation", bot
        in
        let resLabel = lub leftLabel rightLabel in
        let resTypeLabel = lub leftTypeLabel rightTypeLabel in
        (resVal, resLabel, resTypeLabel, lub (lub bl'' resTypeLabel) extraRaise)
    | UnOpExp { oper: unOp; exp: exp; pos: pos } ->
        let expVal, expLabel, expTypeLabel, bl' = eval exp env pc bl in
        let resVal = match oper with
          | NegUnOp -> eval_unop_int expVal pos (~-) "Negation"
          | NotUnOp -> eval_unop_bool expVal pos (not) "Not"
        in
        (resVal, expLabel, expTypeLabel, lub bl' expTypeLabel)
    
    | IfExp { test: exp; thn: exp; els: exp option; pos: pos } ->
        let testVal, testLabel, testTypeLabel, bl' = eval test env pc bl in
        let newPc, newBl = lub pc testLabel, lub bl' testTypeLabel in
        let resVal, resLabel, resTypeLabel, bl'' = match testVal with
          | BoolVal true -> eval thn env newPc newBl
          | BoolVal false ->
              (match els with
                | None -> (UnitVal, newPc, bot, newBl)
                | Some el -> eval el env newPc newBl
              )
          | v -> raise (InterpreterError ("Type mismatch at if condition. Expected Bool, but got " ^ type_string_of_value v ^ ".", pos))
        in
        (resVal, resLabel, lub resTypeLabel testLabel, lub bl'' testLabel)

    | CallExp { func: exp; args: (exp * pos) list; pos: pos } ->
        (match eval func env pc bl with
          | (ClosureVal { params; restyp; body; env=cenv; defs }, l, lt, bl') ->
              (if List.length params != List.length args
                then raise (InterpreterError ("Wrong number of arguments supplied at function call. " ^
                                              "Expected " ^ string_of_int (List.length params) ^
                                              ", but got "^ string_of_int (List.length args) ^ ".", pos)));
              let env', bl'' = List.combine args params |> List.fold_left (
                  fun (env', bl') ((exp, pos), Field { name; typean; _ }) ->
                    let res, resLabel, resTypeLabel, bl'' = eval exp env pc bl' in
                    checkValueType res typean pos "function argument";
                    (S.add name (res, lub resLabel l, resTypeLabel) env', lub bl'' resTypeLabel)
                  ) (cenv, lub bl' lt) in
              let env'' = bindDefs defs env' l in
              let res, resLabel, resTypeLabel, bl''' = eval body env'' (lub pc l) bl'' in
              checkValueType res restyp pos "function return value";
              (res, resLabel, resTypeLabel, lub (lub bl''' l) resTypeLabel)
          | (v, _, _, _) -> raise (InterpreterError ("Calling non function type " ^ type_string_of_value v ^ ".", pos))
        )

    | LambdaExp { params: fielddata list ; body: exp ; _ } ->
        (ClosureVal { params=params; restyp=None; body=body; env=env; defs=[] }, pc, bot, bl)

    | LetExp { decls: decl list; body: exp; _ } ->
        let (env', bl') = decls |> List.fold_left (fun (env, bl) decl -> eval_decl decl env pc bl) (env, bl) in
        eval body env' pc bl'

  (* Eval and update enviroment *)
  and eval_decl (decl: decl) (env: env) (pc: label) (bl: label) : env * label = match decl with
    | FunDec defs ->
        (bindDefs defs env pc, bl)
    | ValDec { name: id; typean: typean; init: exp; pos: pos } ->
        let res, resLabel, resTypeLabel, bl' = eval init env pc bl in
        checkValueType res typean pos "ValDecl";
        let resBl = match typean with
          | None -> bl'
          | Some _ -> lub bl' resTypeLabel
        in
        (S.add name (res, resLabel, resTypeLabel) env, resBl)

  in

  (* Main body *)
  eval exp S.empty bot bot
