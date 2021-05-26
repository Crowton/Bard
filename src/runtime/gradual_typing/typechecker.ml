open Bardcommon

module A = Ast
module T = Typed_ast
module L = Label

open Ast_common

module S = Map.Make(String)
type tenv = typ S.t

exception TypeError of string * pos


let check_binop (pos: pos) (leftType: typ) (rightType: typ) (demandType: typ): (bool * bool) =
  match (leftType, rightType) with
  | (Any, Any) -> (true, true)
  | (Any, t2) when t2 = demandType -> (true, false)
  | (t1, Any) when t1 = demandType -> (false, true)
  | (t1, t2) when t1 = demandType && t2 = demandType -> (false, false)
  | _ -> let demandTypeString = Unparser_common.unparse_typ demandType in
         raise (TypeError ("Type mismatch at binary operator. Expected (" ^ demandTypeString ^ ", " ^ demandTypeString ^ "), but got ("
                            ^ Unparser_common.unparse_typ leftType ^ ", "
                            ^ Unparser_common.unparse_typ rightType ^ ").", pos))

let check_unop (pos: pos) (typ: typ) (demandType: typ): bool =
  match typ with
  | Any -> true
  | t when t = demandType -> false
  | _ -> raise (TypeError ("Type mismatch at unary operator. Expected " ^ Unparser_common.unparse_typ demandType ^ ", but got "
                            ^ Unparser_common.unparse_typ typ ^ ".", pos))


let type_of_typean typean: typ =
  match typean with
  | None -> Any
  | Some (t, _) -> t


let rec typecheck (exp: A.exp) (tenv: tenv): (typ * T.texp) = match exp with
  | A.IntLit i -> (Int, T.IntLit i)
  | A.BoolLit i -> (Bool, T.BoolLit i)
  | A.StringLit (i, p) -> (String, T.StringLit (i, p))
  | A.VarExp (x, pos) ->
      (match tenv |> S.find_opt x with
          | None -> raise (TypeError ("Unbound Identifier " ^ x, pos))
          | Some t -> (t, T.VarExp (x, pos))
        )

  | A.RaisedToExp { exp: A.exp; label: L.label; pos: pos } ->
      let t, texp = typecheck exp tenv in
      (t, T.RaisedToExp { texp=texp; label=label; pos=pos })
  | A.SendExp { exp: A.exp; pos: pos } ->
      let _, texp = typecheck exp tenv in
      (Unit, T.SendExp { texp=texp; pos=pos })

  | A.BinOpExp { left: A.exp; oper: binOp; right: A.exp; pos: pos } ->
      let leftT, leftTexp = typecheck left tenv in
      let rightT, rightTexp = typecheck right tenv in
      let check = check_binop pos leftT rightT in
      let canFail, resType = match oper with
                             | PlusBinOp | MinusBinOp | TimesBinOp | DivideBinOp
                                -> (check Int, Int)
                             | LtBinOp | LeBinOp | GtBinOp | GeBinOp | EqBinOp | NeqBinOp
                                -> (check Int, Bool)
                             | AndBinOp | OrBinOp
                                -> (check Bool, Bool)
                             | ConcatBinOp
                                -> (check String, String)
      in
      (resType, T.BinOpExp { left=leftTexp; oper=oper; right=rightTexp; leftcanfail=fst canFail; rightcanfail=snd canFail; pos=pos })

  | A.UnOpExp { oper: unOp; exp: A.exp; pos: pos } ->
      let typ, texp = typecheck exp tenv in
      let check = check_unop pos typ in
      let canFail, resType = match oper with
                             | NegUnOp -> (check Int, Int)
                             | NotUnOp -> (check Bool, Bool)
      in
      (resType, T.UnOpExp { oper=oper; texp=texp; canfail=canFail; pos=pos })


  | A.IfExp { test: A.exp; thn: A.exp; els: A.exp option; pos: pos } ->
      let testT, testTexp = typecheck test tenv in
      (match testT with
       | Bool | Any -> () (* Do nothing. "can fail" but label must be raised anyways *)
       | _ -> raise (TypeError ("Condition at if expected Boolean but got " ^ Unparser_common.unparse_typ testT, pos))
      );
      let thnT, thnTexp = typecheck thn tenv in
      let elsT, elsTexp =
          match els with
          | None -> (Unit, None)
          | Some e -> let eT, eTexp = typecheck e tenv in (eT, Some eTexp)
      in
      let resT = if thnT = elsT then thnT else Any in
      (resT, T.IfExp { test=testTexp; thn=thnTexp; els=elsTexp; pos=pos })

  | A.CallExp { func: A.exp; args: (A.exp * pos) list; pos: pos } ->
      let funcT, funcTexp = typecheck func tenv in
      let argsTyped = args |> List.map (fun (arg, pos) -> (typecheck arg tenv), pos) in
      let retType, argsStrongTyped = 
          match funcT with
          | FunType (paramsTypes, retType) ->
              (if List.length paramsTypes != List.length args
                then raise (TypeError ("Wrong number of arguments supplied at function call. " ^
                                       "Expected " ^ string_of_int (List.length paramsTypes) ^
                                       ", but got "^ string_of_int (List.length args) ^ ".", pos)));
              let argsStrongTyped = List.combine argsTyped paramsTypes |> List.map (fun (((argType, arg), pos), parType) ->
                  let checkTypeCanFail = 
                      match (argType, parType) with
                      | (_, Any) -> false
                      | (Any, _) -> true
                      | (t, t') when t = t' -> false
                      | _ -> raise (TypeError ("Wrong type of argument. " ^
                                               "Expected " ^ Unparser_common.unparse_typ parType ^
                                               ", but got " ^ Unparser_common.unparse_typ argType ^ ".", pos))
                  in
                  (arg, checkTypeCanFail, pos)
              ) in
              (retType, argsStrongTyped)
          | Any ->
              (Any, argsTyped |> List.map (fun ((_, a), p) -> (a, true, p)))
          | _ -> raise (TypeError ("Calling non function " ^ Unparser_common.unparse_typ funcT, pos))
      in
      (retType, T.CallExp { func=funcTexp; args=argsStrongTyped; pos=pos })

  | A.LambdaExp { params: A.fielddata list; body: A.exp; pos: pos } ->
      let tenv' = params |> List.fold_left (fun tenv' (A.Field { name: id; typean: typean; _ }) -> tenv' |> S.add name (type_of_typean typean)) tenv in
      let bodyT, bodyTexp = typecheck body tenv' in
      let paramsTypes = params |> List.map (fun (A.Field { typean: typean; _ }) -> type_of_typean typean) in
      let params' = params |> List.map (fun (A.Field { name: id; typean: typean; pos: pos }) -> T.Field { name=name; typean=typean; pos=pos }) in
      (FunType (paramsTypes, bodyT), T.LambdaExp { params=params'; body=bodyTexp; pos=pos })

  | A.LetExp { decls: A.decl list; body: A.exp; pos: pos } ->
      let tenv', decls'rev = decls |> List.fold_left (fun (tenv, declsrev) decl -> let tenv', decl' = typecheck_decl decl tenv in (tenv', decl' :: declsrev)) (tenv, []) in
      let bodyT, bodyTexp = typecheck body tenv' in
      (bodyT, T.LetExp { decls=List.rev decls'rev; body=bodyTexp; pos=pos })

and typecheck_decl (decl: A.decl) (tenv: tenv): (tenv * T.decl) = match decl with
  | A.FunDec fundecldatalist ->
      let tenv' = fundecldatalist |> List.fold_left (fun tenv (A.Fdecl { name: id; params: A.fielddata list; result: typean; _ }) ->
          tenv |> S.add name (FunType (params |> List.map (fun (A.Field { typean: typean; _ }) -> type_of_typean typean), type_of_typean result))
      ) tenv in
      let fundecldatalist' = fundecldatalist |> List.map (fun (A.Fdecl { name: id; params: A.fielddata list; result: typean; body: A.exp; pos: pos }) ->
          let tenv'' = params |> List.fold_left (fun tenv' (A.Field { name: id; typean: typean; _ }) -> tenv' |> S.add name (type_of_typean typean)) tenv' in
          let bodyT, bodyTexp = typecheck body tenv'' in
          let resCheckCanFail =
              (match (result, bodyT) with
               | (None, _) -> false
               | (Some (t, _), t') when t = t' -> false
               | (Some _, Any) -> true
               | (Some (t, _), _) -> raise (TypeError ("Returntype at fun expected " ^ Unparser_common.unparse_typ t ^ " but got " ^ Unparser_common.unparse_typ bodyT, pos)))
          in
          let params' = params |> List.map (fun (A.Field { name: id; typean: typean; pos: pos }) -> T.Field { name=name; typean=typean; pos=pos }) in
          T.Fdecl { name=name; params=params'; result=result; body=bodyTexp; rescanfail=resCheckCanFail; pos=pos }
      ) in
      (tenv', T.FunDec fundecldatalist')
  | A.ValDec { name: id; typean: typean; init: A.exp; pos: pos } ->
      let initT, initTexp = typecheck init tenv in
      let resTy, canfail = match typean with
        | None -> (initT, false)
        | Some (t, _) ->
            (match initT with
             | Any -> (t, true)
             | t' when t' = t -> (t, false)
             | _ -> raise (TypeError ("Typeannotation at val declaration expected " ^ Unparser_common.unparse_typ t ^ " but got " ^ Unparser_common.unparse_typ initT, pos)))
      in
      (S.add name resTy tenv, T.ValDec { name=name; typean=typean; init=initTexp; canfail=canfail; pos=pos })



let typecheck_top (exp: A.exp): (typ * T.texp * ((string * pos) option)) =
  try
    let t, texp = typecheck exp S.empty in
    (t, texp, None)
  with
    | (TypeError (msg, pos)) -> (Int, T.IntLit (-1), Some (msg, pos))
