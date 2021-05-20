open Bardcommon

module A = Ast
module T = Typed_ast
module L = Label

open Ast_common

module S = Map.Make(String)
type tenv = typ S.t

exception TypeError of string * pos


let check_binop (pos: pos) (leftType: typ) (rightType: typ) (demandType: typ): bool =
  match (leftType, rightType) with
  | (Any, _) | (_, Any) -> true
  | (t1, t2) when t1 = demandType && t2 = demandType -> false
  | _ -> let demandTypeString = Unparser.unparse_typ demandType in
         raise (TypeError ("Type mismatch at binary operator. Expected (" ^ demandTypeString ^ ", " ^ demandTypeString ^ "), but got ("
                            ^ Unparser.unparse_typ leftType ^ ", "
                            ^ Unparser.unparse_typ rightType ^ ").", pos))

let check_unop (pos: pos) (typ: typ) (demandType: typ): bool =
  match typ with
  | Any -> true
  | t when t = demandType -> false
  | _ -> raise (TypeError ("Type mismatch at unary operator. Expected " ^ Unparser.unparse_typ demandType ^ ", but got "
                            ^ Unparser.unparse_typ typ ^ ".", pos))


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
      (resType, T.BinOpExp { left=leftTexp; oper=oper; right=rightTexp; canfail=canFail; pos=pos })

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
       | _ -> raise (TypeError ("Condition at if expected Boolean but got " ^ Unparser.unparse_typ testT, pos))
      );
      let thnT, thnTexp = typecheck thn tenv in
      let elsT, elsTexp =
          match els with
          | None -> (Unit, None)
          | Some e -> let eT, eTexp = typecheck e tenv in (eT, Some eTexp)
      in
      let resT = if thnT = elsT then thnT else Any in
      (resT, T.IfExp { test=testTexp; thn=thnTexp; els=elsTexp; pos=pos })

  (*| A.CallExp { func: exp; args: (exp * pos) list; pos: pos }

  | A.LambdaExp { params: fielddata list ; body: exp ; pos: pos }

  | A.LetExp { decls: decl list; body: exp; pos: pos } *)


let typecheck_top (exp: A.exp): (typ * T.texp * ((string * pos) option)) =
  try
    let t, texp = typecheck exp S.empty in
    (t, texp, None)
  with
    | (TypeError (msg, pos)) -> (Int, T.IntLit (-1), Some (msg, pos))
