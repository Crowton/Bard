type pos = Lexing.position

type binOp
  = PlusBinOp | MinusBinOp | TimesBinOp | DivideBinOp
  | LtBinOp | LeBinOp | GtBinOp | GeBinOp | EqBinOp | NeqBinOp
  | AndBinOp | OrBinOp
  | ConcatBinOp

type unOp
  = NegUnOp | NotUnOp 

type id = string

type typ
  = Int | Bool | String | Unit
  | FunType of (typ list) * typ
  | Any

type typean
  = (typ * pos) option

type exp
  = IntLit of int
  | BoolLit of bool
  | StringLit of string * pos
  | VarExp of id * pos
  | BinOpExp of { left: exp; oper: binOp; right: exp; pos: pos }
  | UnOpExp of { oper: unOp; exp: exp; pos: pos }
  | IfExp of { test: exp; thn: exp; els: exp option; pos: pos }
  | CallExp of { func: exp; args: (exp * pos) list; pos: pos }
  | LambdaExp of { params: fielddata list ; body: exp ; pos: pos }
  | LetExp of { decls: decl list; body: exp; pos: pos }
and decl
  = FunDec of fundecldata list
  | ValDec of 
      { name: id
      ; typean: typean
      ; init: exp
      ; pos: pos
      }
and fundecldata = Fdecl of
      { name: id
      ; params: fielddata list
      ; result: typean
      ; body: exp
      ; pos: pos
      }
and fielddata =
    Field of { name: id; typean: typean; pos: pos }
