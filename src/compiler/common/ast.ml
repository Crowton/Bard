type pos = Lexing.position

type binOp
  = PlusBinOp | MinusBinOp | TimesBinOp | DivideBinOp
  | LtBinOp | LeBinOp | EqBinOp | NeqBinOp | GtBinOp | GeBinOp
  | AndBinOp | OrBinOp
  | ConcatBinOp

type unOp
  = NegUnOp | NotUnOp 

type exp
  = IntLit of int
  | BoolLit of bool
  | StringLit of string * pos
  | VarExp of string * pos
  | BinOpExp of { left: exp; oper: binOp; right: exp; pos: pos }
  | UnOpExp of { oper: unOp; exp: exp; pos: pos }
  | IfExp of { test: exp; thn: exp; els: exp option; pos: pos }  
  | CallExp of { func: exp; args: (exp * pos) list; pos: pos }   
  | LetExp of { decls: decl list; body: exp; pos: pos }
and decl 
  = FunDec of fundecldata list 
  | ValDec of 
      { name: string
      ; typ: (string * pos) option
      ; init: exp
      ; pos: pos 
      }
and fundecldata = Fdecl of   
      { name: string
      ; params: fielddata list
      ; result: (string * pos) option
      ; body: exp
      ; pos: pos 
      }
and fielddata = 
    Field of { name: string; typ: (string * pos) option; pos: pos }
