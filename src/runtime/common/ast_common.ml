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
