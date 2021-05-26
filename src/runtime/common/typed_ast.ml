open Label
open Ast_common


type texp
  = IntLit of int
  | BoolLit of bool
  | StringLit of string * pos
  | VarExp of id * pos
  | RaisedToExp of { texp: texp; label: label; pos: pos }
  | SendExp of { texp: texp; pos: pos }
  | BinOpExp of { left: texp; oper: binOp; right: texp; leftcanfail: bool; rightcanfail: bool; pos: pos }
  | UnOpExp of { oper: unOp; texp: texp; canfail: bool; pos: pos }
  | IfExp of { test: texp; thn: texp; els: texp option; pos: pos }
  | CallExp of { func: texp; args: (texp * bool * pos) list; pos: pos }
  | LambdaExp of { params: fielddata list; body: texp; pos: pos }
  | LetExp of { decls: decl list; body: texp; pos: pos }
and decl
  = FunDec of fundecldata list
  | ValDec of 
      { name: id
      ; typean: typean
      ; init: texp
      ; canfail: bool
      ; pos: pos
      }
and fundecldata = Fdecl of
      { name: id
      ; params: fielddata list
      ; result: typean
      ; body: texp
      ; rescanfail: bool
      ; pos: pos
      }
and fielddata =
    Field of { name: id; typean: typean; pos: pos }
