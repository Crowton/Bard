%{
  open Bardcommon.Ast
%}

%token EOF
%token <string> ID
%token <int> INT
%token TRUE FALSE
%token <string> STRING
%token COMMA COLON
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDE
%token LT LE GT GE EEQ NEQ
%token AND OR NOT
%token CARET
%token IF THEN ELSE
%token LET IN END VAL FUN EQ
%token INT_TYPE, BOOL_TYPE, STRING_TYPE, UNIT_TYPE, ARROW

(* Lists in lists for functions decls and general decls error.
   Precedence to the inner list - i.e. all functions in the same list *)
%nonassoc FUN
%nonassoc fun_list_base

(* Handles if followed by binary exp and dangleling else *)
%nonassoc THEN
%nonassoc ELSE

(* Precedence and associativity of arithmetics *)
%left OR
%left AND
%nonassoc EEQ NEQ
%nonassoc LT LE GT GE
%right CARET
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc unary_minus NOT

(* Function calls *)
%nonassoc LPAREN

%start <Bardcommon.Ast.exp> program  
(* Observe that we need to use fully qualified types for the start symbol *)

%%
(* Expressions *)
exp:
  | LPAREN e=exp RPAREN                          { e }
  | i=INT                                        { IntLit i }
  | TRUE                                         { BoolLit true }
  | FALSE                                        { BoolLit false }
  | s=STRING                                     { StringLit (s, $startpos) }
  | l=ID                                         { VarExp (l, $startpos) }
  | MINUS e=exp                                  { UnOpExp { oper=NegUnOp; exp=e; pos=$startpos } } %prec unary_minus
  | NOT e=exp                                    { UnOpExp { oper=NotUnOp; exp=e; pos=$startpos } }
  | e1=exp o=op e2=exp                           { BinOpExp { left=e1; oper=o; right=e2; pos=$startpos } }
  | IF t=exp THEN e1=exp                         { IfExp { test=t; thn=e1; els=None; pos=$startpos } }
  | IF t=exp THEN e1=exp ELSE e2=exp             { IfExp { test=t; thn=e1; els=Some e2; pos=$startpos } }
  | LET d=list(decl) IN e=exp END                { LetExp { decls=d; body=e; pos=$startpos } }
  | f=exp LPAREN RPAREN                          { CallExp { func=f; args=[]; pos=$startpos } }
  | f=exp LPAREN a=argslist RPAREN               { CallExp { func=f; args=a; pos=$startpos } }
%inline op:
  | PLUS    { PlusBinOp }
  | MINUS   { MinusBinOp }
  | TIMES   { TimesBinOp }
  | DIVIDE  { DivideBinOp }
  | LT      { LtBinOp }
  | LE      { LeBinOp }
  | GT      { GtBinOp }
  | GE      { GeBinOp }
  | EEQ     { EqBinOp }
  | NEQ     { NeqBinOp }
  | AND     { AndBinOp }
  | OR      { OrBinOp }
  | CARET   { ConcatBinOp }

(* Rules for decls. Made to parse list of decls, in arbitary order *)
decl:
  | fd=fundecllist                       { FunDec fd }
  | VAL id=ID EQ e=exp                   { ValDec { name=id; typean=None; init=e; pos=$startpos } }
  | VAL id=ID COLON typ=typ EQ e=exp     { ValDec { name=id; typean=Some(typ, $startpos(typ)); init=e; pos=$startpos } }

(* Def for nonempty list. Type fundecldata. *)
fundecllist:
  | n = fundecldata                  { [n] }       %prec fun_list_base
  | h = fundecldata t = fundecllist  { h :: t }

(* The function delcs. Rules for explicit type and no type *)
fundecldata:
  | FUN id=ID LPAREN tyf=separated_list(COMMA, tyfield) RPAREN EQ e=exp                { Fdecl { name=id; params=tyf; result=None; body=e; pos=$startpos } }
  | FUN id=ID LPAREN tyf=separated_list(COMMA, tyfield) RPAREN COLON typ=typ EQ e=exp  { Fdecl { name=id; params=tyf; result=Some(typ, $startpos(typ)); body=e; pos=$startpos } }

(* A type field *)
tyfield:
  | id=ID                 { Field { name=id; typean=None; pos=$startpos }  }
  | id=ID COLON ty=typ    { Field { name=id; typean=Some(ty, $startpos(ty)); pos=$startpos }  }

typ:
  | INT_TYPE                                                    { Int }
  | BOOL_TYPE                                                   { Bool }
  | STRING_TYPE                                                 { String }
  | UNIT_TYPE                                                   { Unit }
  | LPAREN ty=separated_list(COMMA, typ) RPAREN ARROW res=typ   { FunType (ty, res) }


(* Def for list. Type exp. Used for function calls *)
argslist:
  | e=exp                   { [(e, $startpos)] }
  | e=exp COMMA t=argslist  { (e, $startpos) :: t}

(* Top-level *)
program: e = exp EOF  { e }
