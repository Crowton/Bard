(**************************************************************************)
(* AU Compilation. Assignment submissions must not modify this file       *)
(**************************************************************************)

(** Tigerc compiler main interface. We do not expose any modules *)

val main: unit -> unit 


(* Obs: if you want to test your phases by calling them from OCaml utop 
   then you need to uncomment the corresponding functions here *)
   
open Tigercommon
type config

val lexonly : config -> unit 
val parse : config -> Ast.exp
(* val semant: config -> Absyn.exp -> Tabsyn.exp *)
