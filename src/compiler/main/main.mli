(** Bard runtime main interface. *)

val main: unit -> unit 

open Bardcommon
type config

val lexonly : config -> unit 
val parse : config -> Ast.exp
