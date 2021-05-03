(**************************************************************************)
(* AU Compilation. Assignment submissions must not modify this file       *)
(**************************************************************************)

(** Tigerc compiler main *)


open Tigercommon 
open Tigerlexer
open Tigerparser 
(* open Tigersemant *)
open Phases
open ExitCodes

module A = Absyn
module A' = Tabsyn
module S = Symbol

type config = {file: string; phase: phase; normalize: bool; unfold: int; out: Format.formatter}

exception ExitMain of phase

(** Open the file and initialize the lexer buffer. Observe that the input 
    buffer must be closed by the caller. *)

let initLexer filename = 
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  (* obs that we need to initialize the pos_fname field ourselves *)
  filebuf.lex_curr_p <- { filebuf.lex_curr_p with pos_fname = filename };  
  (input, filebuf)

let lexonly {file;out;_} =
  let input, filebuf = initLexer file in
  let lexRes =   
    try
      let tokens = Parser.lexdriver Lexer.token filebuf in
      let printToken ((t,p ):string * Lexing.position) = 
        Format.fprintf out "%d:%d:%s\n" 
          p.pos_lnum (p.pos_cnum - p.pos_bol + 1) t
      in  
      List.iter printToken tokens
    with
    | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg ;
      raise (ExitMain LEX)
  in 
  close_in input; lexRes

let parse {file;phase;out;normalize;_} = 
  let input, filebuf = initLexer file in 
  let parseRes = 
    try  Parser.program Lexer.token filebuf
    with
    | Lexer.Error msg -> Printf.eprintf "%s%!" msg; raise (ExitMain LEX)    
    | Parser.Error ->  
      let pos1 = Lexing.lexeme_start_p filebuf in
      let pos2 = Lexing.lexeme_end_p filebuf in
      let lexeme = Lexing.lexeme filebuf in
      Printf.fprintf stderr "%s:%d:%d - %d:%d: syntax error '%s'\n"
        pos1.pos_fname pos1.pos_lnum (pos1.pos_cnum - pos1.pos_bol)
        pos2.pos_lnum (pos2.pos_cnum - pos2.pos_bol + 1)
        lexeme;
      raise (ExitMain PAR)        
  in 
  close_in input;
  (* if phase = PAR
  then Prabsyn.print_exp out parseRes; *)
  parseRes

(* let semant {phase;out;unfold;_} exp = 
  let texp, err = Semant.transProg exp
  in if Errenv.any_errors err
  then raise (ExitMain SEM);
  if phase = SEM
  then Prtabsyn.print_exp unfold out texp;
  texp *)


(* Our reference compiler should allow for stopping at different phases 
   in the compilation and allow us to pick the right backend. We implement 
   this using a straightforward command-line parsing/checking *)


(* --- command-line checking; dispatching to the right phase --- *)  

(*exception InvalidInput of string*)

(* observe that we make sure that exit flags are raised upon
   invalid return from each phase *)
let withFlags ({phase;out;_} as config) =
  let exitCode = ref 0 in

  (*let stop s = raise (InvalidInput ("Invalid input: " ^ s)) in*)

  begin 
    try
      match phase with
      | LEX ->
          lexonly config
      | PAR ->
          let _ = parse config in
          ()
      (* | SEM ->
          let exp = parse config in
          let _ = semant config exp in
          () *)
      |_  ->  failwith "only lexing, parsing, and semantic analysis phases are supported"
    with ExitMain p ->
           exitCode := (error_code p)
  end; 
  Format.pp_print_flush out ();
  flush_all();
  exit (!exitCode) (* obs: exits the program *)

  (* --- program entry point: prep work wrt command line args --- *) 


  (* We use Jane street's Command module for command-line parsing

     http://dev.realworldocaml.org/command-line-parsing.html

     Observe that this part relies on ppx_jane extension.
  *)

  let main () = 
    let open Core in

    let regular_file =
      Command.Arg_type.create
        (fun filename ->
           match Sys.is_file filename with
           | `Yes -> filename
           | `No | `Unknown ->
             eprintf "'%s' is not a regular file.\n%!" filename;
             exit 1) in

    let command = Command.basic 
      ~summary: "Tiger AU reference compiler"
      ~readme: ( fun () -> "More detailed information")
      Command.Let_syntax.(        
        let%map_open file = anon ( "filename" %: regular_file)
        and out = flag ~aliases:["o"] "out" 
                    (optional string) ~doc:"FILE name of output FILE"
        and phase = flag ~aliases:["p"] "phase" 
                    (optional_with_default "sem" string)
          ~doc:"PHASE stop compilation after PHASE:\nlex, par, sem, llvm, x86"
          |> map ~f:(fun p -> match fromHandleOpt p with 
                                Some p -> p 
                              | None -> eprintf "invalid phase '%s'\n%!" p
                                      ; exit 1
                    )
        and normalize = flag ~aliases:["n"] "normalize" no_arg 
                            ~doc:" normalize pretty printing of AST"
        and unfold = flag ~aliases:["u"] "unfold" 
          (optional_with_default 0 int) 
          ~doc:"n unfold name-types n levels when printing AST"
          |> map ~f:
              (fun u -> 
                if u < 0 
                then (eprintf "argument to unfold must be non-negative\n%!"
                      ; exit 1) 
                else u)
        in
        fun () -> 
          let out = match out with 
          | None -> Format.std_formatter 
          | Some s -> Format.formatter_of_out_channel (Out_channel.create s) in
          let config = {file; phase; normalize; unfold; out} in 
          withFlags config
      ) in
    Command.run ~version: "0.05" ~build_info: "AU Tiger compiler" command 
