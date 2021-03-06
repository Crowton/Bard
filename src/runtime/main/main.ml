(** Bard main *)

open Bardcommon
open Bardlexer
open Bardparser

open Bardinterpreter
open Bardtypechecker

open Phases
open ExitCodes


type config = { file: string; phase: phase;
                mailbox: Mailbox_with_labels.mailbox;
                typed_mailbox: Typed_mailbox_with_labels.mailbox;
                out: Format.formatter }

exception ExitMain of phase



(** Open the file and initialize the lexer buffer. Observe that the input 
    buffer must be closed by the caller. *)

let initLexer filename = 
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  (* obs that we need to initialize the pos_fname field ourselves *)
  filebuf.lex_curr_p <- { filebuf.lex_curr_p with pos_fname = filename };  
  (input, filebuf)

let lexonly { file; out; _ } =
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

let parse { file; phase; out; _ } = 
  let input, filebuf = initLexer file in 
  let parseRes = 
    try Parser.program Lexer.token filebuf
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
  if phase = PAR
  then Full_ast_unparser.print_exp out parseRes;
  parseRes

let evaluate { phase; out; _ } exp =
  try
    let res = Interpreter.eval_top exp out in
    if phase = EVAL
    then Format.fprintf out "Result: %s\n" (Interpreter.value_to_string res);
    res
  with
  | Interpreter.InterpreterError (msg, p) -> Printf.eprintf "Exception at %d:%d: %s\n%!" p.pos_lnum (p.pos_cnum - p.pos_bol + 1) msg; raise (ExitMain EVAL)

let evaluate_label { phase; mailbox; out; _ } exp =
  try
    let resVal, resLabel, resTypeLabel, bl = Interpreter_with_labels.eval_top exp mailbox out in
    if phase = EVAL_LABEL
    then (Format.fprintf out "Result: %s\n" (Interpreter_with_labels.full_value_to_string resVal resLabel resTypeLabel);
          Format.fprintf out "Blocking Label: %s\n" (Unparser_common.unparse_label bl));
    (resVal, resLabel, resTypeLabel)
  with
  | Interpreter_with_labels.InterpreterError (msg, p) -> Printf.eprintf "Exception at %d:%d: %s\n%!" p.pos_lnum (p.pos_cnum - p.pos_bol + 1) msg; raise (ExitMain EVAL_LABEL)


let typecheck { phase; out; _ } exp =
  try
    let resType, resExp = Typechecker.typecheck_top exp in
    if phase = TYPE
    then Full_typed_ast_unparser.print_exp out resType resExp;
    (resType, resExp)
  with
  | Typechecker.TypeError (msg, p) -> Printf.eprintf "Exception at %d:%d: %s\n%!" p.pos_lnum (p.pos_cnum - p.pos_bol + 1) msg; raise (ExitMain TYPE)


let evaluate_typed { phase; typed_mailbox; out; _ } texp =
  try
    let resVal, resLabel, resTypeLabel, bl = Typed_interpreter_with_labels.eval_top texp typed_mailbox out in
    if phase = EVAL_TYPE
    then (Format.fprintf out "Result: %s\n" (Typed_interpreter_with_labels.full_value_to_string resVal resLabel resTypeLabel);
          Format.fprintf out "Blocking Label: %s\n" (Unparser_common.unparse_label bl));
    (resVal, resLabel, resTypeLabel)
  with
  | Typed_interpreter_with_labels.InterpreterError (msg, p) -> Printf.eprintf "Exception at %d:%d: %s\n%!" p.pos_lnum (p.pos_cnum - p.pos_bol + 1) msg; raise (ExitMain EVAL_TYPE)


(* --- command-line checking; dispatching to the right phase --- *)  

(*exception InvalidInput of string*)
let withFlags ({ phase; out; _ } as config) =
  let exitCode = ref 0 in
  begin 
    try
      match phase with
      | LEX ->
          lexonly config
      | PAR ->
          let _: Ast.exp = parse config in
          ()
      | EVAL ->
          let exp = parse config in
          let _: Interpreter.value = evaluate config exp in
          ()
      | EVAL_LABEL ->
          let exp = parse config in
          let _: Interpreter_with_labels.value * Label.label * Label.label = evaluate_label config exp in
          ()
      | TYPE ->
          let exp = parse config in
          let _: Ast_common.typ * Typed_ast.texp = typecheck config exp in
          ()
      | EVAL_TYPE ->
          let exp = parse config in
          let (_, texp): Ast_common.typ * Typed_ast.texp = typecheck config exp in
          let _: Typed_interpreter_with_labels.value * Label.label * Label.label = evaluate_typed config texp in
          ()
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

  let get_mailbox phase file out = match phase with
    | EVAL_LABEL ->
        let input = open_in file in
        let value_list: ((Interpreter_with_labels.value * Label.label * Label.label) list ref) = ref [] in
        (try
          while true do
              let line = input_line input in
              let filebuf = Lexing.from_string line in
              let exp = Parser.program Lexer.token filebuf in
              let value, label, typeLabel, _ = Interpreter_with_labels.eval_top exp Mailbox_with_labels.get_empty out in
              value_list := (value, label, typeLabel) :: !value_list
          done
        with End_of_file ->
          close_in input);
        Mailbox_with_labels.get_init (List.rev !value_list)
    | _ -> Mailbox_with_labels.get_empty
  
  let get_typed_mailbox phase file out = match phase with
    | EVAL_TYPE ->
        let input = open_in file in
        let value_list: ((Typed_interpreter_with_labels.value * Label.label * Label.label) list ref) = ref [] in
        (try
          while true do
              let line = input_line input in
              let filebuf = Lexing.from_string line in
              let exp = Parser.program Lexer.token filebuf in
              let _, texp = Typechecker.typecheck_top exp in
              let value, label, typeLabel, _ = Typed_interpreter_with_labels.eval_top texp Typed_mailbox_with_labels.get_empty out in
              value_list := (value, label, typeLabel) :: !value_list
          done
        with End_of_file ->
          close_in input);
        Typed_mailbox_with_labels.get_init (List.rev !value_list)
    | _ -> Typed_mailbox_with_labels.get_empty


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
      ~summary: "Bard runtime"
      ~readme: ( fun () -> "More detailed information")
      Command.Let_syntax.(        
        let%map_open file = anon ( "filename" %: regular_file)
        and inchannel = flag ~aliases:["i"] "in" 
                    (optional string) ~doc:"FILE name of input FILE"
        and out = flag ~aliases:["o"] "out" 
                    (optional string) ~doc:"FILE name of output FILE"
        and phase = flag ~aliases:["p"] "phase" 
                    (optional_with_default "eval_type" string)
          ~doc:"PHASE stop compilation after PHASE:\nlex, par, eval"
          |> map ~f:(fun p -> match fromHandleOpt p with 
                                Some p -> p 
                              | None -> eprintf "invalid phase '%s'\n%!" p
                                      ; exit 1
                    )
        in
        fun () ->
          let out = match out with 
          | None -> Format.std_formatter 
          | Some s -> Format.formatter_of_out_channel (Out_channel.create s) in

          let mailbox, typed_mailbox = match inchannel with
          | None -> Mailbox_with_labels.get_empty, Typed_mailbox_with_labels.get_empty
          | Some s -> get_mailbox phase s out, get_typed_mailbox phase s out
          in

          let config = { file; phase; mailbox; typed_mailbox; out } in 
          withFlags config
      ) in
    Command.run ~version: "0.05" ~build_info: "Bard runtime" command 
