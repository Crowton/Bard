(* See comments in runtests.ml *)

type testphase 
  = LEX | PAR | EVAL |  EVAL_LABEL | TYPE | EVAL_TYPE

let phaseName = function 
  | LEX -> "Lexer" 
  | PAR -> "Parser"
  | EVAL -> "Interpreter"
  | EVAL_LABEL -> "Labeled Interpreter"
  | TYPE -> "Typechecker"
  | EVAL_TYPE -> "Typed Interpreter"

let runWithStatus cmd =
  let inp = Unix.open_process_in cmd in
  let s = Core.In_channel.input_all inp in
  Core.In_channel.close inp;
  (Unix.close_process_in inp, s)

let findByExtension ?(sort=true) dir exts =
  let isMatch f =     
    List.mem (Filename.extension f) exts in

  let rec loop result = function
    | f::fs when Sys.is_directory f ->
          Sys.readdir f
          |> Array.to_list
          |> List.map (Filename.concat f)
          |> List.append fs
          |> loop result
    | f::fs when isMatch f -> loop (f::result) fs
    | _::fs -> loop result fs 
    | []    -> result
  in
    (loop [] [dir]) |> if sort then List.sort compare else Core.Fn.id 

(* we only call this for positive test cases *)
let golden phase code out file overwrite =
  let golden_ext = match phase with 
    | LEX -> ".expected-lex"
    | PAR -> ".expected-par" 
    | EVAL -> ".expected-res"
    | EVAL_LABEL -> ".expected-lab"
    | TYPE -> ".expected-typ"
    | EVAL_TYPE -> ".expected-typ-res"
  in
  let golden_file = file ^ golden_ext in
  match code, Sys.file_exists golden_file, overwrite with
  | 0, false, _ | 0, true, true ->
    (* succeeded, either no expected file or overwriting: write expected file *)
    Core.Out_channel.write_all golden_file ~data:out
  | 0, true, false | _, true, _ ->
    (* expected file found: check against it
       if compilation failed, it is still useful to see what we should have gotten *)
    let golden = Core.In_channel.read_all golden_file in
    if not (String.equal golden out)
    then DiffWriter.logDiff (Filename.basename file) (phaseName phase) golden_file out;
    Alcotest.(check string) ("matching output for " ^ file) golden out
  | _ ->
    (* error and no expected file: we can only fail the test *)
    Alcotest.fail out
 
let phaseFlag phase = "-p " ^ 
  match phase with 
   LEX -> "lex" | PAR -> "par" | EVAL -> "eval" | EVAL_LABEL -> "eval_label" | TYPE -> "type" | EVAL_TYPE -> "eval_type"

let inputFlag file =
  let filenamelen = String.length file in
  let namebase = String.sub file 0 (filenamelen - 5) in
  let infilename = namebase ^ ".in" in
  if Sys.file_exists infilename then "-i " ^ infilename else ""

(* observe that if the exit code is non-zero we make no further checks *)  
let goldenWithExitCode expectedExitCode phase overwrite file () = 
  let flag = " " ^ phaseFlag phase ^ " " ^ inputFlag file in
  let status, out = runWithStatus @@"./testwrap.sh " ^ file ^ flag in 
  match status with 
  | Unix.WEXITED 0 when expectedExitCode = 0  ->
      golden phase 0 out file overwrite   (* only check the output on 0-exit *)
  | Unix.WEXITED code when expectedExitCode = 0 -> 
      golden phase code
      ("<<runtests: test target finished with exit code " ^ (string_of_int code) ^ ". Output suppressed>>")
      file false
  | Unix.WEXITED 1 ->
      Alcotest.fail ("Uncaught exception during compilation")
  | Unix.WEXITED code ->
      Alcotest.(check int) ("matching exit codes") expectedExitCode code;
  | _ -> Alcotest.fail ("Exit via an interrupt or a signal")
 
let fromFileName tester name  = 
  (* let base = name |> Filename.basename |> Filename.remove_extension in  *)
  let open Alcotest in 
  test_case name `Quick (tester name) 

type options = { use_filter: Str.regexp option ; overwrite: bool }  


let testCasesFromFiles options tester files = 
  let filtered = 
    match options.use_filter with 
      None -> files 
    | Some r -> List.filter ( 
        fun s -> 
          try Str.search_forward r s 0 |> fun _ -> true  
          with Not_found -> false            
        ) files in 
  List.map (fromFileName  tester) filtered 
  