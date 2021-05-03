(**************************************************************************)
(* AU Compilation.                                                        *)
(**************************************************************************)

(* See comments in runtests.ml *)

type runmode = Batch | Interactive (* the interactive mode is not yet supported *)

type testphase 
  = LEX | PAR | SEM | LL of runmode | X86 of runmode 

let phaseName = function 
    LEX -> "Lexer" 
  | PAR -> "Parser"
  | SEM -> "Semant"
  | LL Batch -> "LLBatch"
  | LL Interactive -> "LLInter"
  | X86 Batch -> "x86Batch "
  | X86 Interactive -> "x86Inter"

let runWithStatus cmd =
  let inp = Unix.open_process_in cmd in
  let s = Core.In_channel.input_all inp in
  Core.In_channel.close inp;
  (Unix.close_process_in inp, s)
    
(* attribution: https://gist.github.com/lindig/be55f453026c65e761f4e7012f8ab9b5 *) 
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
    LEX -> ".expected-lex"
  | PAR -> ".expected-par" 
  | SEM -> ".expected-sem"
  | _ ->   ".expected-out" in
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
   LEX -> "lex"|PAR ->"par"|SEM->"sem"|LL _ ->"llvm" |X86 _ ->"x86"
   

(* observe that if the exit code is non-zero we make no further checks *)  
let goldenWithExitCode expectedExitCode phase overwrite file () = 
  let flag = " " ^ phaseFlag phase in     
  let status,out = runWithStatus @@"./testwrap.sh " ^ file ^ flag ^ " -normalize" in 
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
          try Str.search_forward r s 0|> fun _ -> true  
          with Not_found -> false            
        ) files in 
  List.map (fromFileName  tester) filtered 
  