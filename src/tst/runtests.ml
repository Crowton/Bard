(**  Golden testing for Bard *)

(* This file implements a golden test harness for Bard runtime. *)

(*

# Implementation notes 

The file is split into two modules:

## Module Bard Golden 

This module implements generic testing of a bard program wrt a particular
compiler phase.

### Test naming convention. 

Every bard test case is associated with a set of expected output files -- one
for each phase. The naming convention is that we associate `file.bard` with the
following expected-files.

- `file.bard.expected-lex`
- `file.bard.expected-par`
- `file.bard.expected-res`

If the corresponding expected-file does not exist it is created upon first
invocation of the test. This is the standard "golden testing". Note that when
comparing the results of the compiler, we also distinguish between positive and
negative tests and additionally check for the exit codes.

### Overwriting the expected files. 

Use flag `--overwrite-expected`. Beware the tool provides no textual feedback
when overwriting the files. Overwritten tests are considered OK passing.

## Module DefaultTests 

This module implements default grouping of the tests. Observe that default
grouping does not include the fine-grained partitioning of the feature sets. 


## Feature set support (in development)

To support feature sets, we need another module that implements fine-grained
partitioning of test cases, where feature set configuration either read off
from a file or following some easy-to-use convention. 

The main entry point of our tool supports a flag "--use-feature-sets" 
that opts for using feature-set test groups instead of the default ones.



## Infrastructure/Alcotest

Our implementation uses Alcotest unit test library that does all the heavy
lifting wrt logging/error reporting. Using a unit test library for the
"end-to-end" testing feels a bit of a hack, but so far it has been rather
unproblematic. 

Alcotest API: https://docs.mirage.io/alcotest/Alcotest/index.html.


## Disclaimer on performance: the IO behavior of our implementation is
relatively naive and very inefficient; in particular all outputs are represented
as immutable strings; we also scan all the test subdirecotries at all times --
we should probably do that in a on-demand manner.

*)



let defaultPhasesToCheck = 
  let open BardGolden in 
    [LEX; PAR; EVAL; EVAL_LABEL]



(**************************************************************************)

(* The rest of this file implements rather boring and very hacky command-line 
   argument processing *)

let flagUSEFEATURESETS = "--use-feature-sets" 
let flagONLY = "--only" 
let flagOVERWRITEEXPECTED = "--overwrite-expected"

let flags = [ flagUSEFEATURESETS; flagONLY; flagOVERWRITEEXPECTED]

    
let parseFlagOnly argv = 
  let args_list = Array.to_list argv in 
  let exit_with_error () = 
      Printf.fprintf stderr "--only flag requires an regexp argument\n"; 
      exit 1 in
  let rec i xs ys = 
     match xs with 
       [] -> (None, List.rev ys)
     | x::x'::xs when x = flagONLY -> 
         if List.mem x' flags then exit_with_error ()
         else (Some (Str.regexp x'), (List.rev ys) @ xs)
     | [x] when x = flagONLY -> exit_with_error ()        
     | x :: xs -> i xs (x :: ys)     
  in
  let u, ls = i args_list [] in 
  u, Array.of_list ls

let removeFlag argv flag = 
  argv |> Array.to_list  |> List.filter ( (<>) flag  ) |> Array.of_list

let setOverwriteFlag argv (options:BardGolden.options) = 
  if Array.mem flagOVERWRITEEXPECTED argv then   
  removeFlag argv flagOVERWRITEEXPECTED, {options with overwrite = true }
  else argv, options  

let () =
  let featureSetTests = [] in  (* Todo: initialize this. *)  
  let options_init:BardGolden.options = 
        {use_filter = None; overwrite = false} in 
  let argv1,options = setOverwriteFlag Sys.argv options_init in 
  let argv2, tests = 
      if Array.mem flagUSEFEATURESETS argv1
      then removeFlag argv1 flagUSEFEATURESETS, featureSetTests   
      else 
        let defaults = (Core.Fn.flip DefaultTesting.defaultTestsForPhases) 
                defaultPhasesToCheck in
        if Array.mem flagONLY argv1
        then               
          let u, argv' = parseFlagOnly argv1 
          in argv', defaults {options with use_filter = u} 
        else argv1, defaults {options with use_filter = None}
  in
  (try
    Alcotest.run ~and_exit:false ~argv: argv2 "Bard runtime" tests
  with
  | _  -> ());
  let diff_file= "_build/_tests/diff-recent.html" in
  DiffWriter.writeResults diff_file;
  Printf.printf "The diff of the most recent test run is available in %s\n" diff_file
