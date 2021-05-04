(**************************************************************************)
(* AU Compilation.                                                        *)
(**************************************************************************)

(* See comments in runtests.ml *)

open TigerGolden

let findBards = (Core.Fn.flip findByExtension) [".bard"]
let neg_lex = findBards "testcases/neg/lex" 
let neg_par = findBards "testcases/neg/par" 
(* let neg_sem = findTigs "testcases/neg/sem" *)
let pos_batch = findBards "testcases/pos/batch"
let pos_io = findBards "testcases/pos/interactive"


let posTests = function 
  LEX -> neg_par (*@ neg_sem*) @ pos_batch @ pos_io 
| PAR -> (*neg_sem @*) pos_batch @ pos_io 
| SEM -> pos_batch @ pos_io 
| LL Batch -> pos_batch 
| X86 Batch -> pos_batch 
| _ -> pos_io

let negTests = function 
  LEX -> neg_lex 
| PAR -> neg_par 
(* | _   -> neg_sem *)

let error_code = function
| LEX   -> 10
| PAR   -> 20
| SEM   -> 30
| LL _  -> 40
| X86 _ -> 50

let defaultTestPos = goldenWithExitCode 0
let defaultTestNeg phase = goldenWithExitCode @@ error_code @@ phase

let defaultPositives options phase = 
    testCasesFromFiles options (defaultTestPos phase options.overwrite) (posTests phase)    

let defaultNegatives options phase = 
  testCasesFromFiles  options ((defaultTestNeg phase) phase options.overwrite)  (negTests phase)

(** create a list of deafult tests for a phase *)
let defaultTests options phase = 
  let name = phaseName phase in 
  [name ^  "+", defaultPositives options phase;
   name ^  "-", defaultNegatives options phase]


let defaultTestsForPhases options phases 
  = phases 
  |> List.map (defaultTests options)
  |> List.concat 
