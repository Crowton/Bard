(* See comments in runtests.ml *)

open BardGolden

let findBards = (Core.Fn.flip findByExtension) [".bard"]
let neg_lex = findBards "testcases/neg/lex" 
let neg_par = findBards "testcases/neg/par" 
let neg_eval = findBards "testcases/neg/eval"
let neg_labeled = findBards "testcases/neg/labeled"
let neg_type = findBards "testcases/neg/type"
let neg_eval_typed = findBards "testcases/neg/eval_type"
let pos_batch = findBards "testcases/pos/batch"
let pos_labeled = findBards "testcases/pos/labeled"
let pos_type = findBards "testcases/pos/type"
let pos_eval_typed = findBards "testcases/pos/eval_type"


let posTests = function 
| LEX -> neg_par @ neg_eval @ pos_batch 
| PAR -> neg_eval @ pos_batch
| EVAL -> pos_batch
| EVAL_LABEL -> pos_labeled
| TYPE -> neg_eval_typed @ pos_type
| EVAL_TYPE -> pos_eval_typed

let negTests = function 
| LEX  -> neg_lex
| PAR  -> neg_par
| EVAL -> neg_eval
| EVAL_LABEL -> neg_labeled
| TYPE -> neg_type
| EVAL_TYPE -> neg_eval_typed

let error_code = function
| LEX   -> 10
| PAR   -> 20
| EVAL  -> 30
| EVAL_LABEL -> 40
| TYPE  -> 50
| EVAL_TYPE -> 60

let defaultTestPos = goldenWithExitCode 0
let defaultTestNeg phase = goldenWithExitCode @@ error_code @@ phase

let defaultPositives options phase = 
    testCasesFromFiles options (defaultTestPos phase options.overwrite) (posTests phase)    

let defaultNegatives options phase = 
  testCasesFromFiles  options ((defaultTestNeg phase) phase options.overwrite) (negTests phase)

(** create a list of deafult tests for a phase *)
let defaultTests options phase = 
  let name = phaseName phase in 
  [name ^  "+", defaultPositives options phase;
   name ^  "-", defaultNegatives options phase]


let defaultTestsForPhases options phases 
  = phases 
  |> List.map (defaultTests options)
  |> List.concat 
