open Phases

let success_code _ = 0

let error_code = function
| LEX        -> 10
| PAR        -> 20
| EVAL       -> 30
| EVAL_LABEL -> 40
