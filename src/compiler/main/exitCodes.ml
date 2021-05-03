open Phases

let success_code _ = 0

let error_code = function
| LEX   -> 10
| PAR   -> 20
| SEM   -> 30
| LLVM  -> 40
| X86   -> 50