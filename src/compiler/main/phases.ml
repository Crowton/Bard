
type phase = LEX | PAR | SEM | LLVM | X86

let toHandle = function
| LEX -> "lex"
| PAR -> "par"
| SEM -> "sem"
| LLVM -> "llvm"
| X86 -> "x86"

let fromHandleOpt = function
| "lex" -> Some LEX
| "par" -> Some PAR
| "sem" -> Some SEM
| "llvm" -> Some LLVM
| "x86" -> Some X86
| _ -> None

