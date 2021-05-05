type phase = LEX | PAR | EVAL

let toHandle = function
| LEX -> "lex"
| PAR -> "par"
| EVAL -> "eval"

let fromHandleOpt = function
| "lex" -> Some LEX
| "par" -> Some PAR
| "eval" -> Some EVAL
| _ -> None
