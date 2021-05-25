type phase = LEX | PAR | EVAL | EVAL_LABEL | TYPE | EVAL_TYPE

let toHandle = function
| LEX -> "lex"
| PAR -> "par"
| EVAL -> "eval"
| EVAL_LABEL -> "eval_label"
| TYPE -> "type"
| EVAL_TYPE -> "eval_type"

let fromHandleOpt = function
| "lex" -> Some LEX
| "par" -> Some PAR
| "eval" -> Some EVAL
| "eval_label" -> Some EVAL_LABEL
| "type" -> Some TYPE
| "eval_type" -> Some EVAL_TYPE
| _ -> None
