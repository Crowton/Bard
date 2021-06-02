open Bardcommon
open Ast_common
open Unparser_common
open Label
open Interpreter_with_labels

class mailbox (init_buffer: (value * label * label) list) =
  object (self)
    val mutable buffer: (value * label * label) list = init_buffer

    method strong_typematcher (typ: typean) (elm: value): bool =
      match typ with
      | None -> true
      | Some (t, _) -> typ_match_value t elm

    method traverse typ inter_buff: ((value * label * label) option * (value * label * label) list) =
        match inter_buff with
        | [] -> (None, [])
        | (elm, lab, typLab) :: tail ->
            if self#strong_typematcher typ elm
              then (Some (elm, lab, typLab), tail)
              else let res, resTail = self#traverse typ tail in (res, (elm, lab, typLab) :: resTail)

    method get (typ: typean) pos: (value * label * label) =
      let res, finalBuff = self#traverse typ buffer in
      match res with
      | None ->
          raise (InterpreterError ("Buffer does not contain value of type " ^ unparse_typean typ, pos))
      | Some elm ->
          buffer <- finalBuff;
          elm
  end


let get_empty = new mailbox []
let get_init b = new mailbox b

