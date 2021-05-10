open Bardcommon.Ast
open Bardlexer
open Bardparser
open Bardinterpreter


let get_baseline_prog =
  (* Lexing phase *)
  let filename = "./src/runtime/benchmarks/turing_tuple_baseline.bard" in
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  filebuf.lex_curr_p <- { filebuf.lex_curr_p with pos_fname = filename };  
  
  (* Parse *)
  let parseRes = Parser.program Lexer.token filebuf in 
  close_in input;
  
  parseRes


let symbol_to_int (symb: string): int =
  if symb =  "d" then  0 else
  if symb =  "0" then  1 else
  if symb =  "1" then  2 else
  if symb =  "2" then  3 else
  if symb =  "3" then  4 else
  if symb =  "4" then  5 else
  if symb =  "5" then  6 else
  if symb =  "6" then  7 else
  if symb =  "7" then  8 else
  if symb =  "8" then  9 else
  if symb =  "9" then  10 else
  if symb =  "1'" then  11 else
  if symb =  "2'" then  12 else
  if symb =  "3'" then  13 else
  if symb =  "4'" then  14 else
  if symb =  "5'" then  15 else
  if symb =  "6'" then  16 else
  if symb =  "7'" then  17 else
  if symb =  "8'" then  18 else
  19


let extend_trans stateFrom symbolFrom stateTo symbolWrite headMove =
  ValDec {
    name="trans_list";
    typean=None;
    init=CallExp {
            func=VarExp ("cons", Lexing.dummy_pos);
            args=[
              (
                CallExp {
                    func=VarExp ("tuple5", Lexing.dummy_pos);
                    args=[
                      (IntLit stateFrom, Lexing.dummy_pos);
                      (IntLit symbolFrom, Lexing.dummy_pos);
                      (IntLit stateTo, Lexing.dummy_pos);
                      (IntLit symbolWrite, Lexing.dummy_pos);
                      (IntLit headMove, Lexing.dummy_pos)
                    ];
                    pos=Lexing.dummy_pos
                },
                Lexing.dummy_pos
              );
              (VarExp ("trans_list", Lexing.dummy_pos), Lexing.dummy_pos)
            ];
            pos=Lexing.dummy_pos
         };
    pos=Lexing.dummy_pos
  }


let get_prog_with_transition =
  let baseProg = get_baseline_prog in

  let transFile = open_in "./src/runtime/benchmarks/sudoku_transitions.txt" in

  let try_read () =
    try Some (input_line transFile) with End_of_file -> None in
  
  let rec loop acc = match try_read () with
    | Some s ->
      let seperatedValues = String.split_on_char ',' s in
      let stateFrom   = List.nth seperatedValues 0 |> int_of_string in
      let symbolFrom  = List.nth seperatedValues 1 |> symbol_to_int in
      let stateTo     = List.nth seperatedValues 2 |> int_of_string in
      let symbolWrite = List.nth seperatedValues 3 |> symbol_to_int in
      let headMove    = List.nth seperatedValues 4 |> int_of_string in
      let newValDecl = extend_trans stateFrom symbolFrom stateTo symbolWrite headMove in
      loop (newValDecl :: acc)
    | None -> List.rev acc
  in
  
  let valOfTrans = loop [ValDec { name="trans_list"; typean=None; init=VarExp ("nil", Lexing.dummy_pos); pos=Lexing.dummy_pos }] in

  close_in transFile;

  match baseProg with
  | LetExp { decls=outDecl; body=LetExp { body=innerBody; pos=innerPos; _ }; pos=outPos } ->
      LetExp { decls=outDecl; body=LetExp { decls=valOfTrans; body=innerBody; pos=innerPos }; pos=outPos }


let get_prog_with_sudoku sudoku =
  let transProg = get_prog_with_transition in

  let rec loop sudokuRest acc = match sudokuRest with
    | [] -> List.rev acc
    | x :: xs ->
      let newValDecl = ValDec {
        name="sudoku";
        typean=None;
        init=CallExp {
                func=VarExp ("cons", Lexing.dummy_pos);
                args=[
                  (IntLit x, Lexing.dummy_pos);
                  (VarExp ("sudoku", Lexing.dummy_pos), Lexing.dummy_pos)
                ];
                pos=Lexing.dummy_pos
            };
        pos=Lexing.dummy_pos
      } in
      loop xs (newValDecl :: acc)
  in
  
  let valOfSudoku = loop sudoku [ValDec { name="sudoku"; typean=None; init=VarExp ("nil", Lexing.dummy_pos); pos=Lexing.dummy_pos }] in

  match transProg with
  | LetExp { decls=outDecl; body=LetExp { decls=transDecls; body=innerBody; pos=innerPos }; pos=outPos } ->
      LetExp { decls=outDecl; body=LetExp { decls=List.append transDecls valOfSudoku; body=innerBody; pos=innerPos }; pos=outPos }


let run () =
  let sudoku = [
      5; 3; 0; 0; 7; 0; 0; 0; 0;
      6; 0; 0; 1; 9; 5; 0; 0; 0;
      0; 9; 8; 0; 0; 0; 0; 6; 0;
      8; 0; 0; 0; 6; 0; 0; 0; 3;
      4; 0; 0; 8; 0; 3; 0; 0; 1;
      7; 0; 0; 0; 2; 0; 0; 0; 6;
      0; 6; 0; 0; 0; 0; 2; 8; 0;
      0; 0; 0; 4; 1; 9; 0; 0; 5;
      0; 0; 0; 0; 8; 0; 0; 7; 9
  ] in

  let prog = get_prog_with_sudoku sudoku in

  print_endline "Rasmus er gay\n";

  let res, _ = Interpreter.eval_top prog in
  Format.eprintf "Result: %s\n" (Interpreter.value_to_string res)
