module S = Set.Make(String)

type label = S.t

let list_to_label li = S.of_list li
let label_to_list lab = S.fold (fun (l: string) (acc: string list) -> l :: acc) lab [] |> List.rev

let bot = S.empty
(* let least_upper_bound label1 label2 = S.union label1 label2 *)
let lub label1 label2 = S.union label1 label2

(* let (#|) label1 label2 = least_upper_bound label1 label2 *)
