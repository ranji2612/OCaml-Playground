(* Insertion Sort with OCaml*)

let rec ins_sort l = match l with
        | [] -> []
        | h::t -> (insert_in_list h (ins_sort t))
and insert_in_list e l = match l with
        | [] -> [e]
        | h::t -> if h>=e then e::h::t
                  else h:: insert_in_list e t;;

(* Test cases *)

print_string "Inserting List [9;1;8;2;8;3;7;4;5] ";;
ins_sort [9;1;8;2;8;3;7;4;5];;

print_string "Inserting List [9;1;8;2;8;3;7;4;5] ";;
ins_sort [-56;98;23;-40;15;0;-88;12];;