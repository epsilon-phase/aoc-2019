open Intcode;;
open Utilities;;

let ()=
  let l=load_file "day-5.txt" in
  let state=String.split_on_char ',' l in
  let state=List.map (fun x->
      try int_of_string (String.trim x)
      with Failure s->output_string stdout x;raise (Failure s)
    ) state in
  let state=Array.append (Array.of_list state) (Array.make 10000 0) in
  ignore(operate read_int (fun x->ignore(Printf.printf "%i\n" x)) state 0)
