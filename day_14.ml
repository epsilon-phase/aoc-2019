open Angstrom;;
open Utilities;;
type reactant=
  string*int

type reaction=
  reactant list * int * string

let ws = skip_while (function
    |' '|'\t'->true
    |_->false)

let quantity= take_while (function | '0'..'9'->true |_->false)

let identifier=take_while (function | 'A'..'Z'->true|_->false)
let reactant=
  let q=ws *> quantity <* ws in
  let i=identifier in
  lift2 (fun x y->y,int_of_string x) q i

let reaction=
  let req=sep_by (char ',') reactant in
  let prod=ws *> string "=>" *> reactant in
  lift2 (fun x (y,z)->x,y,z) req prod

let print_reactant (a,b)=
  Printf.printf "%i %s\n" b a

let ()=
  let lines=List.filter (fun x->String.length x>0) @@List.map String.trim @@ String.split_on_char '\n' @@ load_file "day-14.txt" in
  List.iter (fun l->
  match parse_string reaction l with
  | Ok (a,n,b)->List.iter print_reactant a;Printf.printf "=>%i %s\n" b n
  | Error e->print_endline e;print_endline l) lines;;
