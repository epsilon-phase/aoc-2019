let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (Bytes.unsafe_to_string s)

let split_layers s n:string list=
  let rec collect l s=
    let sl=String.length s in
    if String.length s = 0 then
      l
    else
      collect (String.sub s 0 n::l)
        (String.sub s n (sl-n)) in
  collect [] s

let count_occurences s c=
  let count=ref 0 in
  String.iter (fun x->count:=!count+if x = c then 1 else 0) s;
  !count

let find_min img c=
  let rec min_min best best_count i=
    match i with
    | row::rest->
      let nr = count_occurences row c in
      if nr < best_count then
        min_min row nr rest
      else
        min_min best best_count rest
    | _->best in
  let c=count_occurences (List.hd img) c in
  min_min (List.hd img) c (List.tl img)
let rec first_n xs n=match xs with
  | [] -> failwith "First_n"
  | x::xs -> if n = 1 then [x] else x::first_n  xs (n-1)

let header output=
  output_string output "P1\n 25 6 \n"

let rec print_layer output layer=
  let sl=String.length layer in
  if sl>0 then begin
    output_string output @@
    Str.global_replace (Str.regexp "." ) "\\0 "
    @@ Str.global_replace (Str.regexp "2") "0"@@String.sub layer 0 25;
    output_char output '\n';
    print_endline @@
    Str.global_replace (Str.regexp "\\(0\\|2\\)") " "@@
    Str.global_replace (Str.regexp "1") "█"@@ String.sub layer 0 25;
    print_layer output (String.sub layer 25 (sl-25))
  end
let num = ref 0


let combine l1 l2=
  let c = String.mapi (fun i x->
      if x = '2' then
        String.get l2 i
      else x
    ) l1 in
  let n = !num in
  num:=!num+1;
  let out=open_out @@ Printf.sprintf "8/%03i.pbm" n in
  header out;
  print_layer out c;print_newline ();
  close_out out;
  c


let ()=
  let f=String.trim @@ load_file "day-8.txt" in
  let image=split_layers f (25*6) in
  Printf.printf "Got %i rows of length %i\n" (List.length image) (String.length @@ List.hd image);
  let best_row=find_min image '0' in
  Printf.printf "%i\n" @@(count_occurences best_row '1'
                          * count_occurences best_row '2');
  let image=List.rev image in
  let final=String.map (fun x->if x=='0' then ' ' else '1' )
    @@ List.fold_left combine  (List.hd image) (List.tl image) in
  for i = 0 to 5 do
    print_string@@
    Str.global_replace (Str.regexp "1") "█"@@ String.sub final (i*25) 25;
    Printf.printf " %i\n" ((i+1)*25);
  done;
  print_endline final;
