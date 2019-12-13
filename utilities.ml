let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (Bytes.unsafe_to_string s)

let load_intcode f=
  let s=load_file f in
  let arr=Array.of_list @@ List.map int_of_string @@ String.split_on_char ',' @@ String.trim s in
  Array.append arr @@ Array.make 10000 0
