type orbit = Node of string * int * orbit array | Leaf of string * int

let read_file f =
  let f = open_in f in
  let b = Hashtbl.create 10 in
  try
    while true do
      let l = input_line f in
      let l = List.map String.trim @@ String.split_on_char ')' l in
      Hashtbl.add b (List.hd l) (List.hd @@ List.tl l)
    done;
    b
  with End_of_file ->
    close_in f;
    b

let rec to_graph t depth node =
  let successors = Hashtbl.find_all t node in
  if List.length successors = 0 then Leaf (node, depth)
  else
    Node
      ( node,
        depth,
        Array.of_list @@ List.map (to_graph t (depth + 1)) successors )

let rec checksum graph =
  match graph with
  | Leaf (_, n) -> n
  | Node (_, n, l) -> n + (Array.fold_left ( + ) 0 @@ Array.map checksum l)

type augmented_orbit =
  | Leaf of string * int * int
  | Node of string * int * int * augmented_orbit array

let not_found_const = 32 * 32 * 32 * 32

let rec augment_orbit (orbit : orbit) : augmented_orbit =
  let cost x =
    match x with Leaf (_, a, b) -> (a, b) | Node (_, a, b, _) -> (a, b)
  in
  match orbit with
  | Leaf (s, _) -> (
      match s with
      | "YOU" -> Leaf (s, 0, not_found_const)
      | "SAN" -> Leaf (s, not_found_const, 0)
      | _ -> Leaf (s, not_found_const, not_found_const) )
  | Node (s, _, l) ->
      let l = Array.map augment_orbit l in
      let a, b =
        Array.fold_left
          (fun (x, y) (w, z) -> (min x w, min y z))
          (not_found_const * 2, not_found_const * 2)
        @@ Array.map cost l
      in
      let a = if a < not_found_const && b < not_found_const then a else a + 1 in
      let b = if b < not_found_const && a < not_found_const then b else b + 1 in
      Node (s, a, b, l)

let () =
  let h = to_graph (read_file "day-6.txt") 0 "COM" in
  print_int @@ checksum h;
  print_newline ();
  match augment_orbit h with
  | Node (_, a, b, _) -> Printf.printf "%i + %i->%i\n" a b (a + b)
  | _ -> print_endline "What?"
