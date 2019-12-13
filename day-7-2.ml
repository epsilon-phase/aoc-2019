(*Following from https://gist.github.com/Bamco/6151962*)
(* interleave 1 [2;3] = [ [1;2;3]; [2;1;3]; [2;3;1] ] *)
let rec interleave x lst =
  match lst with
  | [] -> [ [ x ] ]
  | hd :: tl -> (x :: lst) :: List.map (fun y -> hd :: y) (interleave x tl)

(*permutations [1; 2; 3] = [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]] *)
let rec permutations lst =
  match lst with
  | hd :: tl -> List.concat (List.map (interleave hd) (permutations tl))
  | _ -> [ lst ]

(*The rest is ours*)
let operate state input pc =
  let pc = ref pc in
  let instruction () = state.(!pc) in
  let mode () =
    let q = instruction () in
    (q / 100 mod 10, q / 1000 mod 10, q / 10000 mod 10)
  in
  let arg n =
    let a, b, c = mode () in
    match n with
    | 1 -> if a == 1 then state.(!pc + 1) else state.(state.(!pc + 1))
    | 2 -> if b == 1 then state.(!pc + 2) else state.(state.(!pc + 2))
    | 3 -> if c == 1 then state.(!pc + 3) else state.(state.(!pc + 3))
    | _ -> raise (Invalid_argument "Fack")
  in
  let read () =
    (*opcode 3*)
    ( if Stack.length input == 0 then (
      output_string stdout "READING>";
      state.(state.(!pc + 1)) <- read_int () )
    else
      let a = Stack.pop input in
      (*Printf.printf "Recieved %i\n" a;*)
      state.(state.(!pc + 1)) <- a );
    pc := !pc + 2
  in
  let output () =
    (*opcode 4*)
    let a, _, _ = mode () in
    let n = arg 1 in
    Printf.printf "%i\n" n;
    Stack.push n input;
    pc := !pc + 2
  in
  let add () =
    let a = arg 1 in
    let b = arg 2 in
    let c = state.(3 + !pc) in
    (*Printf.printf "Adding %i %i into %i\n" a b c;*)
    state.(c) <- a + b;
    pc := !pc + 4
  in
  let mult () =
    let a = arg 1 in
    let b = arg 2 in
    state.(state.(3 + !pc)) <- a * b;
    pc := !pc + 4
  in
  let jit () =
    (*Opcode 5 jump if true*)
    let a = arg 1 in
    let b = arg 2 in
    (*if a!=0 then
      Printf.printf "Jumping to %i(%i is true)\n" b a;*)
    pc := if a != 0 then b else !pc + 3
  in
  let jif () =
    (*Opcode 6 jump if false*)
    let a, b, _ = mode () in
    let a = arg 1 in
    let b = arg 2 in
    if a = 0 then Printf.printf "Jumping to %i\n" b;
    pc := if a == 0 then b else !pc + 3
  in
  let less_than () =
    let a = arg 1 in
    let b = arg 2 in
    let c = state.(!pc + 3) in
    state.(c) <- (if a < b then 1 else 0);
    (*Printf.printf "%i = %i < %i\n" c a b;*)
    pc := !pc + 4
  in
  let equals () =
    let a = arg 1 in
    let b = arg 2 in
    let c = state.(!pc + 3) in
    (*Printf.printf "Equality of %i and %i results in %i\n"
      a b (if a=b then 1 else 0);*)
    state.(c) <- (if a = b then 1 else 0);
    pc := !pc + 4
  in
  let continue = ref true in
  while state.(!pc) != 99 && !continue do
    let m1, m2, m3 = mode () in
    (*Printf.printf "pc at %i, instruction %i(Mode %i,%i,%i)\n" !pc
      (instruction ()) m1 m2 m3;*)
    let a = instruction () mod 100 in
    match a with
    | 1 -> add ()
    | 2 -> mult ()
    | 3 -> read ()
    | 4 ->
        output ();
        continue := false
    | 5 -> jit ()
    | 6 -> jif ()
    | 7 -> less_than ()
    | 8 -> equals ()
    | x ->
        Printf.printf "got instruction %i\n" x;
        raise (Invalid_argument "Was not a valid instruction")
  done;
  (!pc, state.(!pc) != 99)

(*Program counter, halted*)

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.unsafe_to_string s

let run_permutations program lst =
  let permutes = permutations lst in
  let stack = Stack.create () in
  let m = ref 0 in
  List.iter
    (fun x ->
      Stack.push 0 stack;
      let states = Array.init (List.length x) (fun _ -> Array.copy program) in
      let first = ref true in
      List.iter (fun z -> Printf.printf "%i " z) x;
      let pcs = Array.make (List.length x) (0, true) in
      print_newline ();
      while Array.fold_left (fun x (_, y) -> x && y) true pcs do
        List.iteri
          (fun i y ->
            if !first then Stack.push y stack;
            let p, _ = pcs.(i) in
            Array.set pcs i @@ operate states.(i) stack p)
          x;
        first := false
      done;
      let t = Stack.pop stack in
      assert (Stack.length stack == 0);
      if !m < t then (
        List.iter (fun z -> Printf.printf "%i " z) x;
        m := t;
        Printf.printf "->%i" !m ))
    permutes;
  print_int !m

let () =
  let l = load_file "day-7.txt" in
  let state = String.split_on_char ',' l in
  let max_output = ref 0 in
  let state =
    List.map
      (fun x ->
        try int_of_string (String.trim x)
        with Failure s ->
          output_string stdout x;
          raise (Failure s))
      state
  in
  let input_stack = Stack.create () in
  let state = Array.append (Array.of_list state) (Array.make 10000 0) in
  (*operate (Array.copy state) input_stack;*)
  run_permutations state [ 5; 6; 7; 8; 9 ]
