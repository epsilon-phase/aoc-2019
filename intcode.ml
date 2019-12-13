let operate ?(pause_on_output = false) (reader : unit -> int)
    (writer : int -> unit) state pc =
  let pc = ref pc in
  let relative_base = ref 0 in
  let instruction () = state.(!pc) in
  let mode () =
    let q = instruction () in
    (q / 100 mod 10, q / 1000 mod 10, q / 10000 mod 10)
  in
  let arg n =
    let a, b, c = mode () in
    let v = state.(!pc + n) in
    match
      match n with
      | 1 -> a
      | 2 -> b
      | 3 -> c
      | _ -> failwith "Arg indicator too big"
    with
    | 0 -> state.(v)
    | 1 -> v
    | 2 -> state.(!relative_base + v)
    | _ -> failwith "Unknown Mode!"
  in
  let dest n =
    let a, b, c = mode () in
    let q = state.(!pc + n) in
    match match n with 1 -> a | 2 -> b | 3 -> c | _ -> c with
    | 0 -> q
    | 1 -> q
    | 2 -> !relative_base + q
    | _ -> failwith "Invalid mode"
  in
  let read () =
    (*opcode 3*)
    let c = dest 1 in
    state.(c) <- reader ();
    pc := !pc + 2
  in
  let output () =
    (*opcode 4*)
    let n = arg 1 in
    writer n;
    pc := !pc + 2
  in
  let add () =
    let a = arg 1 in
    let b = arg 2 in
    let c = dest 3 in
    Printf.printf "Adding %i %i into %i\n" a b c;
    state.(c) <- a + b;
    pc := !pc + 4
  in
  let mult () =
    let a = arg 1 in
    let b = arg 2 in
    state.(dest 3) <- a * b;
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
    let a = arg 1 in
    let b = arg 2 in
    if a = 0 then Printf.printf "(jif)Jumping to %i\n" b;
    pc := if a == 0 then b else !pc + 3
  in
  let less_than () =
    let a = arg 1 in
    let b = arg 2 in
    let c = dest 3 in
    state.(c) <- (if a < b then 1 else 0);
    (*Printf.printf "%i = %i < %i\n" c a b;*)
    pc := !pc + 4
  in
  let equals () =
    let a = arg 1 in
    let b = arg 2 in
    let c = dest 3 in
    (*Printf.printf "Equality of %i and %i results in %i\n"
      a b (if a=b then 1 else 0);*)
    state.(c) <- (if a = b then 1 else 0);
    pc := !pc + 4
  in
  let set_base () =
    let a = arg 1 in
    relative_base := !relative_base + a;
    Printf.printf "Setting base to %i\n" !relative_base;
    pc := !pc + 2
  in
  let continue = ref true in
  while state.(!pc) != 99 && !continue do
    let m1, m2, m3 = mode () in
    Printf.printf "pc at %i, instruction %i(Mode %i,%i,%i)\n" !pc
      (instruction ()) m1 m2 m3;
    let a = instruction () mod 100 in
    match a with
    | 1 -> add ()
    | 2 -> mult ()
    | 3 -> read ()
    | 4 ->
        output ();
        continue := not pause_on_output
    | 5 -> jit ()
    | 6 -> jif ()
    | 7 -> less_than ()
    | 8 -> equals ()
    | 9 -> set_base ()
    | x ->
        Printf.printf "got instruction %i\n" x;
        raise (Invalid_argument " Was not a valid instruction")
  done;
  (!pc, state.(!pc) != 99)

(*Program counter, resumable*)
