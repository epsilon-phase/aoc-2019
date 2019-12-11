(*The rest is ours*)
let operate ?(pause_on_output=false) (reader:unit->int) (writer:int->unit) state pc=
  let pc = ref pc in
  let relative_base = ref 0 in
  let instruction ()= state.(!pc) in
  let mode ()=
    let q=(instruction ())in
    ((q/100) mod 10 ,(q/1000) mod 10,(q/10000) mod 10) in
  let arg n=
    let (a,b,c)= mode() in
    let v=state.(!pc+n) in
    match (match n with |1->a|2->b|3->c|_->failwith "Arg indicator too big") with
    | 0->state.(v)
    | 1->v
    | 2->state.(!relative_base+v)
    | _->failwith "Unknown Mode!" in
  let dest n=
    let (a,b,c)=mode () in
    let q=state.(!pc+n) in
    match (match n with |1->a |2->b|3->c|_->c) with
    | 0->q
    | 1->q
    | 2-> !relative_base + q
    |_ ->failwith "Invalid mode" in
  let read ()=(*opcode 3*)
    let c=dest 1 in
    Array.set state c (reader ());
    pc:=!pc+2 in
  let output () = (*opcode 4*)
    let n=arg 1 in
    writer n;
    pc:=!pc+2 in
  let add ()=
    let a=arg 1 in
    let b= arg 2 in
    let c=dest 3 in
    Printf.printf "Adding %i %i into %i\n" a b c;
    Array.set state c (a+b);
    pc:=!pc + 4 in
  let mult ()=
    let a=arg 1 in
    let b=arg 2 in
    Array.set state (dest 3) (a*b);
    pc:=!pc+4 in
  let jit () = (*Opcode 5 jump if true*)
    let a=arg 1 in
    let b=arg 2 in
    (*if a!=0 then
      Printf.printf "Jumping to %i(%i is true)\n" b a;*)
    pc:=if a!=0 then b else !pc + 3 in
  let jif () = (*Opcode 6 jump if false*)
    let a=arg 1 in
    let b=arg 2 in
    if a=0 then
      Printf.printf "(jif)Jumping to %i\n" b;
    pc:=if a==0 then b else !pc+3 in
  let less_than ()=
    let a=arg 1 in
    let b=arg 2 in
    let c =dest 3 in
    Array.set state c (if a<b then 1 else 0);
    (*Printf.printf "%i = %i < %i\n" c a b;*)
    pc:=!pc+4 in
  let equals ()=
    let a = arg 1 in
    let b = arg 2 in
    let c = dest 3 in
    (*Printf.printf "Equality of %i and %i results in %i\n"
      a b (if a=b then 1 else 0);*)
    Array.set state c (if a=b then 1 else 0);
    pc:=!pc+4 in
  let  set_base ()=
    let a=arg 1 in
    relative_base:=!relative_base + a;
    Printf.printf "Setting base to %i\n" !relative_base;
    pc:=!pc+2 in
  let continue = ref true in
  while state.(!pc)!=99 && !continue do
    let (m1,m2,m3) = mode ()in
    Printf.printf "pc at %i, instruction %i(Mode %i,%i,%i)\n" !pc
      (instruction ()) m1 m2 m3;
    let a = instruction () mod 100 in
    match a with
      | 1->add ()
      | 2->mult ()
      | 3->read ()
      | 4->output (); continue := not pause_on_output;
      | 5->jit ()
      | 6->jif ()
      | 7-> less_than ()
      | 8-> equals ()
      | 9->set_base ()
      | x->Printf.printf "got instruction %i\n" x;
        raise (Invalid_argument " Was not a valid instruction" )
  done;
  !pc,state.(!pc)!=99 (*Program counter, resumable*)

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (Bytes.unsafe_to_string s)


let ()=
  let l=load_file "day-11.txt" in
  let state=String.split_on_char ',' l in
  let state=List.map (fun x->
      try int_of_string (String.trim x)
      with Failure s->output_string stdout x;raise (Failure s)
    ) state in
  let rx=ref 0 in
  let ry=ref 0 in
  let dir=ref (0,1) in
  let hull=Hashtbl.create 10 in
  let get_hull_color () =
    if Hashtbl.mem hull (!rx,!ry) then
      Hashtbl.find hull (!rx,!ry)
    else
      0 in
  let print_hull ()=
    let (min_x,min_y) = Hashtbl.fold (fun (ox,oy) _ (nx,ny)->
        (min ox nx),(min oy ny)) hull (10000,10000) in
    let (max_x,max_y)= Hashtbl.fold (fun (ox,oy) _ (nx,ny)->
        max ox nx,max oy ny
      ) hull (0,0) in
    for y =max_y downto min_y do
      for x = min_x to max_x do
        let hv= match Hashtbl.find_opt hull (x,y) with
          | None->0
          | Some n->n in
        print_char @@ match hv with
        | 0->' '
        | _->'#'
      done;
      print_newline ();
    done in
  let set_hull_color n =
    Hashtbl.replace hull (!rx,!ry) n in
  set_hull_color 1;
  let step ()=
    let (a,b)= !dir in
    rx:= !rx+a;
    ry:= !ry+b in
  let turn d=
    let (dx,dy)= !dir in
    dir:=if d=1 then
        (dy,-dx)
      else (-dy,dx) in
  let reader ()= get_hull_color () in
  let output_count=ref 0 in
  let writer n=
    if !output_count mod 2 == 0 then
      set_hull_color n
    else begin
      turn n;
      step ()
    end;
    output_count:= !output_count+1 in
  let state=Array.append (Array.of_list state) (Array.make 10000 0) in
  let (pc,halted)=operate reader writer state 0 in
  Printf.printf "Pc at %i(%i), resumable = %b\n" pc state.(pc) halted;
  Printf.printf "Painted %i panels\n" @@ Hashtbl.length hull;
  print_hull ()
