open Utilities
open Intcode

let () =
  let l = load_file "day-11.txt" in
  let state = String.split_on_char ',' l in
  let state =
    List.map
      (fun x ->
        try int_of_string (String.trim x)
        with Failure s ->
          output_string stdout x;
          raise (Failure s))
      state
  in
  let rx = ref 0 in
  let ry = ref 0 in
  let dir = ref (0, 1) in
  let hull = Hashtbl.create 10 in
  let get_hull_color () =
    if Hashtbl.mem hull (!rx, !ry) then Hashtbl.find hull (!rx, !ry) else 0
  in
  let print_hull () =
    let min_x, min_y =
      Hashtbl.fold
        (fun (ox, oy) _ (nx, ny) -> (min ox nx, min oy ny))
        hull (10000, 10000)
    in
    let max_x, max_y =
      Hashtbl.fold
        (fun (ox, oy) _ (nx, ny) -> (max ox nx, max oy ny))
        hull (0, 0)
    in
    for y = max_y downto min_y do
      for x = min_x to max_x do
        let hv =
          match Hashtbl.find_opt hull (x, y) with None -> 0 | Some n -> n
        in
        print_char @@ match hv with 0 -> ' ' | _ -> '#'
      done;
      print_newline ()
    done
  in
  let set_hull_color n = Hashtbl.replace hull (!rx, !ry) n in
  set_hull_color 1;
  let step () =
    let a, b = !dir in
    rx := !rx + a;
    ry := !ry + b
  in
  let turn d =
    let dx, dy = !dir in
    dir := if d = 1 then (dy, -dx) else (-dy, dx)
  in
  let reader () = get_hull_color () in
  let output_count = ref 0 in
  let writer n =
    if !output_count mod 2 == 0 then set_hull_color n
    else (
      turn n;
      step () );
    output_count := !output_count + 1
  in
  let state = Array.append (Array.of_list state) (Array.make 10000 0) in
  let pc, halted = operate reader writer state 0 in
  Printf.printf "Pc at %i(%i), resumable = %b\n" pc state.(pc) halted;
  Printf.printf "Painted %i panels\n" @@ Hashtbl.length hull;
  print_hull ()
