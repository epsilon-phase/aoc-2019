let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.unsafe_to_string s

type point = { mutable x : int; mutable y : int; mutable z : int }

let print_point (p : point) =
  let { x = a; y = b; z = c } = p in
  Printf.printf "<x=%2i, y=%2i, z=%2i>" a b c

let copy_point (p : point) =
  let { x = a; y = b; z = c } = p in
  { x = a; y = b; z = c }

type moon = { mutable pos : point; mutable velocity : point }

let copy_moon m = { pos = copy_point m.pos; velocity = copy_point m.velocity }

let to_position_array s =
  let lines = List.map String.trim @@ String.split_on_char '\n' s in
  let components =
    List.filter (fun x -> String.length x > 0)
    @@ List.map (Str.global_replace (Str.regexp "\\([^-0-9]\\)") " ") lines
  in
  List.iter print_endline components;
  Array.of_list
  @@ List.map
       (fun x ->
         let l =
           List.map int_of_string
           @@ List.filter (fun x -> String.length x > 0)
           @@ List.map String.trim @@ String.split_on_char ' ' x
         in
         { x = List.nth l 0; y = List.nth l 1; z = List.nth l 2 })
       components

let to_moons arr =
  Array.map (fun pos -> { pos; velocity = { x = 0; y = 0; z = 0 } }) arr

let adjust_velocity (arr : moon array) =
  let rel_vel_adjust a b = if a == b then 0 else if a < b then 1 else -1 in
  for a = 0 to Array.length arr - 1 do
    for b = 0 to Array.length arr - 1 do
      if a != b then (
        let i = arr.(a) in
        let j = arr.(b) in
        i.velocity.x <- i.velocity.x + rel_vel_adjust i.pos.x j.pos.x;

        i.velocity.y <- i.velocity.y + rel_vel_adjust i.pos.y j.pos.y;
        i.velocity.z <- i.velocity.z + rel_vel_adjust i.pos.z j.pos.z )
    done
  done

let step arr =
  adjust_velocity arr;
  for a = 0 to Array.length arr - 1 do
    let i = arr.(a) in
    i.pos.x <- i.pos.x + i.velocity.x;
    i.pos.y <- i.pos.y + i.velocity.y;
    i.pos.z <- i.pos.z + i.velocity.z
  done

let energy m =
  let point_energy m =
    let x = abs m.x in
    let y = abs m.y in
    let z = abs m.z in
    x + y + z
  in
  point_energy m.pos * point_energy m.velocity

let rec gcd a b = if b != 0 then gcd b (a mod b) else a

let lcm a b = abs b * (abs a / gcd a b)

let total_energy (arr : moon array) =
  Array.fold_left (fun x y -> x + energy y) 0 arr

let () =
  let c = to_moons @@ to_position_array @@ load_file "day-12.txt" in
  let test x =
    print_point x.pos;
    print_string " vel=";
    print_point x.velocity;
    print_newline ()
  in
  let q = Array.map copy_moon c in
  Array.iter test c;
  for i = 1 to 1000 do
    step c
  done;
  Array.iter test c;
  Printf.printf "Total energy=%i\n" @@ total_energy c;
  let xr = ref 0 in
  let found_x = ref false in
  let yr = ref 0 in
  let found_y = ref false in
  let zr = ref 0 in
  let found_z = ref false in
  let c = Array.map copy_moon q in
  let component_test ?(half = true) n a b =
    let a, b, c, d =
      match n with
      | 1 -> (a.pos.x, b.pos.x, a.velocity.x, b.velocity.x)
      | 2 -> (a.pos.y, b.pos.y, a.velocity.y, b.velocity.y)
      | _ -> (a.pos.z, b.pos.z, a.velocity.z, b.velocity.z)
    in
    if half then c = 0 else a = b && c = d
  in
  let test_equal = component_test ~half:true in
  while not (!found_x && !found_y && !found_z) do
    step c;
    if not !found_x then (
      xr := !xr + 1;
      if Array.fold_left ( && ) true @@ Array.map2 (test_equal 1) c q then
        found_x := true );
    if not !found_y then (
      yr := !yr + 1;
      if Array.fold_left ( && ) true @@ Array.map2 (test_equal 2) c q then
        found_y := true );
    if not !found_z then (
      zr := !zr + 1;
      if Array.fold_left ( && ) true @@ Array.map2 (test_equal 3) c q then
        found_z := true )
  done;
  Printf.printf "Determined cyclicity over %i steps at round %i\n"
    (2 * lcm !xr (lcm !yr !zr))
    (max !xr (max !yr !zr))
