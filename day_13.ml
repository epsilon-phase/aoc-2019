open Intcode;;
open Utilities;;
open Graphics;;
let () =
  let state = load_intcode "day-13.txt" in
  let screenstate = Hashtbl.create 10 in
  let phase = ref 0 in
  let x=ref 0 in
  let y=ref 0 in
  let score = ref 0 in
  (*auto_synchronize false;
  display_mode false;*)
  let draw_command x y tile=
    if x= -1 && y=0 then begin
      set_color white;
      fill_rect 100 10 100 10;
      moveto 100 10;
      set_color black;
      draw_string @@ string_of_int !score
    end else begin
      let y=43-y in
      let color=match tile with
        |0->white
        |1->black
        |2->red
        |3->green
        |4->blue
        |_->white in
      set_color color;
      if tile == 4 then
        fill_circle (x*10+5) (y*10+5) 5
      else
        fill_rect (x*10) (y*10) 10 10
    end in
  let _draw_screen () =
    set_color white;
    fill_rect 100 10 100 10;
    moveto 100 10;
    set_color black;
    draw_string @@ string_of_int !score;
    Hashtbl.iter (fun (x,y) tile->
        let y=43-y in
        let color=match tile with
          |0->white
          |1->black
          |2->red
          |3->green
          |4->blue
          |_->white in
        set_color color;
        if tile == 4 then
          fill_circle (x*10+5) (y*10+5) 5
        else
          fill_rect (x*10) (y*10) 10 10
      ) screenstate;
    synchronize () in
  let reader () =
    let c=read_key () in
    match c with
    | 'a' -> -1
    | 'd'-> 1
    | _-> 0 in
  let writer n=
    match !phase with
    | 0 -> x:=n; phase:= !phase+1
    | 1 -> y:=n;phase:= !phase + 1
    | 2 -> begin
        if !x == -1 && !y == 0 then
          score:=max !score n
        else
          Hashtbl.replace screenstate (!x,!y) n;
        phase:=0;
        draw_command !x !y n
      end
    | _ -> failwith "Invalid draw phase!" in
  Array.set state 0 2;
  open_graph "";
  let (_a,_b) = operate reader writer state 0 in
  let (max_x,max_y) = Hashtbl.fold (fun (x,y) _ (mx, my)->max x mx, max y my) screenstate (0,0) in
  let blocks = Hashtbl.fold (fun _ tile n->n+ if tile == 2 then 1 else 0) screenstate 0 in
  Printf.printf "%i blocks remain on screen(screensize =%i,%i,score=%i)\n" blocks max_x max_y !score;
  
