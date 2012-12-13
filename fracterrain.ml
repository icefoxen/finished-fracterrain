(* Generates fractal terrain, badly.
   Because I can't waste enough time.
*)

open Sdlvideo
open Sdlkey

let scrx = 600
and scry = 600;;

let drawPoint scr x y height =
  let color = (height,height,height) in

  if must_lock scr then
    lock scr;

  if x < scrx && x > 0 then
    put_pixel_color scr ~x: x ~y: y color
  else
    ();

  if must_lock scr then
    unlock scr
;;

let delta = 0.7
and limit = 128;;

let newHeight oldheight = 
  let heightdiff = int_of_float ((float_of_int oldheight) *. delta) in
  let randheight = (Random.int heightdiff) - (heightdiff / 2) in
    min (max (oldheight + randheight) 0) 128
;;

let getHeight scr x y =
  let c, _, _ = get_pixel_color ~x: x ~y: y scr in
    c
;;


(* This tries to draw a fractal thingy, in a fairly simple manner.
*)
let rec fractalDraw scr xcenter ycenter maxoff height =
(*  Printf.printf "Maxoff: %d xcenter: %d\n" maxoff xcenter; *)
  drawPoint scr xcenter ycenter height;
(*  flip scr; *)

  if maxoff = 0 then
    ()
  else (

      let x1 = xcenter + maxoff
      and y1 = ycenter + maxoff
      and x2 = xcenter - maxoff
      and y2 = ycenter - maxoff in
	fractalDraw scr x1 y1 (maxoff / 2) (newHeight height);
	fractalDraw scr x2 y1 (maxoff / 2) (newHeight height);
	fractalDraw scr x1 y2 (maxoff / 2) (newHeight height);
	fractalDraw scr x2 y2 (maxoff / 2) (newHeight height);
    )
;;

(*
let rec diamondSquareDraw scr x1 y1 x2 y2 =
  let getHeight x y =
    let c, _, _ = get_pixel_color x y in
      c
  in
  let diamond
  

;;
*)



(* Yay, I've created... um, abstract art. 
   This just raises regions randomly, making them overlap.
*)
let randomAdditions scr maxx maxy regionsize =
  let maxHeight = 250 in
  let currentMax = ref 0 in



  let fillRegion x y heightadd =
    for x1 = x to (x+regionsize) do
      for y1 = y to (y+regionsize) do
	let height = getHeight scr x1 y1 in

	(* Soften the edges!  Might improve the overall look *)
(*
	let delta = abs ((heightadd / 2) - 
	let heightadd = 
*)

	  drawPoint scr x1 y1 (height + heightadd);
	  currentMax := (max !currentMax (height+heightadd));
      done;
    done
  in
    


    while !currentMax < maxHeight do
      let x = Random.int maxx
      and y = Random.int maxy in
(*      let height = getHeight x y in *)
(*	drawPoint scr x y (height + 10); *)
	fillRegion x y 10;

    done
;;


(* This works by taking a dividing line at random, and raising everything
   on one side of it by a small amount.

   If we wanted to do it right, we'd do random orientations of lines,
   but I'm lazy.
*)
let raiseLine scr maxx maxy times amount =

  let fillRegion x maxx y maxy =
    for x1 = x to maxx do
      for y1 = y to maxy do
	let height = getHeight scr x1 y1 in
	  drawPoint scr x1 y1 (height + amount);
      done;
    done
  in

    for i = 0 to times do
      Printf.printf "Doing iteration %d\r" i;
      flush stdout;


      if (Random.bool ()) then
	(* Divide along x *)
	let divline = Random.int maxx in
	  if Random.bool () then
	    (* Fill right of the line *)
	    fillRegion divline maxx 0 maxy
	  else
	    (* Fill left of the line *)
	    fillRegion 0 divline 0 maxy
      else
	(* Divide along y *)
	let divline = Random.int maxy in
	  if Random.bool () then
	    (* Fill below line *)
	    fillRegion 0 maxx divline maxy
	  else
	    (* Fill above line *)
	    fillRegion 0 maxx 0 divline;



    done
;;


let mainloop scr =
  fill_rect scr (map_RGB scr black);
  fractalDraw scr ((scrx) / 2 + 1) ((scrx) / 2 + 1) 128 128;
  flip scr;
  Sdltimer.delay 5000;

  fill_rect scr (map_RGB scr black);
  randomAdditions scr 400 400 25;
  flip scr;
  Sdltimer.delay 5000;

  fill_rect scr (map_RGB scr black);
  raiseLine scr 200 200 300 10;
  flip scr;
  Sdltimer.delay 5000;

;;  

  
  

let main () =
  Sdl.init [`EVERYTHING];
  Random.self_init ();

  let scr = set_video_mode ~w: scrx ~h: scry ~bpp: 16
    [`DOUBLEBUF] in

    mainloop scr;

    Sdl.quit ();
;;

let _ = main ();;
