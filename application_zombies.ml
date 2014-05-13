#load "graphics.cma";;

#load "unix.cma";;

open Graphics;;

open List;;

open Unix;;

exception Touchee of int;;

let pi = 4.0 *. atan 1.0;;

let direc = ref ([||]);;

let compteur = ref 0;;

Random.init (int_of_float (time ()));;

let modulo a b = if a mod b >= 0 then (a mod b) else (a mod b + b);;

let random p q = Random.int (q) < p;;

type agent = int*int*float ;;

let vide_all () = Graphics.set_color white; Graphics.fill_rect 0 0 1200;;

let new_agent () = let a = Random.int 1200 in let b = Random.int 800 in let c = (float_of_int (Random.int 4000)/. 400.0) in
		Graphics.fill_circle a b 4; (a,b,c);;
		
let vide a b color = Graphics.set_color white; Graphics.fill_circle a b 4; Graphics.set_color color;;

let norm a b = let c = float_of_int a in let d = float_of_int b in sqrt (c*.c+.d*.d);;

let inertie a b x y = (int_of_float ( -.(x/.(norm (a-1190) 0)) +. (y/.(norm (a-10) 0))),
									 int_of_float (-.(x/.(norm (b-790) 0)) +. (y/.(norm (b-20) 0))) );;
									 
let   move_proie x y z tab  = let n = ref 0 in let m = ref (1000000.0) in
		for i = 0 to (Array.length (tab)-1) do 
			let (a,b,c) = tab.(i) in 
				if  !m > ((floor (norm (a-x) (b-y)))) then 
					(m := ((floor (norm (a-x) (b-y)))); n := i)
		done; 
		let (a,b,c) = tab.(!n) in let (u,v) = inertie x y 100. 200. in 
		let abc =  -int_of_float ((z*. float_of_int ((a-x)))/.(!m)) + x + u (*+ (Random.int (5)-2) *)in 
		let ord =  -int_of_float ((z*. float_of_int ((b-y)))/.(!m)) + y + v (*+ (Random.int (5)-2) *)in 
		(vide x y green; Graphics.fill_circle abc ord 4; (abc, ord,z));; 

let essaie plop = if random 1 10 then float_of_int (Random.int 360)  else !direc.(plop);;

let  move_agent a b c plop = 
	let x = essaie plop in let (z,t) = inertie a b 100. 200. in 
		let abc = int_of_float (cos((2.0*.pi*.x)/.360.0)*.(c) +. float_of_int (a)) + z in 
		let ord = 	int_of_float (sin((2.0*.pi*.x)/.360.0)*.(c) +. float_of_int (b)) + t in 
		 vide a b black; Graphics.fill_circle abc ord 4; (abc,ord,c)
		  ;;
		  
let ajout_tab tab x = let tableau = Array.make (Array.length tab + 1)  (0,0,0.) in 
		for i = 0 to (Array.length tab -1) do 
			tableau.(i) <- tab.(i)
		done; 
		tableau.(Array.length tab) <- x; tableau;;

let enleve_tab tab n= if Array.length tab = 1 then [||] 
else 
let tableau = Array.make (Array.length tab - 1)  (0,0,0.) in 
		if n = 0 then 
		(for i = 1 to (Array.length tab - 1) do 			
			   tableau.(i-1) <- tab.(i)
		done; tableau) 
		else if n = (Array.length tab - 1) then
		(for i = 0 to (Array.length tab - 2) do 
		tableau.(i) <- tab.(i)
		done; tableau)
		else
		(for i = 0 to (n - 1) do 			
			   tableau.(i) <- tab.(i)
		    done; 
		for i = (n+1) to (Array.length tab - 1) do 			
			   tableau.(i-1) <- tab.(i)
		done; tableau)
		;;
		
let init_tab n = let tab = Array.make n 0. in 
	for i = 0 to (n-1) do 
		tab.(i) <- float_of_int (Random.int 360)
	done; tab;;
		
let poursuit x y z  tab = let n = ref 0 in let m = ref (1000000.0) in
		(*if x < 4 then raise Touchee 
		else*)
		for i = 0 to (Array.length (tab)-1) 
				do 
			let (a,b,c) = tab.(i) in 
				if  !m > ((floor (norm (a-x) (b-y)))/.c) then 
					(m := ((floor (norm (a-x) (b-y)))/.c); n := i)
	done;
		if !m < 5. then raise (Touchee (!n))
		else if !m < 200. then 
			let (a,b,c) = tab.(!n) in let (u,v) = inertie x y 0. 0. in 
    	  let abc =  int_of_float ((z*. float_of_int ((a-x)))/.((floor (norm (a-x) (b-y))))) + x + u in 
	  let ord =  int_of_float ((z*. float_of_int ((b-y)))/.((floor (norm (a-x) (b-y))))) + y + v in 
	  vide x y black; Graphics.fill_circle abc ord 4; (abc, ord,z)
		else 
				(x,y,z);;
				
let rec main tab_ag tab_proie  = 
if Array.length tab_proie = 0 then main (ajout_tab tab_proie tab_ag.(0)) (enleve_tab tab_ag 0)
else
if (!compteur = 50 || (Array.length (!direc) < Array.length (tab_ag))) then (compteur := 0; 
direc := init_tab (Array.length tab_ag))
else compteur := !compteur + 1;
let l = Array.length tab_ag  in 
	for i = 0 to (l-1) do 
		let (a,b,c) = tab_ag.(i) in 
			try
			if tab_ag.(i) = poursuit a b c  tab_proie then 
			tab_ag.(i) <- move_agent a b c i			
			else
		  tab_ag.(i) <- poursuit a b c  tab_proie   
		  with Touchee n -> ignore (Unix.system "sleep 0.02"); vide_all () ;main (ajout_tab tab_ag tab_proie.(n)) (enleve_tab tab_proie n)
	done;
	
let w = Array.length tab_proie  in 
	for i = 0 to (w-1) do 
		let (a,b,c) = tab_proie.(i) in 
		  tab_proie.(i) <- move_proie a b c tab_ag
	done;
 ignore (Unix.system "sleep 0.02"); set_color blue; draw_rect 10 20 1180 770  ; main tab_ag tab_proie ;;
 
open_graph " 1200x800";;
set_window_title "test";;
set_color blue;;
draw_rect 10 20 1180 770 ;;
let tab_proie = Array.make 30 (0,0,0.0);;
for i = 0 to 29 do tab_proie.(i) <- new_agent () done ;;
let tab_ag = [|(700,500,6.5)|];;
(*let tab = [|(500,500,11.); (900,500,10.); new_agent (); new_agent (); new_agent (); new_agent ();
new_agent (); new_agent (); new_agent ()|] ;;*)
tab_proie ;;
main  tab_ag tab_proie;;
tab_ag;;
tab_proie ;;
enleve_tab tab_proie 0;;
