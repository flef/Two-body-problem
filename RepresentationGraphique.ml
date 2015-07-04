(*
 	Two-Body-Problem
    Copyright (C) 2010-2011  Florian Lefèvre

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)
#open "float";;

(*Fonction "plot" améliorée*)

#open "graphics";;

type options = {scallingAuto : bool ;quad : bool ;lineMode : bool; xmin : float; xmax : float; ymin : float; ymax : float; color : color };;
let auto = {scallingAuto = true; quad = true; lineMode = true; xmin = 0. ; xmax = 0.; ymin = 0. ; ymax = 0.; color = blue };;


let prefix ++ a b = int_of_float(float_of_int(a) + float_of_int(b)) ;;
let prefix -- a b = int_of_float(float_of_int(a) - float_of_int(b)) ;;
let prefix ** a b = int_of_float(float_of_int(a) * float_of_int(b)) ;;
let prefix ^^ a b = int_of_float( power (float_of_int(a)) (float_of_int(b)));;
let prefix ^^^^ a b = power a b;;
let prefix // a b = int_of_float(float_of_int(a) / float_of_int(b)) ;;

let pi = acos(-1.);;


let drawNum x =
match x with
|x when x = 0. -> draw_string("0")
|x when mod_float x pi = 0. -> draw_string((string_of_int((int_of_float(x / pi))))^"pi")
|x when float_of_int(int_of_float(x)) = x -> draw_string(string_of_int(int_of_float(x)))
|_ -> draw_string(string_of_float(x));;


let moins x =
(0 -- x);;


let abs_or_nul x =
match x with
|x when x < 0. -> -x
|x when x > 0. -> 0.
|_ -> x;;

let graph vect x y sx sy options=
	let fenetre = string_of_int(x)^"x"^string_of_int(y) in
	open_graph fenetre;
	(*clear_graph();*)
	let xmin = ref vect.(0).(0) and xmax = ref 0.
	and ymin = ref 0. and ymax = ref 0. in
		if options.scallingAuto 
		then 
			begin
			let n = ref vect.(0).(1) and  m = ref vect.(0).(0) in 
				for i=0 to ((vect_length vect) -- 100) do
					n := vect.(i).(1);
					m := vect.(i).(0);
					if !n >= !ymax then ymax := !n;
					if !n <= !ymin then ymin := !n;
					if !m >= !xmax then xmax := !m;
					if !m <= !xmin then xmin := !m;
				done;
		end
		else
			begin
			xmin := options.xmin ;xmax := options.xmax;
			ymin := options.ymin ;ymax := options.ymax;
			end;
		(*Définition de l'échelle*)
		let scalling_x = float_of_int(x) / (!xmax - !xmin) and scalling_y = float_of_int(y) / (!ymax - !ymin) in
		(*Traçage du vecteur*)
		set_color options.color;
		if options.lineMode 
		then
		(*LineMode, Traçage par ligne*)
			begin
				moveto (int_of_float(((vect.(0).(0) -(!xmin)) * scalling_x)))
 							 (int_of_float(((vect.(0).(1) -(!ymin)) * scalling_y)));
				for i=1 to ((vect_length vect) -- 1) do
				(*Correctif pour les vecteurs se finnissant par (0,0)*)
				if (((int_of_float(((vect.(i).(0) -(!xmin)) * scalling_x))) <> 0) &&
					 ((int_of_float(((vect.(i).(1) -(!ymin)) * scalling_y))) <> 0)) 
				then begin
				lineto (int_of_float(((vect.(i).(0) -(!xmin)) * scalling_x)))
 							 (int_of_float(((vect.(i).(1) -(!ymin)) * scalling_y)));
						 end
				done;
			end
		else 
		(*Traçage par point*)
			begin 
				for i=0 to ((vect_length vect) -- 1) do
				plot 	(int_of_float(((vect.(i).(0) -(!xmin)) * scalling_x)))
 							(int_of_float(((vect.(i).(1) -(!ymin)) * scalling_y)));
				done;
			end;
		set_color black;
		(*Affichage des axes*)
			moveto (0) (int_of_float((- !ymin) * (scalling_y)));
			lineto (x) (int_of_float((- !ymin) * (scalling_y)));
			moveto (int_of_float((abs_or_nul !xmin) * (scalling_x))) (0);
			lineto (int_of_float((abs_or_nul !xmin) * (scalling_x))) (y);
		(* Graduation des axes *)
		for i= int_of_float(!xmin / sx - 1.) to int_of_float(!xmax / sx + 1.) do
			moveto (int_of_float((float_of_int(i) * sx  - !xmin) * (scalling_x))) (int_of_float(((- !ymin)* (scalling_y) + 0.)));
			lineto (int_of_float((float_of_int(i) * sx  - !xmin) * (scalling_x))) (int_of_float(((- !ymin)* (scalling_y) + 5.)));
			drawNum (float_of_int(i)*sx);
		done;
		for i= int_of_float(!ymin / sy - 1.)  to int_of_float(!ymax / sy + 1.) do
			moveto (int_of_float(((abs_or_nul !xmin)* (scalling_x) + 0.))) (int_of_float((float_of_int(i) * sy - !ymin)* (scalling_y)));
			lineto (int_of_float(((abs_or_nul !xmin)* (scalling_x) + 5.))) (int_of_float((float_of_int(i) * sy - !ymin)* (scalling_y)));
			drawNum (float_of_int(i)*sy);
		done;
		(*Affichage du cadrillage*)
		if options.quad then
			begin
				for i = 0 to x do
					for j = (int_of_float(!ymin) -- 1)  to int_of_float(!ymax / sy + 1.)  do
					plot (i ++ i mod 2) (int_of_float((float_of_int(j) * sy - !ymin)* (scalling_y)));
					done;
				done;
				for i = 0 to y do
					for j = (int_of_float(!xmin) -- 1) to int_of_float(!xmax /  sx + 1.) do
					plot (int_of_float((float_of_int(j) * sx - !xmin)* (scalling_x))) (i ++ i mod 2);
					done;
				done;
			end
;;

(*Création d'une matrice de taille adaptée*)
let create_vect xo xf dx = make_matrix (int_of_float((xf-xo)/dx + 1./dx)) 2 0.;;

(*Vectorisation des valeurs d'une fonction quelconque*)
let fonction_to_vect f xo xf dx =
	let i = ref xo and ii = ref 0 
		and vect = (create_vect xo xf dx) in
			let y = ref (f(xo)) in
					while !i <= xf do
					(vect.(!ii).(0) <- !i);
					(vect.(!ii).(1) <- !y);
					y  := f(!i);
					i := !i + dx;
					ii:= (int_of_float(float_of_int(!ii)+ 1.)); 
					done;
			vect;;


(*Vectorisation des valeurs d'une courbe polaire r=f(T)*)
let polaire_to_vect f t0 tf dt =
	let t = ref t0 and tt = ref 0 
		and vect = (create_vect t0 tf dt) in
			let r = ref (f(t0)) in
					while !t <= tf do
					(vect.(!tt).(0) <- (!r * cos(!t)));
					(vect.(!tt).(1) <- (!r * sin(!t)));
					r := f(!t);
					t := !t + dt;
					tt:= (int_of_float(float_of_int(!tt)+ 1.)); 
					done;
			vect;;

(*
	- Version finale basée sur Représentation Graphique 2.ml.
*)

