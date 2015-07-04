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

(*Création d'une matrice de taille adaptée*)
let create_vect xo xf dx = make_matrix (int_of_float((xf-xo)/dx + 1./dx + dx)) 2 0.;;

(*Euler 1 - récursive*)

let rec euler1 f' yo xo xf dx =
	let y1 = yo + f'(xo,yo) * dx in
	match xo with
	|xo when xo < xf	-> (xo,yo)::(euler1 f' y1 (xo + dx) xf dx); 
	|_ 								-> []
;;

(*Euler 1 - itérative*)

let euler1 f' yo xo xf dx =
	let i = ref xo and ii = ref 0 
		and vect = (create_vect xo xf dx) in
			let y = ref yo in
					while !i <= xf do
					(vect.(!ii).(0) <- !i);
					(vect.(!ii).(1) <- !y);
					y  := !y + f'(!i,!y) * dx;
					i := !i + dx;
					incr ii; 
				done;
			vect;;

(*Euler 2*)

let euler2 f'' y'o yo xo xf dx =
	let y'' = ref (f''(xo,yo,y'o)) and y' = ref y'o and y = ref yo in
		let i = ref xo and ii = ref 0
		and vect = (create_vect xo xf dx) in
			while !i <= xf do
				(vect.(!ii).(0) <- !i);
				(vect.(!ii).(1) <- !y);
				y   := !y + dx * ( !y' + ( !y'' * dx));
				y'  := !y' + !y'' * dx;
				y'' := f''(!i,!y,!y');
				i := !i + dx;
				incr ii; 
			done;
vect;;

(* 	
	- Version finale (Copie de Méthode d'Euler 8.ml)
*)

