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

(* Runge Kutta 4 Equa-diff Ordre 1*)

let RK4_1 f' yo xo xf dx =
	let K1 = ref 0. and K2 = ref 0. and K3 = ref 0. and K4 = ref 0. in	
	let x = ref xo and y = ref yo in
	let xx = ref 0 and vect = (create_vect xo xf dx) in
		while !x <= xf do
			(vect.(!xx).(0) <- !x);
			(vect.(!xx).(1) <- !y);
			K1 := f'(!x,!y);
			K2 := f'(!x + 0.5 * dx , !y + 0.5 * !K1 * dx);
			K3 := f'(!x + 0.5 * dx , !y + 0.5 * !K2 * dx);
			K4 := f'(!x + 0.5 , !y + !K3 * dx);
			y := !y + (!K1 + 2. * !K2 + 2. * !K3 + !K4) * dx / 6. ;
			x := !x + dx;
			incr xx; 
		done;
vect;;


(* Runge Kutta 4 Equa-diff Ordre 2*)

let RK4_2 f'' y'o yo xo xf dx =
	let K1 = ref 0. and K2 = ref 0. and K3 = ref 0. and K4 = ref 0. 
	and J1 = ref 0. and J2 = ref 0. and J3 = ref 0. and J4 = ref 0. 
	and x = ref xo and y = ref yo and y' = ref y'o in
	let xx = ref 0 and vect = (create_vect xo xf dx) in
		while !x <= xf do
			(vect.(!xx).(0) <- !x);
			(vect.(!xx).(1) <- !y);
			J1 := !y'*dx;
			K1 := f''(!x, !y, !y')*dx;
			J2 := (!y' + !K1 / 2.)*dx;
			K2 := f''(!x,( !y + !J1 / 2.),(!y' + !K1 / 2.))*dx;
			J3 := (!y' + !K2 / 2.)*dx;
			K3 := f''(!x, (!y + !J2 / 2.), (!y' + !K2 / 2.))*dx;
			J4 := (!y' + !K3)*dx;
			K4 := f''(!x, (!y + !J3), (!y' + !K3))*dx;
			y' := !y' + (!K1 + 2. * !K2 + 2. * !K3 + !K4)/ 6. ;
			y  := !y  + (!J1 + 2. * !J2 + 2. * !J3 + !J4)/ 6. ;
			x  := !x + dx;
			incr xx; 
		done;
vect;;

(* 	
	- Version finale (Copie de Runge Kutta 4.ml)
*)
