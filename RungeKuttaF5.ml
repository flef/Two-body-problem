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

(* Runge Kutta F 5 Equa-diff Ordre 1*)

let RKF5_1 f' yo xo xf dx =
	let K1 = ref 0. and K2 = ref 0. and K3 = ref 0. and K4 = ref 0. and K5 = ref 0. and K6 = ref 0.in	
	let x = ref xo and y = ref yo in
	let xx = ref 0 and vect = (create_vect xo xf dx) in
		while !x <= xf do
			(vect.(!xx).(0) <- !x);
			(vect.(!xx).(1) <- !y);
			K1 := dx * f'(!x,!y);
			K2 := dx * f'(!x + (1./4.) * dx, !y + (1./4.) * !K1);
			K3 := dx * f'(!x + (3./8.) * dx, !y + (3./32.) * !K1 + (9./32.) * !K2);
			K4 := dx * f'(!x + (12./13.)*dx, !y + (1932./2197.) * !K1 - (7200./2197.) * ! K2 + (7296./2197.) * !K3);
			K5 := dx * f'(!x + dx          , !y + (439./216.) * !K1 - (8.) * !K2 + (3680./513.) * !K3 - (845./4104.) * !K4 ); 
			K6 := dx * f'(!x + (1./2.) * dx, !y - (8./21.) * !K1 + 2. * !K2 - (3544./2565.) * !K3 + (1859./4104.) * !K4 - (11./40.) * !K5);
			y := !y + (16./135.) * !K1 + (6656./12825.)* !K3 + (28561./56430.) * !K4 - (9./50.) * !K5 + (2./55.) * !K6;
			x := !x + dx;
			xx := (int_of_float(float_of_int(!xx)+ 1.)); 
		done;
vect;;

