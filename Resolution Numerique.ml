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

include "Euler.ml";;
include "RungeKutta.ml";;
include "RepresentationGraphique.ml";;



(*Décharge d'un condensateur (U(x))*)

let RC = 80.;;

let f'(x,y) = 
- ( 1. / RC )* y;;

let numerique = {scallingAuto = false ; quad = true; lineMode = true ;
xmin = 0. ; xmax = 200. ; ymin = 0. ; ymax = 6.; color = red};;
let numerique2 = {scallingAuto = false ; quad = true; lineMode = true ;
xmin = 0. ; xmax = 200. ; ymin = 0. ; ymax = 6.; color = blue};;
let analytique = {scallingAuto = false ; quad = true; lineMode = false ;
xmin = 0. ; xmax = 200. ; ymin = 0. ; ymax = 6.; color = green};;

graph (euler1 f' 6. (0.) 400. 10.) 500 250 30. 1. numerique;;
graph (RK4_1 f' 6.0 0. 400. 10.) 500 250 30. 1. numerique2;;
graph (fonction_to_vect (fun t -> 6. * exp(-t/RC)) 1. 400. 0.1) 500 250 30. 1. analytique;;



(* Fonction Exponentielle*)

let exponentiel(x,y) =
y ;;

graph (euler1 exponentiel 2.71 1. 3. 0.01) 500 250 1. 10. auto;;
graph (RK4_1  exponentiel 2.71 1. 3. 0.001) 500 250 1. 1. auto;;
graph (fonction_to_vect (fun x -> exp(x)) 1. 3. 0.001) 500 250 1. 10. auto;;

(*Systeme masse ressort avec frottement fluide*)

let numerique = {scallingAuto = false ; quad= true; lineMode = true ;
xmin = 0. ; xmax = 500. ; ymin = -5. ; ymax = 5.; color = red};;
let analytique = {scallingAuto = false ; quad = true; lineMode = false ;
xmin = 0. ; xmax = 500. ; ymin = -5. ; ymax = 5.; color = green};;
let numerique2 = {scallingAuto = false ; quad= true; lineMode = true ;
xmin = 0. ; xmax = 500. ; ymin = -5. ; ymax = 5.; color = blue};;

let a = 5. and m = 500. and k = 10.;;

let solution t =
let racineDelta = sqrt( 4. * k / m - (a ^^^^ 2.) / (m ^^^^ 2.)) in
exp( - a * t / (2.*m))*(5.*cos(racineDelta * t /2. ));;


let f''1(x,y,y') =
	- ( a / m ) * y' - ( k / m ) * y ;;

graph (euler2 f''1 0. 5. 0. 500. 3.) 500 250 100. 1. numerique;;
graph (RK4_2 f''1 0. 5. 0. 500. 3.) 500 250 100. 1. numerique2;;
graph (fonction_to_vect solution 0. 500. 0.01) 500 250 100. 1. analytique;;


(* Exemple à deux variables*)

let f''(x,y,y') =
3. * exp( - x ) - 0.4 * y;;

graph (euler2 f'' 0. 5. (0.) 10. 0.0001) 500 250 1. 1. auto;;
graph (RK4_2 f'' 0. 5. (0.) 10. 0.0001) 500 250 1. 1. auto;;


(*Fonction quelconque*)

let perso = {scallingAuto = false ; quad = true; lineMode = false ;xmin = -2. * pi ; xmax = 2. * pi ; ymin = -1. ; ymax = 1.; color = red};;
let f = function x -> cos x;;
graph (fonction_to_vect f (-2. * pi) (2. * pi) 0.001) 500 250 (pi) 1. perso;;

let perso = {scallingAuto = false ; quad = true; lineMode = false ;xmin = -2.; xmax = 2.; ymin = -2.3 ; ymax = 0.5; color = red};;
let f = function t -> (1. - sin t);;
graph (polaire_to_vect f 0. (3. * pi) 0.001) 500 250 (0.5) 1. perso;;




(* Exemple 3 *)
let ex3'(x,y) =
-tan(x) * y;; 

graph (euler1 ex3' 1. (0.) 10. 0.001) 500 250 pi 1. perso;;

(* 	
	- Basé sur Euler.ml.
	- Basé sur Runge Kutta.ml.
	- Basé sur Représentation Graphique.ml.
*)