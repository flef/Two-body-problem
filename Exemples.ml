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

include "SystemeNonPerturbe.ml";;

(*Exemple 1*)
(*Deux masses sans vitesse initiale en interaction gravitationnelle*)

let A = {masse = 5e12; rayon = 5; coord_x = 250. ; coord_y = 200.; vitesse_x = 0.; vitesse_y = 0.};;
let B = {masse = 1e13; rayon = 7; coord_x = 50.; coord_y = 300.; vitesse_x = 0.; vitesse_y = 0.};;

solve A B 0.001;;

(*Exemple 2*)
(*Cas d'une masse négligeable devant la seconde*)

let A = {masse = 1e17; rayon = 25; coord_x = 300. ; coord_y = 200.; vitesse_x = 0.; vitesse_y = 0.};;
let B = {masse = 300.; rayon = 3; coord_x = 450.; coord_y = 200.; vitesse_x = 0.; vitesse_y = 200.};;

solve A B 0.0001;;

(*Exemple 3*)
(*Deux masses égales en interaction*)

let A = {masse = 3e18; rayon = 5; coord_x = 150.; coord_y = 200.; vitesse_x = 0.;  vitesse_y = -500. };;
let B = {masse = 3e18; rayon = 5; coord_x = 400.; coord_y = 200.; vitesse_x = 0.; vitesse_y = 500.};;

solve A B 0.0001;;

(*Exemple 3 bis*)
(*Deux masses égales en interaction sur une orbite circulaire*)

let A = {masse = 3e18; rayon = 5; coord_x = 100.; coord_y = 200.; vitesse_x = 0.;  vitesse_y = -500. };;
let B = {masse = 3e18; rayon = 5; coord_x = 500.; coord_y = 200.; vitesse_x = 0.; vitesse_y = 500.};;

solve A B 0.0001;;

(*Exemple 4*)
(*Centre de masse en Mouvement Rectiligne Uniforme*)

let A = {masse = 1e17; rayon = 25; coord_x = 0. ; coord_y = 0.; vitesse_x = 25.; vitesse_y = 25.};;
let B = {masse = 300.; rayon = 3; coord_x = 150.; coord_y = 0.; vitesse_x = 0.; vitesse_y = 250.};;

solve A B 0.0001;;


(*Simulation Terre-Lune*)
(*
- Données :
- Masse de la Terre   : ~ 5,9736 x 10 ^ 24 Kg.
- Masse de la Lune    : ~ 7,3477 x 10 ^ 22 Kg.
- Distance Terre-Lune : ~ 384 400 Km.
- Vitesse orbitale de la Lune : 1,022 Km/s.
- Donnée de vérification de la simulation :
- Vitesse de libération	: 2,38 km/s
- Période de révolution	: 27,321582 Jours (Soit ~ 2 360 584 s).
*)

let Terre = {masse = 5.9736e24; rayon = 6000000; coord_x = 0.;     	 coord_y = 500000.; vitesse_x = 0.;  vitesse_y = 0.};;
let Lune  = {masse = 7.3477e22; rayon = 1737400; coord_x = 384.4e6;	 coord_y = 500000.; vitesse_x = 0.;  vitesse_y = 1.022e3};;

solve Terre Lune 1.;;

(***************************)
(*Première vitesse cosmique*)

(*Echec de lancement*)

let Terre = {masse = 5.9736e24; rayon = 6378000; coord_x = 0.;     	 coord_y = 500000.; vitesse_x = 0.;  vitesse_y = 0.};;
let Sat  = {masse = 1e3; rayon = 1; coord_x = 6378100.;	 coord_y = 500000.; vitesse_x = 0.;  vitesse_y = 7.900e3};;

solve Terre Sat 0.001;;

(*Mise en orbite correct*)
let Terre = {masse = 5.9736e24; rayon = 6378000; coord_x = 0.;     	 coord_y = 500000.; vitesse_x = 0.;  vitesse_y = 0.};;
let Sat  = {masse = 1e3; rayon = 1; coord_x = 6378100.;	 coord_y = 500000.; vitesse_x = 0.;  vitesse_y = 7.950e3};;

solve Terre Sat 0.001;;

(*Deuxième vitesse cosmique*)

(*Echec de libération*)

let Terre = {masse = 5.9736e24; rayon = 6378000; coord_x = 0.;     	 coord_y = 500000.; vitesse_x = 0.;  vitesse_y = 0.};;
let Sat  = {masse = 1e3; rayon = 1; coord_x = 6378100.;	 coord_y = 500000.; vitesse_x = 0.;  vitesse_y = 11.100e3};;

solve Terre Sat 0.001;;

(*Le satellite échappe à l'attraction de la Terre*)
let Terre = {masse = 5.9736e24; rayon = 6378000; coord_x = 0.;     	 coord_y = 500000.; vitesse_x = 0.;  vitesse_y = 0.};;
let Sat  = {masse = 1e6; rayon = 1; coord_x = 6378100.;	 coord_y = 500000.; vitesse_x = 0.;  vitesse_y = 11.300e3};;

solve Terre Sat 0.001;;




