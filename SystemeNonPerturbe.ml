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
#open "graphics";;
#open "float";;
#open "format";;

(*Opérations sur les entiers*)

	let prefix ++ a b = int_of_float(float_of_int(a) + float_of_int(b)) ;;
	let prefix -- a b = int_of_float(float_of_int(a) - float_of_int(b)) ;;
	let prefix ** a b = int_of_float(float_of_int(a) * float_of_int(b)) ;;
	let prefix ^^ a b = int_of_float( power (float_of_int(a)) (float_of_int(b)));;
	let prefix // a b = int_of_float(float_of_int(a) / float_of_int(b)) ;;

(*Opération sur les flottants*)
	(* '**.' reste disponible.*)
	let prefix ^^^^ a b = power a b;;

(*Référencement d'un couple*)
	let prefix ~:= (a,b) (c,d) = a := c;	b := d;;

(*Conversions des types*)
	let int(a) = int_of_float a;;
	let float(a) = float_of_int a;;
	let string(a) = string_of_float a;;

(*Déclarations des types.*)
	type corps   = 
		{mutable masse 	  : float;
	 	mutable rayon	    : int;
	 	mutable coord_x   : float;
	 	mutable coord_y   : float;
	 	mutable vitesse_x : float;
	 	mutable vitesse_y : float;
		};;

	type options = 
		{mutable dt 	    : float;
	 	mutable zoom 	    : float;
	 	mutable origine_x : int ;
	 	mutable origine_y : int ;
		mutable scale_F   : float;
		mutable scale_v   : float;
		mutable dispTime 	: bool;
		mutable dispState : bool;
		mutable dispTraj  : bool;
	 	mutable dispM     : bool;
		mutable dispG     : bool;
		mutable dispCorps : bool;
		mutable blinker   : bool;
		mutable hypermode : bool;
		mutable lineto    : bool;
		mutable reset     : bool;
		mutable elag      : bool;
	};;

	type state  = 
		{mutable corps1  : corps;
	 	mutable corps2   : corps;
	 	mutable partFict : corps;
	 	mutable centreG  : corps;
	 	mutable F 		   : float;
	};;

(*Déclaration d'un printer pour le type Corps*)

	let pp_print_corps state c =
		pp_print_string state ("Masse      : "^(string c.masse));
		print_string ("\n Rayon      : "^(string_of_int c.rayon));
		print_string ("\n Coordonées : ("^(string c.coord_x)^", "^(string c.coord_y)^")");
		print_string ("\n Vitesse    : ("^(string c.vitesse_x)^", "^(string c.vitesse_y)^")");
	;;

	let print_corps = pp_print_corps std_formatter;;

	install_printer "print_corps";;


(*Couleurs*)
	let marron  = (rgb 135 085 065);;
	let rouge   = (rgb 200 000 000);;
	let bleu    = (rgb 000 000 200);;
	let vert    = (rgb 000 200 000);;
	let c_oie   = (rgb 219 219 000);;

(*Reset key_pressed (Type unit)*)
	let readkey() = let c = ref `a` in c:= read_key();;

(*Pause jusqu'à ce qu'une touche soit pressée*)
	let wait() = while not(key_pressed()) do done; readkey() ;;

(*Pause de x ms*)
	let pause x = sound 20000 x;;

(*Exceptions*)
	exception collision;;

(*Auxiliaire - Utilisé dans le test pour les trajectoires.*)
let gt1 a = if (a < 1.) then 1 else int a;;

(*Affichage de l'état de la simulation*)
	let vectString_of_state state options =
	[|
		"Vitesse de simulation : "^(string options.dt);
		"Zoom : x"^(string options.zoom);
		"Echelle Vitesse : 1/"^(string (options.scale_v))^" ème";
		"Echelle Force :   1/"^(string (options.scale_F))^" ème";
		"";
		"Corps 1 :";
		"Xa   : "^(string state.corps1.coord_x);
		"Ya   : "^(string state.corps1.coord_y);
		"Vxa  : "^(string state.corps1.vitesse_x)^" m/s";
		"Vya  : "^(string state.corps1.vitesse_y)^" m/s";
		"|Va| : "^(string (sqrt((state.corps1.vitesse_x^^^^2.) + (state.corps1.vitesse_y^^^^2.))))^" m/s";
		"";
		"Corps 2 :";
		"Xb   : "^(string state.corps2.coord_x);
		"Yb   : "^(string state.corps2.coord_y);
		"Vxb  : "^(string state.corps2.vitesse_x)^" m/s";
		"Vyb  : "^(string state.corps2.vitesse_y)^" m/s";
		"|Vb| : "^(string (sqrt((state.corps2.vitesse_x^^^^2.) + (state.corps2.vitesse_y^^^^2.))))^" m/s";
		"";
		"Force d'interaction (De 1 sur 2) :";
		"F    :"^(string (state.F))^" N";
		"";
		"Centre de Masse G";
		"Xg   : "^(string state.centreG.coord_x);
		"Yg   : "^(string state.centreG.coord_y);
		"Vxg  : "^(string state.centreG.vitesse_x)^" m/s";
		"Vyg  : "^(string state.centreG.vitesse_y)^" m/s";
		"|Vg| : "^(string (sqrt((state.centreG.vitesse_x^^^^2.) + (state.centreG.vitesse_y^^^^2.))))^" m/s";
		"";
		"Particule Fictive M";
		"Xm   : "^(string state.partFict.coord_x);
		"Ym   : "^(string state.partFict.coord_y);
		"Vxm  : "^(string state.partFict.vitesse_x)^" m/s";
		"Vym  : "^(string state.partFict.vitesse_y)^" m/s";
		"|Vm| : "^(string (sqrt((state.partFict.vitesse_x^^^^2.) + (state.partFict.vitesse_y^^^^2.))))^" m/s";

	|];;

(*Messages d'informations*)
	let infos =
		[|
		(*0*) [|"Origine décalée : ";" Y = y + 1"|];
		(*1*) [|"Origine décalée : ";" X = x - 1"|];
		(*2*) [|"Origine décalée : ";" Y = y - 1"|];
		(*3*) [|"Origine décalée : ";" X = x + 1"|];
		(*4*) [|"Zoom - : (divisé par 1.2)";""|];
		(*5*) [|"Zoom + : (multiplié par 1.2)";""|];
		(*6*) [|"Vitesse de simulation doublée :";"Attention les erreurs s'accumulent."|];
		(*7*) [|"Vitesse de simulation réduite :";"Attention les erreurs s'accumulent."|];
		(*8*) [|"Affichage de l'état de la simulation :";"Ralentit la simulation."|];
		(*9*) [|"Affichage de la particule fictive :";"Purement théorique."|];
		(*10*)[|"Affichage de centre de masse :";"Purement théorique."|];
		(*11*)[|"Affichage des trajectoires :";"Ralentit progressivement la simulation."|];
		(*12*)[|"Affichage du temps : ";"Unité : Seconde."|];
		(*13*)[|"Sens normal de simulation :";"Attention les erreurs s'accumulent."|];
		(*14*)[|"Sens inverse de simulation :";"Attention les erreurs s'accumulent."|];
		(*15*)[|"Mode Anti-Blinker : Ralentit";"Grandement la vitesse de simulation."|];
		(*16*)[|"Hypermode, désactive les options :";"Accélère la simulation."|];
		(*17*)[|"En Pause : ";"Appuyer sur une touche pour continuer"|];
		(*18*)[|"Affichage des trajectoires: ";"Mode continu"|];
		(*19*)[|"Trajectoires (listes): ";"Perte d'une information sur deux."|];
		(*20*)[|"Trajectoires (listes): ";"Remise à zero"|];
		|];;

(*Fonction d'affichage du temps*)
	let draw_time time = 
		(*Nettoyage de la zone d'écriture*)
		set_color white;
		fill_rect (size_x()-- 300) (size_y()-- 20) 300 20;
		(*Ecriture*)
		set_color black;
		moveto (size_x()-- 300) (size_y()-- 20);
		draw_string ("Temps  : "^(string_of_float time)^" s.");
	;;

(*Affichage de l'état de la simulation*)
let draw_state states = 
		let n = vect_length states -- 1 in
		(*Nettoyage de la zone d'écriture*)
			set_color white;
			fill_rect (size_x()-- 300) (40) 300 (size_y()-- 60);
		(*Ecriture*)
			set_color black;
			for i=0 to n do
				moveto (size_x()-- 300) (size_y() -- (15**i ++ 35)) ;
				draw_string states.(i);
			done;	
;; 

(*Fonction d'affichage des messages de notifications.*)
	let notification message =
		(*Nettoyage de la zone d'écriture*)
		set_color white;
		fill_rect (size_x()-- 300) 0 300 40;
		(*Ecriture*)
		set_color black;
		(*Ligne 1*)
		moveto (size_x()-- 300) 20 ;
		draw_string message.(0);
		(*Ligne 2*)
		moveto (size_x()-- 300) 0 ;
		draw_string message.(1);
	;;


(*Fonction d'interaction clavier.*)
	let waitif options state=
		if key_pressed() 
			then 
				begin
				match read_key() with
				(*Déplacement*)
				| `z`   -> clear_graph(); (notification infos.(0)); options.origine_y <- options.origine_y ++ 1;options;
				| `q`   -> clear_graph(); (notification infos.(1)); options.origine_x <- options.origine_x -- 1;options;
				| `s`   -> clear_graph(); (notification infos.(2)); options.origine_y <- options.origine_y -- 1;options;
				| `d`   -> clear_graph(); (notification infos.(3)); options.origine_x <- options.origine_x ++ 1;options;
				(*Zoom + et Zoom -*)
				| `-`   -> clear_graph(); (notification infos.(4)); options.zoom <- options.zoom / 1.2;options;
				| `+`   -> clear_graph(); (notification infos.(5)); options.zoom <- options.zoom * 1.2;options;
				(*Vitesse de simulation*)
				| `r`   -> (notification infos.(6)); options.dt <- options.dt * 2.; options;
				| `l`   -> (notification infos.(7)); options.dt <- options.dt / 2.; options;
				(*Information*)
				| `i`   -> clear_graph(); (notification infos.(8)); options.dispState <- not(options.dispState); options;
				(*Affichage de la Particule Fictive*)
				| `m`   -> clear_graph(); (notification infos.(9)); options.dispM <- not(options.dispM) ;options;
				(*Affichage du Centre de Masse G*)
				| `g`   -> clear_graph(); (notification infos.(10)); options.dispG <- not(options.dispG) ;options;
				(*Affichage des trajectoires*)
				| `a`   -> clear_graph(); (notification infos.(11)); options.dispTraj <- not(options.dispTraj); options;
				(*Mode "Lineto" sur les trajectoires*)
				| `c`   -> clear_graph(); (notification infos.(18)); options.lineto <- not(options.lineto); options;
				(*Elagage des listes*)
				| `e`   -> clear_graph(); (notification infos.(19)); options.elag  <- true; options;
				(*Reset listes*)
				| `x`   -> clear_graph(); (notification infos.(20)); options.reset <- true; options;
				(*Affichage du temps*)
				| `t`   -> clear_graph(); (notification infos.(12)); options.dispTime <- not(options.dispTime ); options;
				(*Sens de la simulation*)
				| `>`   -> (notification infos.(13)); options.dt <-  abs_float options.dt; options;
				| `<`   -> (notification infos.(14)); options.dt <- - abs_float options.dt; options;
				(*Anti-blinker - réduit le clignotement*)
				| `b`   -> (notification infos.(15)); options.blinker <-  not(options.blinker); options;
				(*Hypermode*)
				| `h`		-> clear_graph(); (notification infos.(16)); options.hypermode <- not(options.hypermode);	options;
				(*Exit*)
				| `\027`-> close_graph(); raise Exit; options;
				(*Pause*)
				|   _   -> (notification infos.(17)); draw_state (vectString_of_state state options); wait(); clear_graph(); options;
				end
			else options;;


(*Fonctions graphiques*)
	let effaceCorp c fx fy options=
		(*cercle*)
		set_color white;
		fill_circle (int (c.coord_x * options.zoom) ++ options.origine_x)
								(int (c.coord_y * options.zoom) ++ options.origine_y)
								(int (float c.rayon *  options.zoom));
		(*Vecteur Vitesse*)
		moveto (int (c.coord_x * options.zoom) ++ options.origine_x)
					 (int (c.coord_y * options.zoom) ++ options.origine_y);
		lineto (int ((c.vitesse_x / options.scale_v + c.coord_x) * options.zoom) ++ options.origine_x)
					 (int ((c.vitesse_y / options.scale_v + c.coord_y) * options.zoom) ++ options.origine_y);
		(*Vecteur Force*)
		moveto (int (c.coord_x  * options.zoom) ++ options.origine_x) 
					 (int (c.coord_y  * options.zoom) ++ options.origine_y);
		lineto (int ((fx / options.scale_F + c.coord_x) * options.zoom) ++ options.origine_x)
					 (int ((fy / options.scale_F + c.coord_y) * options.zoom) ++ options.origine_y);
	;;

	let drawCorp c color fx fy options=
		set_color color;
		(*cercle*)
		fill_circle (int (c.coord_x * options.zoom) ++ options.origine_x)
							  (int (c.coord_y * options.zoom) ++ options.origine_y)
								(int (float c.rayon * options.zoom));
		(*Vecteur Vitesse*)
		set_color red;
		moveto (int (c.coord_x * options.zoom) ++ options.origine_x)
					 (int (c.coord_y * options.zoom) ++ options.origine_y);
		lineto (int ((c.vitesse_x / options.scale_v + c.coord_x) * options.zoom) ++ options.origine_x)
					 (int ((c.vitesse_y / options.scale_v + c.coord_y) * options.zoom) ++ options.origine_y);
		(*Vecteur Force*)
	  set_color c_oie;
		moveto (int (c.coord_x * options.zoom) ++ options.origine_x)
					 (int (c.coord_y * options.zoom) ++ options.origine_y);
		lineto (int ((fx / options.scale_F + c.coord_x) * options.zoom) ++ options.origine_x)
					 (int ((fy / options.scale_F + c.coord_y) * options.zoom) ++ options.origine_y);
	;;

(*Fonction plot couple de flottant*)
	let plotc (a,b) (x,y) options =
	if (options.lineto) 
	then
		begin
		moveto (int (a * options.zoom) ++ options.origine_x)
				   (int (b * options.zoom) ++ options.origine_y);
		lineto (int (x * options.zoom) ++ options.origine_x)
				   (int (y * options.zoom) ++ options.origine_y)
		end
	else
		begin
		plot (int (x * options.zoom) ++ options.origine_x)
				 (int (y * options.zoom) ++ options.origine_y)
		end;;

(*Fonction plot liste*)
	let rec plotlist l color options=
	set_color color;
	let rec aux l options = 
		match l with
			|[]    -> ()
			|[a]	 -> ()
			| a::b::q -> (plotc a b options ; aux q options)
	in aux l options;;


(*Elagage d'une liste (renvoie un élément sur deux)*)
let rec elag_list l =
	match l with
	|[]   -> []
	|[a]  -> [a]
	|a::b::q -> a::(elag_list q);;

let op_list l options =
	try
		match (options.reset, options.elag) with
		|true , _ -> [];
		|_ , true -> (elag_list l);
		|_ ,_     -> l;
	with 
	|Out_of_memory -> [];;


(*Diminuer l'ordre de grandeur d'un nombre*)
	let low_number nn max =
		let n = ref nn and exp = ref 0 and coeff = ref 1. in
			while !n > 1e3 do
				n := !n / 10.;
				incr exp;
			done;
			while !n > max do
				n := !n / 1.5;
				coeff := !coeff * 1.5;
			done;
		(!coeff * (10.^^^^(float !exp)));;

let Runge_Kutta_4_ordre_2 f''x x x' (y:float) options=
	let K1 = ref 0. and K2 = ref 0. and K3 = ref 0. and K4 = ref 0. 
	and J1 = ref 0. and J2 = ref 0. and J3 = ref 0. and J4 = ref 0. in
(*Calcule de x*)
			J1 := x'* options.dt;
			K1 := f''x(y, x, x')* options.dt;
			J2 := (x' + !K1 / 2.)* options.dt;
			K2 := f''x(y,( x + !J1 / 2.), (x' + !K1 / 2.))* options.dt;
			J3 := (x' + !K2 / 2.)* options.dt;
			K3 := f''x(y, (x + !J2 / 2.), (x' + !K2 / 2.))* options.dt;
			J4 := (x' + !K3)* options.dt;
			K4 := f''x(y, (x + !J3), (x' + !K3))* options.dt;
	(x  + (!J1 + 2. * !J2 + 2. * !J3 + !J4)/ 6.,
	 x' + (!K1 + 2. * !K2 + 2. * !K3 + !K4)/ 6.);;

let bang()= 
	let boom = [|"Bang !"; "Boum !"|] in
	set_font "Times";
		for i = 255 downto 0 do
			set_text_size (15 ++ 7 ** (i mod 2) );
			set_color (rgb 255 (i) 0);
			moveto (random__int (size_x() -- 300)) (random__int (size_y()));
			draw_string boom.(i mod 2);
			sound 0 25;
		done
	;;

(*Fonction solve() - Cas Général : Mode Continu*)
let solve corpsA corpsB dtt = 
	open_graph "";
	(*Trajectoire int*int list.*)
	let la = ref [] and lb = ref [] and lm = ref [] and lg = ref [] in
	(*Compteur*)
	let count = ref 0 in
(*Constante de Gravitation Universelle*)
	let Gr = 6.67e-11 in
(*Centre de Masse G*)
	let xG =  (corpsA.masse * corpsA.coord_x + corpsB.masse * corpsB.coord_x) / (corpsA.masse + corpsB.masse) in
	let yG =  (corpsA.masse * corpsA.coord_y + corpsB.masse * corpsB.coord_y) / (corpsA.masse + corpsB.masse) in
(*Application du TCM*)
	let VxG = (corpsA.masse * corpsA.vitesse_x + corpsB.masse * corpsB.vitesse_x)/(corpsA.masse + corpsB.masse) 
	and VyG = (corpsA.masse * corpsA.vitesse_y + corpsB.masse * corpsB.vitesse_y)/(corpsA.masse + corpsB.masse) in
	let G = {masse = 0.; rayon = 5; coord_x = xG ; coord_y = yG; vitesse_x = VxG; vitesse_y = VyG} in
(*Particule Fictive M*)
	let Mu = corpsA.masse * corpsB.masse / (corpsA.masse + corpsB.masse) in
	let Vx = corpsB.vitesse_x - corpsA.vitesse_x + G.vitesse_x in
	let Vy = corpsB.vitesse_y - corpsA.vitesse_y + G.vitesse_y in
	let xM = corpsB.coord_x - corpsA.coord_x + G.coord_x in
	let yM = corpsB.coord_y - corpsA.coord_y + G.coord_y in
	let M = {masse = corpsA.masse*corpsB.masse; rayon = 5; coord_x = xM; coord_y = yM; vitesse_x = Vx; vitesse_y = Vy} in
(*Equation différentielle en x'' et y'' de M*)
(*En x*)
	let f''x(y,x,v) =
		-(Gr * (M.masse)*(x-G.coord_x)) / ((sqrt(((x-G.coord_x)*(x-G.coord_x) + (y-G.coord_y)*(y-G.coord_y)))^^^^3.) * Mu ) in
(*En y*)
	let f''y(x,y,v) =
		-(Gr * (M.masse)*(y-G.coord_y)) / ((sqrt(((x-G.coord_x)*(x-G.coord_x) + (y-G.coord_y)*(y-G.coord_y)))^^^^3.) * Mu ) in
(*Déduction des coordonnées corpsA et corpsB par homothétie*)
(*Corps A*)
	let fxcorpsA = function x -> - (corpsB.masse * (x-G.coord_x)) / (corpsA.masse + corpsB.masse) in
	let fycorpsA = function y -> - (corpsB.masse * (y-G.coord_y)) / (corpsA.masse + corpsB.masse) in
(*Corps B*)
	let fxcorpsB = function x -> (corpsA.masse * (x - G.coord_x)) / (corpsA.masse + corpsB.masse) in
	let fycorpsB = function y -> (corpsA.masse * (y - G.coord_y)) / (corpsA.masse + corpsB.masse) in 
(*Déduction des vitesses corpsA et corpsB par homothétie*)
(*Corps A*)
	let fvxcorpsA = function x -> - (corpsB.masse * (x-G.vitesse_x)) / (corpsA.masse + corpsB.masse) in
	let fvycorpsA = function y -> - (corpsB.masse * (y-G.vitesse_y)) / (corpsA.masse + corpsB.masse) in
(*Corps B*)
	let fvxcorpsB = function x -> (corpsA.masse * (x - G.vitesse_x)) / (corpsA.masse + corpsB.masse) in
	let fvycorpsB = function y -> (corpsA.masse * (y - G.vitesse_y)) / (corpsA.masse + corpsB.masse) in 
(*Déclaration de "options" et "state"*)
	let options = ref {dt = dtt ; zoom = 1.; origine_x = 0; origine_y = 0; scale_F = 0.; scale_v = 0.;
	dispTime = false; dispState = false; dispTraj = false ; dispM = false; dispG = false; blinker = false ; 
	dispCorps = true; hypermode = false; lineto = false; elag = false; reset = false} in
	let state = { corps1 = corpsA; corps2 = corpsB; partFict = M; centreG = G; F = 0. } in
(*Définitions obsolètes*)
	let yo = M.coord_y and vyo = M.vitesse_y
	and xo = M.coord_x and vxo = M.vitesse_x in
(*End_def*)
	let fx = ref 0. and fy = ref 0. in
(*Calcul de F 1 -> 2 à t0*)
	fx := (abs_float (f''x(yo,xo,vxo)) * Mu); 
	fy := (abs_float (f''y(xo,yo,vyo)) * Mu); 
(*Evaluation d'une échelle appropriée*)
	(*Force*)
 	!options.scale_F <- (low_number (max !fx !fy) ((sqrt ( ((corpsB.coord_x - corpsA.coord_x)^^^^2.) + ((corpsB.coord_y - corpsA.coord_y)^^^^2.) ))/2.));
	(*Vitesse*)
	!options.scale_v <- (low_number (max vxo vyo) (300.));
(*Runge Kutta 4*)
	let y = ref yo and y' = ref vyo and x = ref xo  and x' = ref vxo  and t = ref 0. in
	let yy = ref yo and yy' = ref vyo and xx = ref xo  and xx' = ref vxo  in
	(*Pause avant de commencer*)
	wait();
	try
		while true do
			(*Calcule de x*)
			(xx, xx') ~:= (Runge_Kutta_4_ordre_2 f''x !x !x' !y !options);
			(*Calcule de y*)
			(yy, yy') ~:= (Runge_Kutta_4_ordre_2 f''y !y !y' !x !options);
			(*Vitesse et coordonnée de M*)
			(x, x')   ~:= (!xx, !xx');
			(y, y')   ~:= (!yy, !yy');
			(*Traitement des données*)
			(*Effaçage des corps*)
				if not(!options.hypermode)
					then 
					begin
						(*"Réels"*)
						effaceCorp corpsB (!fx)   (!fy) !options;
						effaceCorp corpsA (- !fx) (- !fy) !options;
						(*"Fictifs"*)
						if !options.dispM then effaceCorp M (0.) (0.) !options;
						if !options.dispG then effaceCorp G (0.) (0.) !options;
					end;
				(*Mise à jour des Corps (Fictifs et simulés)*)
				(*Centre de Masse G - coordonnées*)
					G.coord_x <- (VxG * !t + xG);
					G.coord_y <- (VyG * !t + yG);
				(*Corps A - coordonnées*)
					corpsA.coord_x <- (G.coord_x + fxcorpsA !x);
					corpsA.coord_y <- (G.coord_y + fycorpsA !y);
				(*Corps B - coordonnées*)
					corpsB.coord_x <- (G.coord_x + fxcorpsB !x);
					corpsB.coord_y <- (G.coord_y + fycorpsB !y);
				(*Particule fictive - coordonnées*)
					M.coord_x <- !x;
					M.coord_y <- !y;
					if not(!options.hypermode)
					then 
					begin			
						(*calcul de F 1/2*)
						fx := (f''x(!y,!x,!x') * Mu); 
						fy := (f''y(!x,!y,!y') * Mu); 
						(*Particule Fictive M Vitesses*)
						M.vitesse_x <- !x';
						M.vitesse_y <- !y';
						(*Corps A - Vitesses*)
						corpsA.vitesse_x <- (G.vitesse_x + fvxcorpsA !x');
						corpsA.vitesse_y <- (G.vitesse_y + fvycorpsA !y');
						(*Corps B - Vitesses *)
						corpsB.vitesse_x <- (G.vitesse_x + fvxcorpsB !x');
						corpsB.vitesse_y <- (G.vitesse_y + fvycorpsB !y');
						(*Actualisation de l'état de la simulation*)
						state.corps1   <- corpsA;
						state.corps2   <- corpsB;
						state.partFict <- M;
						state.centreG  <- G;
						state.F        <- (sqrt((!fx ^^^^ 2.)+(!fy ^^^^2.)));
						(*Affichage des corps*)
						(*"Réels"*)
						drawCorp corpsB bleu (!fx) (!fy) !options;
						drawCorp corpsA marron (- !fx)  (- !fy) !options;
						(*"Fictifs"*)
						if !options.dispM then drawCorp M vert (0.) (0.) !options;
						if !options.dispG then drawCorp G rouge (0.) (0.) !options;
					end;
			(*Enregistrement des trajectoires tous les x calculs*)
				if (!count mod gt1(100. * dtt /(!options.dt)) == 0) then
					begin
					(*Enregistrement*)
					la := (op_list ((corpsA.coord_x,corpsA.coord_y) :: !la) !options);
					lb := (op_list ((corpsB.coord_x,corpsB.coord_y) :: !lb) !options);
					lm := (op_list ((M.coord_x,M.coord_y) :: !lm) !options);
					lg := (op_list ((G.coord_x,G.coord_y) :: !lg) !options);
					!options.reset <- false; !options.elag <- false; 
					(*Affichage*)
					if (!options.dispTraj && not(!options.hypermode)) then
						begin
						plotlist !la marron !options;
						plotlist !lb bleu !options;
						if !options.dispM then (plotlist !lm vert !options;);
						if !options.dispG then (plotlist !lg rouge !options;);
						end
				end
			else ();
			incr count;
			(*Vérifications des collisions*)
			if ((sqrt (((corpsB.coord_x - corpsA.coord_x)^^^^2.) + ((corpsB.coord_y - corpsA.coord_y)^^^^2.)))  < float (corpsA.rayon ++ corpsB.rayon))
				then raise collision;
			(*Temps*)
				t  := !t + !options.dt;
			(*Affichage du temps*)
				if (!options.dispTime && not(!options.hypermode)) then draw_time !t;
				if (!options.dispState&& not(!options.hypermode)) then draw_state (vectString_of_state state !options);
			(*Interaction utilisateur*)
				options := waitif !options state;
			(*Anti-blink*)
			if (!options.blinker && not(!options.hypermode)) then pause 1;
	done;
		with 
	|Exit -> print_string "Fin de la simulation."
	|collision -> bang(); print_string "BANG";;


(*
- Version finale - copie de Système non perturbé 19.ml.
*)



