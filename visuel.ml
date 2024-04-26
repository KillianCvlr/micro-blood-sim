open Graphics;;
		
let show_point mi rayon color =(*
	Affiche le point mi de la couleur color avec le bon rayon *)
	(set_color color;
	fill_circle (coord_x mi) (coord_y mi) rayon);;

let show_point_str mi color = (*
	affiche le nombre du point mi de la couleur color *)
	(set_color color ;
	moveto (coord_x mi) (coord_y mi); 
	draw_string (string_of_int mi));;

let show_points tab rayon color montrer_str color_str size_str = 
	(* affiches les points de la couleur et au rayon demandé contenus dans un tableau tab
	avec tab = int*int array
	affiche le numero de mi ssi montrer_str *)
	(let n = Array.length tab in 
	for mi = 0 to n -1 do
		show_point mi rayon color;
		if montrer_str
		then (set_text_size size_str ;show_point_str mi color_str )
	done );;

let show_route mi mk color = (*
	 Affiche la route ie le segment allant de mi a mk de la couleur color *)
	(set_color color ;
	moveto (coord_x mi) (coord_y mi);
	lineto (coord_x mk) (coord_y mk));;
	
let rec show_trajet (trajet : int list) color = 
	(* Affiche les routes pour mi et mj consecutifs dans trajet
	Permet d'afficher un ensemble de segments donc un trajet *)
	match trajet with 
	|[] -> ()
	|m2::[] -> ()
	|m1::m2::reste_trajet -> 
		show_route m1 m2 color;
		show_trajet (m2::reste_trajet) color;;

let show_routes color  = (*
	affiche en color toutes les routes contenues dans la matrice route *)
	let n = Array.length route in 
	(set_color color;
	for mi = 0 to n-1 do	
		for mk = 0 to n-1 do
			if route.(mi).(mk) < infinity
			then
			   show_route mi mk color
		done
	done);;
	
(*let show_ponts color width  = (*
	Affiches les routes contenues dans pont de la couleur color et la largeur width *)
	let n = Array.length route in 
	(set_color color;
	set_line_width width;
	for mi = 0 to n-1 do	
		for mk = 0 to n-1 do
			if pont.(mi).(mk) < infinity
			then
			   show_route mi mk color
		done
	done);;*)
	
(*let show_connexe mi color = (*
	Affiche les points accessibles depuis mk de la couleur color et de rayon 2*)
	for mj = 0 to !nb_points -1 do
		if tab_trajets.(mi).(mj) < infinity 
		then show_point mj 3 color
	done;;*)
set_text_size 30;;
if affichage_graphe then
	open_graph "1500x1000";
	set_line_width 3 ;
	set_text_size 80 ;
	show_routes red ;
	show_points tab_points 5 red affichage_str black 80;;


let draw_line pi pj ~width ~color = 
	let xdeb = current_x () and
	ydeb = current_y ()in
	(set_line_width width;
	set_color color;
	moveto (pi.x) (pi.y );
	lineto (pj.x) (pj.y );
	moveto (xdeb) (ydeb ) );;
	
	(* fonction g_**** permettent de tracer sans bouger le point initial*)

let g_horizontal ~origine ~longueur ~width ~color =
(*trace une droite horizontale de longueur fixé depuis l'origine et revient dessus*)
	moveto (origine.x) (origine.y);
	set_line_width width;
	set_color color;
	lineto (origine.x + longueur) origine.y;
    moveto (origine.x) (origine.y);;
	
let g_vertical ~origine ~longueur ~width ~color =
(*trace une droite verticale de longueur fixé depuis l'origine et revient dessus*)
	moveto (origine.x) (origine.y);
	set_line_width width;
	set_color color;
	lineto (origine.x) (longueur + origine.y);
	moveto (origine.x) (origine.y)	;;


	
(*let g_abscisse ~origine ~tabx ~gap_aff ~gap_s ~size  =
	for k = 0 to Array.length tabx do
		if k mod gap_aff = 0 then
			(draw_line {x = (origine.x + k); y =((origine.y) + size +2)} {x=(origine.x + k); y =((origine.y) - (size + 2))} ~width:2 ~color:black;
			let contenu_tab = string_of_int tabx.(k) in
			let a = coord_x (text_size contenu_tab) and b = coord_y (text_size contenu_tab) in 
			(moveto (origine.x + k - a / 2) ((origine.y) - (size + 2) - b /2);
			draw_string contenu_tab))
		else if k mod gap_s = 0 then
			draw_line {x =tabx.(k); y=((origine.y) + size )} {x =tabx.(k); y=((origine.y) - size )} ~width:1 ~color:black
	done;
	moveto (origine.x) (origine.y);;



let g_graphe ~origine ~fin ~tabx ~taby =

	(*On trace les abscisses et ordonnes*)
	moveto (origine.x) (origine.y) ;
	let width = 3 and color = black in
	(g_horizontal ~origine ~longueur:((fin.x) - (origine.x)) ~width ~color;
	g_vertical ~origine ~longueur:((coord_y fin) - (origine.y)) ~width ~color);
	
	(*on donne l'échelle*)
	g_abscisse ~origine ~tabx ~gap_aff:100 ~gap_s:10 ~size:3 ;;*)

	
let color_bornee_float valeur re_min re_max = 
	let intervalle_passage = re_max -. re_min in
	let prop = int_of_float ((1020. /. intervalle_passage) *. (( valeur) -. re_min) ) in
	let r = ref 0 and g = ref 0 and b = ref 0 in
	
	(if prop >= 0 && prop <= 255
		then 
		( r:= 0;
		g := prop;
		b := 255  )
				
	else if prop > 255 && prop <= 510
		then let prop' = prop - 255 in
		( r := 0;
		g := 255;
		b := 255 - prop' )
		
	else if prop > 510 && prop < 765 
		then let prop' = prop - 510 in
		( r := prop' ;
		g := 255 ;
		b := 0 )
		
	else
		let prop' = prop - 765 in
		( r := 255 ;
		g := 255 - prop';
		b := 0)
	);
	rgb !r !g !b ;;
	
let color_bornee_int valeur min max = 
	color_bornee_float (float_of_int valeur) (float_of_int min) (float_of_int max);;
	
let color_of_passage mi mj passage_min passage_max = 
	if tab_passage.(mi).(mj) < 0 then rgb 0 0 0
	else
		let intervalle_passage = log2 (float_of_int (passage_max - passage_min)) in
		let prop = int_of_float ((1020. /. intervalle_passage) *. (log2 (float_of_int (tab_passage.(mi).(mj) - passage_min) )) ) in
		let r = ref 0 and g = ref 0 and b = ref 0 in
		
		(if prop >= 0 && prop <= 255
			then 
			( r:= 0;
			g := prop;
			b := 255  )
					
		else if prop > 255 && prop <= 510
			then let prop' = prop - 255 in
			( r := 0;
			g := 255;
			b := 255 - prop' )
			
		else if prop > 510 && prop < 765 
			then let prop' = prop - 510 in
			( r := prop' ;
			g := 255 ;
			b := 0 )
			
		else
			let prop' = prop - 765 in
			( r := 255 ;
			g := 255 - prop';
			b := 0)
		);
	rgb !r !g !b ;;
	
let show_glob gi r =
	let mi = tab_glob.(gi).debut and mj = tab_glob.(gi).arrivee and dist_parc = tab_glob.(gi).dist in
	let prop = dist_parc /. l in
	let x = coord_x mi + (int_of_float (prop *. (float_of_int ( (coord_x mj) - (coord_x mi) ) ) ) ) in
	let y = coord_y mi + (int_of_float (prop *. (float_of_int ( (coord_y mj) - (coord_y mi) ) ) ) ) in
	fill_circle x y r ;
	moveto x y;;

let show_globs color rad =
	set_color color ;
	for gi = 0 to nb_glob -1 do
		show_glob gi rad
	done;;
	
let show_Re size =	
	let re_min = min_selon_voisin tab_Re and re_max = max_selon_voisin tab_Re in
		(set_line_width size ;
		for mi = 0 to (!nb_points -2) do
			for i_voisin = 0 to (Array.length tab_depart.(mi) - 1) do
				let mj = tab_depart.(mi).(i_voisin) in
				show_route mi mj (color_bornee_float (tab_Re.(mi).(mj)) re_min re_max) 
			done
		done );;

let show_press () = 
	let p_min = 0. and p_max = tab_p.(indice_max tab_p) in
		for mi = 0 to (!nb_points -1) do
				show_point mi 45 (color_bornee_float (tab_p.(mi)) p_min p_max)
		done ;;

let show_qij size =	
	let qij_min = min_selon_voisin mat_qij and qij_max = max_selon_voisin mat_qij in
		(set_line_width size ;
		for mi = 0 to (!nb_points -2) do
			for i_voisin = 0 to (Array.length tab_depart.(mi) - 1) do
				let mj = tab_depart.(mi).(i_voisin) in
				show_route mi mj (color_bornee_float (mat_qij.(mi).(mj)) qij_min qij_max)
			done
		done );;	

let show_passage () =
	let passage_min = min_selon_voisin tab_passage and passage_max = max_selon_voisin tab_passage in
	for mi = 0 to (!nb_points -2) do
		for i_voisin = 0 to (Array.length tab_depart.(mi) - 1) do
			let mj = tab_depart.(mi).(i_voisin) in
			show_route mi mj (color_of_passage mi mj passage_min passage_max)
		done
	done ;;
	
let show_qij_neg ()= 
	set_line_width 3;
	for mi = 0 to !nb_points -1 do
		for j = 0 to (Array.length tab_depart.(mi)) -1 do
			let mj = tab_depart.(mi).(j) in
			if mat_qij.(mi).(mj) < 0. then show_route mi mj red
		done
	done;;
	
let actualiser () =
	clear_graph () ;
	show_points tab_points 5 black affichage_str black 3;
	show_qij 5 ;
	show_globs magenta 3 ;;

let nb_etape n = 
	for i = 0 to n-1 do
		prochaine_etape ();
		actualiser ()
	done;;