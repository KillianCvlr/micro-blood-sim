
(* TO DO: 
modele simple
modele plus realiste
prise en compte du fluide
ne pas oublier de sucer avec beaucoup de salive
se la prendre dans le cul
modelisation couleur*)

(*Mode de configuration :
	-simple : carré avec n*n points
	-realiste :	nuages de points reliés (sens de bas vers haut et gauche à droite *)
	
exception Foo of string;;	
type point = {x : int ; y : int };;
type glob = {debut : int ; arrivee : int ; mutable dist : float ; mutable vel : float};;

let configuration = "simple";;
let calcul_trajet = true;;
let affichage_graphe = true;;
let affichage_str = false;;
let generation = true ;;
let param_globule = true ;;
let considerer_Re = true ;;
let bifurc_simple = false ;;
let temps_passe = ref 0. ;;

let puiss_10 x = Float.pow 10. (x);;
Random.self_init () ;;
(*PARTIE 1 : INITIALISATION *)
 
let nb_glob = 550 ;;
let nb_points = ref 124;; 
let origine = {x = 50 ; y = 50} and fin = {x = 850 ; y = 850};;
let nb_points_cote = ref 0;;

let d = 5. *. (puiss_10 (-6.) );;
let l = 50. *. (puiss_10 (-6.) );;
let l_chap = 8. *. (puiss_10 (-6.) );;
let beta = 2.7;;
let theta = 1.;;
let roh = 1050.;;
let mu = 1.5 *. (puiss_10 (-3.) );;
let q0 = (( roh *. Float.pi *. (Float.pow d 2.) ) /. (4.) ) *. (puiss_10 (-3.) ) ;;
let r = (128. *. mu )/. ( Float.pi *. (Float.pow d 4.) *. roh) ;; 
let r_maj = r *. l ;;


if configuration = "simple" then 
	(nb_points_cote := (int_of_float ( sqrt (float_of_int !nb_points))); nb_points := (!nb_points_cote * !nb_points_cote));;
 
let route_coupee = ref (Array.make 0 (0,0)) ;;
(*chaque point mi est distingué par ses coordonnées xi,yi positives 
tab_points : (int * int) array 
tab_points.(mi) renvoie le couple xi,yi*)
let tab_points = Array.make !nb_points (0,0);;

(*route : float array array
matrice !nb_points * !nb_points tel que route.(mi).(mj) renvoie
la distance de la route entre mi et mj si elle existe et infinty sinon *)
let route = Array.make_matrix !nb_points !nb_points (infinity);;

let tab_passage = Array.make_matrix !nb_points !nb_points (0);;
let tab_depart = Array.make !nb_points (Array.make 0 0);;
let tab_arrivee = Array.make !nb_points (Array.make 0 0) ;;

let proba_depart = Array.make !nb_points (Array.make 0 0.);;

let tab_glob = Array.make nb_glob {debut = 0; arrivee = 0; dist = 0.; vel  = 0.} ;;
let tab_Re  = Array.make_matrix !nb_points !nb_points (0.);;
let tab_N = Array.make_matrix !nb_points !nb_points (0);;
let mat_qij = Array.make_matrix !nb_points !nb_points (0.);;
let tab_p = Array.make !nb_points 0. ;;
let mat_coeff = Array.make_matrix !nb_points !nb_points (0.) ;;
let tab_temps_restant = Array.make nb_glob infinity ;;

let tab_q = Array.make !nb_points 0. ;;
tab_q.(0) <- q0 ; tab_q.(!nb_points -1) <- -1. *. q0;;

(*debugage *)
let nb_dist_nulle () = 
	let somme = ref 0 in 
	for gi = 0 to nb_glob -1 do
		if tab_glob.(gi).dist < 0. then somme := !somme + 1
	done ; 
	print_string "nb de distance nulles " ;
	print_string ( string_of_int !somme );
	print_string "\n";;

let est_triang_inf mat = 
	let marqueur = ref true in
	for i = 1 to (Array.length mat -1) do
		let j = ref 0 in
		while !j < i do
			(if mat.(i).(!j) > 0.
				then(marqueur := false ;print_string ( "i et j non diag " ^( string_of_int i ) ^ "   "^ ( string_of_int !j ) ^ "\n" ) )
			; incr j)
		done
	done;
	!marqueur ;;

let est_triang_sup mat = 
	let marqueur = ref true in
	for i = 1 to (Array.length mat -1) do
		let j = ref (i+1) in
		while !j < Array.length mat.(i) do
			(if mat.(i).(!j) > 0.
				then(marqueur := false ;print_string ( "i et j non diag " ^( string_of_int i ) ^ "   "^ ( string_of_int !j ) ^ "\n" ) )
			; incr j)
		done
	done;
	!marqueur ;;

(*Debut programme *)

let est_infini a = (a = infinity);;

let coord_x mi = match tab_points.(mi) with
		|xi,yi -> xi ;;
		
let coord_y mi = match tab_points.(mi) with
		|xi,yi -> yi;;

let couple_egale (x,y) = if x = y then true else false;;


let nb_infini tab = (*
  bool array -> int : renvoie le nb d'infini dans un bool array *)
  let n = Array.length tab and
  occ = ref 0 in 
  (for i = 0 to n-1 do
     if tab.(i) =infinity
     then incr occ
   done;
   !occ);;
 
let indice_min tab = 
	(*donne l'indice ou le min est atteint dans un tableau *)
	let min = (ref tab.(0)) and i_min = (ref (0)) in
	for k = 0 to (Array.length tab ) -1 do
		if tab.(k) < !min then
			(i_min := k;
			min := tab.(k) )
		else ()
	done ;
	!i_min;;

let indice_max tab = 
	(*donne l'indice ou le max est atteint dans un tableau *)
	let min = (ref tab.(0)) and i_min = (ref (0)) in
	for k = 0 to (Array.length tab ) -1 do
		if tab.(k) > !min then
			(i_min := k;
			min := tab.(k) )
		else ()
	done ;
	!i_min;;
	
let distance_euclidienne mi mj = (*
  point -> point -> float : distance entre deux points *)
  match tab_points.(mi),tab_points.(mj) with
  |(xi,yi),(xj,yj) -> sqrt (float_of_int ((xi-xj)*(xi-xj) + (yi-yj)*(yi-yj))) ;; 

let ajouter_route mi mj = (*
	int -> int -> unit : cree une route de mi a mj de distance l *)
	route.(mi).(mj) <- l;
	tab_depart.(mi) <- Array.append tab_depart.(mi) [|mj|];
	tab_arrivee.(mj) <- Array.append tab_arrivee.(mj) [|mi|];
	proba_depart.(mi) <- Array.append proba_depart.(mi) [|0.|];;  

let creation_carte_simpliste () = (*
  int -> int -> unit : modifie les tab_points, route, tab_distance pour donner un graphe 
  dont on ne connait pas la connexité*)
                                    
	(* ETAPE 1 : CREATION DE LA NUEE ET INITIALISATION DES DISTANCES *)
	let longueur_x = (fin.x) - (origine.x) and
	longueur_y = (fin.y) - (origine.y) in
	let pas_x = longueur_x / !nb_points_cote and
	pas_y = longueur_y / !nb_points_cote in
	(for mi = 0 to !nb_points -1 do
		let q = (mi / !nb_points_cote) and r = (mi mod !nb_points_cote) in 
		tab_points.(mi) <- ((origine.x)+ r * pas_x, (origine.y) + q * pas_y)
	done);
	print_string "fin etape 1";
	
  
  (*ETAPE 2 : CREATION DES ROUTES*)
  
 	for mi = 0 to (!nb_points -1) do
		let q = mi / !nb_points_cote and r = mi mod !nb_points_cote in
		(if q != (!nb_points_cote -1) then
			ajouter_route mi (mi + !nb_points_cote)
		else ();
		if r != (!nb_points_cote -1) then
			ajouter_route mi (mi+1)
		else () )
	done;
  (*ETAPE 3 : On pondère les carrefours en fonctions des globules ou non *)
	if param_globule then 
		for i_glob = 0 to nb_glob -1 do
			let mi = Random.int (!nb_points -1) in
			let i_depart = Random.int (Array.length tab_depart.(mi)) in
			let mj = tab_depart.(mi).(i_depart) and position = Random.float (l -. Float.epsilon) in
			(tab_glob.(i_glob) <- {debut = mi ; arrivee = mj ; dist = position ; vel = 0.};
			tab_N.(mi).(mj) <- tab_N.(mi).(mj) + 1)
		done
	
	
	else  
	(* dans ce cas ci tous les voisins ont une chance égale d'etre pris *)
		for mi = 0 to (!nb_points -1) do
			let nb_voisins = Array.length tab_depart.(mi) in
			let proba_par_voisin = 1. /. float_of_int nb_voisins in
			for i = 0 to (nb_voisins -1) do
				proba_depart.(mi).(i) <- proba_par_voisin
			done
		done;;
				


(*PARTIE 2 : ALGORITHMIQUE*)

let log2 y =
	(log y ) /. (log 2.);;

let rec distance_trajet (trajet : int list) = 
	match trajet with 
	|[] -> 0.
	|m2::[] -> 0.
	|m1::m2::reste_trajet -> route.(m1).(m2) +. (distance_trajet (m2::reste_trajet));;

if generation then
	(if configuration = "simple"
		then (creation_carte_simpliste () ) )
	else ();;
	(*else
		creation_carte_realiste () ;;*) 

let choisis_depart (proba: float ref) mi =
	let indice_voisin = ref 0 in 
	while proba_depart.(mi).(!indice_voisin) < !proba && !indice_voisin != (Array.length tab_depart.(mi) -1) do
		( proba := (!proba -. proba_depart.(mi).(!indice_voisin));
		incr indice_voisin)
	done;
	tab_depart.(mi).(!indice_voisin);;

let rec etape_suivante trajet mi =
	if mi = !nb_points -1 then trajet
	else(*on choisit un voisin de mi pour continuer le trajet *)
		(let proba = ref (Random.float 1.) in
		let prochain_passage = choisis_depart proba mi in
		if Array.memq (mi,prochain_passage) (!route_coupee)
			then trajet
		else
			(tab_passage.(mi).(prochain_passage) <- tab_passage.(mi).(prochain_passage) +1;
			etape_suivante (trajet @ [prochain_passage]) prochain_passage) );;

let creation_trajet_RBC () =
	(* création du trajet d'un globule rouge
	trajet : int ist commençant en 0 et finissant en !nb_points*)
	etape_suivante [0] 0;;

let creation_pop nb_individus =
	let tab = Array.make nb_individus [] in
	for i = 0 to nb_individus -1 do
		tab.(i) <- creation_trajet_RBC ()
	done;
	tab;;
	
let min_selon_voisin mat = 
	let min = ref mat.(0).(tab_depart.(0).(0)) in
	for mi = 0 to !nb_points -2 do
		let nb_voisins = ((Array.length tab_depart.(mi)) -1) in
		for i_voisin = 0 to nb_voisins do
			if mat.(mi).(tab_depart.(mi).(i_voisin)) < !min 
				then min := mat.(mi).(tab_depart.(mi).(i_voisin))
		done
	done;
	!min;;
	
let max_selon_voisin mat =
	let max = ref mat.(0).(tab_depart.(0).(0)) in
	for mi = 0 to !nb_points -2 do
		let nb_voisins = (Array.length tab_depart.(mi) -1) in
		for i_voisin = 0 to nb_voisins do
			if mat.(mi).(tab_depart.(mi).(i_voisin)) > !max 
				then max := mat.(mi).(tab_depart.(mi).(i_voisin))
		done
	done;
	!max;;

(* Avec globules + fluides *)


(*Etape 1 : calcul de Re*)

let re_pour mi mj =  
	let reste = ( (beta *. l_chap *. ( float_of_int tab_N.(mi).(mj) ) ) /. l ) in
	r_maj *. (1. +. reste ) ;;

let calcul_Re () = 
	for mi = 0 to !nb_points - 1 do
		for i_voisin = 0 to Array.length tab_depart.(mi) - 1 do
			let mj = tab_depart.(mi).(i_voisin) in
			if tab_N.(mi).(mj) > 0 then
				tab_Re.(mi).(mj) <- (re_pour mi mj)
			else 
				tab_Re.(mi).(mj) <- r_maj
		done
	done ;;

let calcul_R () =
	for mi = 0 to !nb_points - 1 do
		for i_voisin = 0 to Array.length tab_depart.(mi) - 1 do
			let mj = tab_depart.(mi).(i_voisin) in
			tab_Re.(mi).(mj) <- r_maj
		done
	done ;;
	
(*Etape 2 : calcul des pi 
        PIVOT DE GAUSS    *)
let set_up_mat_coeff () = 
	(*on vide la matrice des coeffs et initialise tab_q *)
	(for i = 0 to !nb_points -1 do
		(for j = 0 to !nb_points -1 do
			mat_coeff.(i).(j) <- 0.
		done ;
		tab_q.(i) <- 0.)
	done;
	tab_q.(0) <- q0 ;
	tab_q.(!nb_points -1) <- q0;
	(* on remplit la matrice avec les coefficients associés *)
	for mi = 0 to !nb_points -1 do
		(for i_depart = 0 to ((Array.length tab_depart.(mi) ) -1 ) do
			(*on teste les departs possibles de mi *)
			(let mj = tab_depart.(mi).(i_depart) in
			(mat_coeff.(mi).(mj) <- mat_coeff.(mi).(mj) +. ( -1. /. tab_Re.(mi).(mj) );
			mat_coeff.(mi).(mi) <- mat_coeff.(mi).(mi) +. (1. /. tab_Re.(mi).(mj) ) 
			) 
		)done;
		
		for i_arrivee = 0 to (Array.length tab_arrivee.(mi)) -1 do 
			(*on teste les arrivees possibles de mi *)
			(let m_debut = tab_arrivee.(mi).(i_arrivee) in
			(mat_coeff.(mi).(m_debut) <- mat_coeff.(mi).(m_debut) +. ( -1. /. tab_Re.(m_debut).(mi) );
			mat_coeff.(mi).(mi) <- mat_coeff.(mi).(mi) +. (1. /. tab_Re.(m_debut).(mi) ) )
		)done )
	done);; 
	
let echange_ligne mat i i' =
	let tab_i = Array.copy mat.(i) in
	for j = 0 to (Array.length mat.(i)) -1 do
		mat.(i).(j) <- mat.(i').(j);
		mat.(i').(j) <- tab_i.(j)
	done;;
	
let echange_tab tab i j =
	let tab_i_initial = tab.(i) in
	tab.(i) <- tab.(j);
	tab.(j) <- tab_i_initial;;

let transvection mat i j lam = 
	for k = j to (Array.length mat.(i)) -1 do
		mat.(i).(k) <- mat.(i).(k) +. lam *. mat.(j).(k)
	done ;;

let pivot_partiel mat j0 =
	let n = Array.length mat in
	let i_max = ref j0 in
	for i = j0 + 1 to n-1 do
		if (Float.abs mat.(i).(j0) ) > (Float.abs mat.(!i_max).(j0) )
			then i_max := i 
	done; !i_max ;;
	
let triangulise_gauss mat b =
	let n = Array.length mat in
	(* mise sous forme triangulaire  *)
	for j = 0 to n - 2 do
		let i0 = pivot_partiel mat j in
		echange_ligne mat j i0 ;
		echange_tab b j i0;
		for i = j +1 to n-1 do
			let lam = mat.(i).(j) /. mat.(j).(j) in 
			transvection mat i j (-1. *. lam) ;
			mat.(i).(j) <- 0. ;
			b.(i) <- b.(i) -. (lam *. b.(j))
		done
	done;; 
	
let phase_elimination_gauss () =
	for k = 0 to !nb_points -2 do
		(if Float.abs mat_coeff.(k).(k) < Float.epsilon then
			(let lpivot = ref (-1) and ligne = ref (k+1) in
			while !ligne < !nb_points -1 && !lpivot = -1 do
				if mat_coeff.(!ligne).(k) != 0. then
					lpivot := !ligne
				else incr ligne
			done ;
			if !lpivot != -1 then
				(echange_ligne mat_coeff !lpivot k ;
				echange_tab tab_q !lpivot k)
			else raise (Foo "pas de solution ") )
		else ();
		for i = k + 1 to !nb_points -1 do
				let lam = mat_coeff.(i).(k) /. mat_coeff.(k).(k) in
				(tab_q.(i) <- tab_q.(i) -. (lam *. tab_q.(k)) ;
				(for j = k to !nb_points - 1 do
					mat_coeff.(i).(j) <- mat_coeff.(i).(j) -. (lam *. mat_coeff.(k).(j))
				done);
				mat_coeff.(i).(k) <- 0.)
		done)
	done;; 

(*let phase_substitution_gauss () = 
	tab_p.(!nb_points -1) <- 0. ;
	for i = 1 to !nb_points -1 do
		let mi = !nb_points -1 -i in
		let somme_ax = ref 0. in
		(for mj = mi +1 to !nb_points -1 do
			somme_ax := !somme_ax +. (tab_p.(mj) *. mat_coeff.(mi).(mj) )
		done;
		let p_mi = (1. /. mat_coeff.(mi).(mi) ) *. (tab_q.(mi) -. !somme_ax) in
		tab_p.(mi) <- p_mi )
	done;; *)
	
let phase_substitution_gauss () = 
	tab_p.(!nb_points -1) <- 0. ;
	for i = 1 to !nb_points - 1  do
		let mi = !nb_points -1 -i in
		let somme_ax = ref 0. in
		(for mj = mi +1 to !nb_points - 1 do
			somme_ax := !somme_ax +. (tab_p.(mj) *. mat_coeff.(mi).(mj) )
		done;
		let p_mi = (1. /. mat_coeff.(mi).(mi) ) *. (tab_q.(mi) -. !somme_ax) in
		tab_p.(mi) <- p_mi )
	done;;

(* CORRECTION *)
let correct_qij_neg mi mj q = 
	mat_qij.(mi).(mj) <- Float.abs q ;;
(*ETAPE 3 : calcul des qij *)
let calcul_qij () = 
	for mi = 0 to !nb_points - 1 do
		for i_voisin = 0 to Array.length tab_depart.(mi) - 1 do
			let mj = tab_depart.(mi).(i_voisin) in
			let q = ((tab_p.(mi) -. tab_p.(mj) ) /. (tab_Re.(mi).(mj)) ) in
			(if q < 0. then
				(print_string ("q = "^(string_of_float q)^" pour" ^ (string_of_int mi) ^ " et " ^ (string_of_int mj) ^ " \n" );
				print_string ("il y a " ^ (string_of_int tab_N.(mi).(mj))^ "globules ici \n") ;
				correct_qij_neg mi mj q)
			else 
				mat_qij.(mi).(mj) <- q)
		done 
	done;;
	
(*Etape 4 : calculer la velocité et temps de parcours restant *)
let calcul_vel_temps_restant () =
	for gi = 0 to nb_glob -1 do 
		let mi = tab_glob.(gi).debut and mj = tab_glob.(gi).arrivee and dist_parc = tab_glob.(gi).dist in
		let vitesse = ( 4. *. mat_qij.(mi).(mj) *. theta ) /. (Float.pi *. roh *. (Float.pow d 2.) ) in
		tab_glob.(gi).vel <- vitesse;
		tab_temps_restant.(gi) <- (l -. dist_parc) /. vitesse
	done;;

(*Etape 5 : propagation et regle de bifurcation *)	


let depart_max_gradient mj =
	(*donne le mi voisin de mj ayant le plus grand gradient de qij depuis mj *)
	if bifurc_simple then
		(if Array.length tab_depart.(mj) = 1 then 
			tab_depart.(mj).(0)
		else 
			let q_vois0 = mat_qij.(mj).(tab_depart.(mj).(0)) and q_vois1 = mat_qij.(mj).(tab_depart.(mj).(1)) in
			let prop = q_vois0 /. q_vois1 in let prop' = Float.abs ( prop -. 1.) in
			let proba = Random.float (1. +. prop') in
			if proba < 0.5
				then ( if prop > 1. 
					then tab_depart.(mj).(1)
				else 
					tab_depart.(mj).(0) )
			else 
				(if prop > 1. 
					then tab_depart.(mj).(0)
				else 
					tab_depart.(mj).(1) ) )
	else
		if Array.length tab_depart.(mj) = 1 then 
			tab_depart.(mj).(0)
		else 
			(if mat_qij.(mj).(tab_depart.(mj).(0)) > mat_qij.(mj).(tab_depart.(mj).(1))
				then tab_depart.(mj).(0)
			else tab_depart.(mj).(1) );;
	
let bifurcation gi = 
	print_string "glob bifurqué : " ;
	print_string ((string_of_int gi) ^" depuis " ^(string_of_int tab_glob.(gi).arrivee) ^"jusqu a ");
	let mj = tab_glob.(gi).arrivee and mi = tab_glob.(gi).debut in
	(if mj = !nb_points - 1 then
		let depart_inflow = depart_max_gradient 0 in
		(tab_N.(mi).(mj) <- tab_N.(mi).(mj) - 1; 
		tab_N.(0).(depart_inflow) <- tab_N.(0).(depart_inflow) + 1;
		tab_glob.(gi) <- {debut = 0 ; arrivee = depart_inflow ; dist = Float.epsilon; vel = 0.};
		print_string ((string_of_int depart_inflow) ^ "\n") )
	else 
		(let depart_depuis_mj = depart_max_gradient mj in
		tab_N.(mj).(depart_depuis_mj) <- tab_N.(mj).(depart_depuis_mj) + 1;
		tab_N.(mi).(mj) <- tab_N.(mi).(mj) - 1;
		tab_glob.(gi) <- {debut = mj ; arrivee = depart_depuis_mj ; dist = Float.epsilon; vel = 0.};
		print_string ((string_of_int depart_depuis_mj) ^ "\n" ) ) );;	
	
let propagation_et_bifurc_RBC () =
	let i_glob_min = indice_min tab_temps_restant in
	let temps_propa = tab_temps_restant.(i_glob_min) in
	temps_passe := !temps_passe +. temps_propa ;
	for gi = 0 to nb_glob -1 do
		if gi != i_glob_min then
			(tab_glob.(gi).dist <- tab_glob.(gi).dist +. (tab_glob.(gi).vel *. temps_propa) )
		else
			bifurcation gi 
	done ;
	print_string "\n";;

(*Algorithme complet *)
let prochaine_etape () =
	if considerer_Re then
	calcul_Re ()
	else calcul_R ();
	set_up_mat_coeff ();
	phase_elimination_gauss () ;
	if est_triang_sup mat_coeff then print_string " matrice A triangulaire \n" ;
	phase_substitution_gauss ();
	calcul_qij ();
	calcul_vel_temps_restant ();
	nb_dist_nulle () ;
	propagation_et_bifurc_RBC ();
	print_string ("temps passé = " ^(string_of_float !temps_passe)^ "\n");;
	

	

(* BOUCHAGE / HEMMORAGIE *)

let pop_tab tab indice =
	let n = Array.length tab in
	let pre = Array.sub tab 0 indice and
	post = Array.sub tab (indice + 1) (n - indice - 1)
	in Array.append pre post;;

let enlever_route mi mj = 
	(*enleve la possibilité de passer par une route et ne la compte pas comme bouchée, juste inexistante*)
	route.(mi).(mj) <- infinity;
	let indice_mj_voisin = ref max_int and k = ref 0 in
	(while !k <= (Array.length tab_depart.(mi) -1) && !indice_mj_voisin = max_int do
		if tab_depart.(mi).(!k) = mj 
			then (indice_mj_voisin := !k)
		else incr k
	done;
	
	if !k = (Array.length tab_depart.(mi)) 
		then raise ( Foo "mj n'est pas voisin de mi")
	else 
		(tab_depart.(mi) <- pop_tab tab_depart.(mi) !indice_mj_voisin ;
		proba_depart.(mi) <- pop_tab proba_depart.(mi) !indice_mj_voisin ;
		if param_globule = false then
			let nb_voisins = Array.length tab_depart.(mi) in
			let proba_par_voisin = 1. /. float_of_int nb_voisins in
			for i = 0 to (nb_voisins -1) do
				proba_depart.(mi).(i) <- proba_par_voisin
			done
		else () (* ***************** *)));;

let boucher_route mi mj = 
	(*bouche une route, la comptant dans le tableau passage *)
	route.(mi).(mj) <- infinity;
	let nb_voisins = Array.length tab_depart.(mi) in
	let indice_mj_voisin = ref max_int and k = ref 0 in
	
	(* On recupere l'indice mj dans tab_voisin*)
	(while !k <= (nb_voisins) && !indice_mj_voisin = max_int do
		if tab_depart.(mi).(!k) = mj 
			then (indice_mj_voisin := !k)
		else incr k
	done;
	
	(*on agis en conséquences *)
	if !k = (Array.length tab_depart.(mi)) 
		then raise ( Foo "mj n'est pas voisin de mi")
	else 
		if param_globule = false then
			let proba_par_voisin = 1. /. float_of_int (nb_voisins -1) in
			for i = 0 to (nb_voisins -1) do
				if tab_depart.(mi).(i) != mj then
					proba_depart.(mi).(i) <- proba_par_voisin
				else
					(proba_depart.(mi).(i) <- 0.;
					tab_passage.(mi).(mj) <- -1)
			done
		else () (* ***************** *));;

let somme_vois mi = 
	let somme = ref 0. in 
	for i = 0 to (Array.length tab_depart.(mi) )-1 do
		let mj = tab_depart.(mi).(i) in
		somme := !somme -. mat_qij.(mi).(mj)
	done;
	for i = 0 to (Array.length tab_arrivee.(mi) )-1 do
		let mj = tab_arrivee.(mi).(i) in
		somme := !somme +. mat_qij.(mj).(mi)
	done;
	!somme ;;
	
let tab_qi () = 
	let tab = Array.make !nb_points 0. in
	for mi = 0 to !nb_points -1 do 
		tab.(mi) <- somme_vois mi
	done; tab;;

let reynolds = (4. *. q0 ) /. (Float.pi *. d *. mu) ;;
#use "visuel.ml";;
prochaine_etape () ; actualiser () ;;
