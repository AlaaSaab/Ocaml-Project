(*-----------------------------------------------
Alaa SAAB <alaasaab194@gmail.com>
-------------------------------------------------*)


type dimension = int;; (*restreint aux entiers strictement positifs*)

type case = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)
type vecteur = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)

type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (*Les couleurs des joueurs*)
             | Libre 
             | Code of string (*une chaine restreinte a 3 caracteres*);;


type case_coloree = case * couleur;;

type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)
          
type coup = Du of case * case | Sm of case list;;

let indice_valide (x:int) (dim:dimension) : bool =
  x >= -2*dim && x<= 2*dim;;

let est_case ((i,j,k):case):bool=
  (i+j+k=0);;

(*Q1.*)
(*ð‘– < âˆ’ð‘‘ð‘–ð‘š :
(-4,1,3) (-5,2,3) (-6,3,3) (-5,3,2) (-4,3,1)

ð‘– > ð‘‘ð‘–ð‘š :
(4,1,3) (5,2,3) (6,3,3) (5,3,2) (4,3,1)

ð‘— < âˆ’ð‘‘ð‘–ð‘š :
(0,3,0)

(ð‘–, ð‘—, ð‘˜) = (2ð‘‘ð‘–ð‘š, âˆ’ð‘‘ð‘–ð‘š, âˆ’ð‘‘ð‘–ð‘š) :
((6,-3,-3)

(ð‘–, ð‘—, ð‘˜) = (âˆ’ð‘‘ð‘–ð‘š âˆ’ 1, 1, ð‘‘ð‘–ð‘š) :
(-4,1,3)

ð‘– â‰¥ âˆ’ð‘‘ð‘–ð‘š âˆ§ ð‘— â‰¥ âˆ’ð‘‘ð‘–ð‘š âˆ§ ð‘˜ â‰¥ âˆ’ð‘‘ð‘–ð‘š :
C'est  tous les pions contenus dans le triangle  Ã©qulaterale ayant pour 
  sommet le pion jaune le plus Ã©lÃ©vÃ© en hauteur et pour centre de gravitÃ© le point (0,0,0) 
*)

(*Q2.*)
(*
 âˆ’dim â‰¤ j â‰¤ dim
*)


(*A MODIFIER en Q2*)
let est_dans_losange (i,j,k:case) (dim:dimension) :bool =
  (i<= 2*dim) && (i>= -2*dim) &&
  (j<=dim) && (j>= (-dim)) &&
  (k<=dim) && (k>= (-dim)) &&
  (est_case (i,j,k));;         

(*A MODIFIER en Q3*)
let est_dans_etoile (i,j,k:case) (dim:int): bool=
  if not (est_case (i,j,k)) then false
  else (est_dans_losange (i,j,k) dim) || (est_dans_losange (k,i,j) dim) || (est_dans_losange (j,k,i) dim)
;;

(*Q4*)
let rec tourner_case (m: int) (ca: case) : case =
  let a,b,c = ca in 
  match m with 
  |1 -> (-c,-a,-b)
  |_ -> (tourner_case (m-1) (-c,-a,-b))

(*Q5*)
let translate (c1,c2,c3: case) (v1,v2,v3: vecteur) : case = 
  (c1 + v1, c2 + v2, c3 + v3);;

(*Q6*)
let diff_case (c1:case)(c2:case):vecteur=
  let (i,j,k)=c1 in let (m,n,l)=c2 in 
  (i-m,j-n,k-l);;

(*Q7*)
let sont_cases_voisines(c1:case)(c2:case):bool=
  diff_case c1 c2 = (1,0,-1)|| diff_case c1 c2 = (-1,0,1)|| 
  diff_case c1 c2 = (1,-1,0)||diff_case c1 c2 = (-1,1,0)||
  diff_case c1 c2 = (0,-1,1) ||diff_case c1 c2 = (0,1,-1) ;;

(*Q8*)
type option = None | Some of case ;;

(*Cette fonction est juste une fonction auxiliare permettant de verifier si deux case sont alignÃ©s*)
let sont_alignes (c1:case) (c2:case) : bool = 
  let (x,y,z)=c1 in
  let (i,j,k)=c2 in 
  est_case (i,j,k) && est_case (x,y,z) &&
  i=x || j=y || k=z ;;

let calcul_pivot (x,y,z:case) (i,j,k:case) : option = 
  let c = ((x+i)/2, (y+j)/2,(z+k)/2 ) in 
  if ( sont_alignes (x,y,z) (i,j,k) ) &&
     (x-i) mod 2 = 0 &&
     (y-j) mod 2 = 0 &&
     (z-k) mod 2 = 0 
  then 
    Some(c)
  else
    None;;

(*Q9*)
let vec_et_dist (x,y,z: case) (i,j,k: case) : (vecteur * int) =
  let (dx,dy,dz) = (i-x,j-y,k-z) in
  match (dx,dy,dz) with
  |(0,dy,dz) -> if dy >=0 then ((0,1,-1),abs(dy)) else ((0,-1,1),abs(dy)) 
  |(dx,0,dz) -> if dz >=0 then ((-1,0,1),abs(dx)) else ((1,0,-1),abs(dx)) 
  |(dx,dy,0) -> if dx >=0 then ((1,-1,0),abs(dy)) else ((-1,1,0),abs(dy)) 
  |_ ->(0,0,0),0;;
  
(*Q10*)
let tourner_liste (l:'a list) :'a list=
  match l with
  |[] -> []
  |hd::tl -> tl@[hd];;

let rec der_liste (l:'a list) :'a=
  match l with
  |[a]->a
  |hd::tl -> der_liste tl;;

(*Q11*)
let rec remplir (a:int) (b:case list) :case list =
  match a with
  |1 -> b
  |_ -> let (i,j,k) = (der_liste b) in 
      remplir (a-1) (b@[(i,j+1,k-1)]);;
let remplir_segment (a:int) (b:case) :case list = 
  remplir a [b];;

(*Q12*)

let rec rem_triangle_bas (a:int) (c:int) (b:case list) :case list =
  match a with 
  |0 -> b
  |_ -> let (x,y,z) = (List.hd b) in
      rem_triangle_bas (a-1) (c+1) (b@(remplir_segment a (x-c,y+c,z)));;

let remplir_triangle_bas (a:int) (b:case) :case list =
  List.tl (rem_triangle_bas a 0 [b]);;

(*Q13*)
let rec rem_triangle_haut (a:int) (c:int) (b:case list) :case list =
  match a with 
  |0 -> b
  |_ -> let (x,y,z) = (List.hd b) in
      rem_triangle_haut (a-1) (c+1) (b@(remplir_segment a (x+c,y,z-c)));;
let remplir_triangle_haut (a:int) (b:case) :case list =
  List.tl (rem_triangle_haut a 0 [b]);;

(*Q14*)
let rec colorie (a:couleur) (lc:case list) :case_coloree list = 
  match lc with
  |[] -> []
  |hd::tl -> (hd,a)::(colorie a tl);;

(*Q15*)
let rec tourner (n:int) (cl: case_coloree list) :case_coloree list =
  match cl with
  |[]->[]
  |hd::tl -> let (case,colo) = hd in 
      let (x,y,z) =case in (tourner_case n (x,y,z),colo)::(tourner n tl);;

let rec tourner_n (n:int) (con: configuration) :configuration = 
  let (cl,cololist,dim) = con in
  ((tourner n cl),(tourner_liste cololist),dim);;

let tourner_config (con:configuration) :configuration = 
  let (cl,cololist,dim) = con in
  tourner_n (6/(List.length cololist)) con;;

(*Q16*)
let mergeandturn (a:configuration) (b:configuration) :configuration=
  let a1,a2,a3 = a in 
  let b1,b2,b3 = b in 
  tourner_config (a1@b1,a2,a3);;
let merge_list (l:configuration list) : configuration = 
  let l1 = (List.hd l) in 
  let a,b,c = l1 in 
  List.fold_left mergeandturn ([],b,c) l;;
let rec r_init (li: 'a list) (dim:int) :configuration list=
  match li with 
  |[]->[[],[Rouge],dim]
  |hd::tl ->[(colorie hd (remplir_triangle_bas dim (-dim-1,1,dim)),li,dim)]@(r_init tl dim);;
let remplir_init (li: 'a list) (dim: int):configuration = 
  merge_list (r_init li dim);;

(*Q17*)
let rec associe (a:'a) (l:('a*'b) list) (defaut:'b):'b = 
  match l with
  |[] -> defaut
  |(el1,el2)::_ when el1=a -> el2
  |_::q -> associe a q defaut
;;

let quelle_couleur (c:case) (config: configuration) :couleur =
  let a,_,_ = config in
  associe c a Libre;;

(*Q18*)
let supprime_dans_config (config: configuration) (c:case) :configuration =
  let a,cl,dim = config in 
  let rec sup (c:case) (li: case_coloree list) :case_coloree list= 
    match li with
    |[]->[]
    |hd::tl -> let cas,_ = hd in 
        if cas = c then tl else hd::(sup c tl) 
  in
  (sup c a),cl,dim;;

(*Q19*)
let est_coup_valide (config: configuration) (turn: coup) :bool =
  let _,cl,dim = config in 
  match turn with
  |Du (c1,c2) -> (quelle_couleur c1 config) = (List.hd cl) && (sont_cases_voisines c1 c2)
                 && (quelle_couleur c2 config) = Libre && (est_dans_losange c2 dim)
  |_ -> false;;

(*Q20*) 
let appliquer_coup (config:configuration) (turn: coup) :configuration = 
  match turn with 
  |Du (c1,c2) -> let li,cl,dim = (supprime_dans_config config c1) in 
      ((c2,List.hd cl)::li,cl,dim)
  |_ -> config;;

(*Q21*)
let mettre_a_jour_configuration (config:configuration) (turn: coup) :configuration =
  if (est_coup_valide config turn) then appliquer_coup config turn else failwith "Coup invalide";;


(*Q22*)
let rec est_libre_seg (c1: case) (c2: case) (config: configuration) :bool = 
  let vec,dis = (vec_et_dist c1 c2) in 
  match dis with 
  |1 -> true
  |_ -> (quelle_couleur (translate c1 vec) config = Libre) && (est_libre_seg (translate c1 vec) c2 config);;

(*Q23*)
let est_saut (c1: case) (c2: case) (config:configuration) :bool = 
  let _,cl,dim = config in 
  if (est_dans_losange c2 dim) && ((quelle_couleur c2 config) = Libre) && ((quelle_couleur c1 config) = (List.hd cl)) then
    let c3 = (calcul_pivot c1 c2) in 
    match c3 with 
    |None -> false
    |Some(c) -> (not ((quelle_couleur c config) = Libre)) &&  (est_libre_seg c1 c config) && (est_libre_seg c c2 config)
  else false;;

(*Q24*)
let rec est_saut_multiple (li: case list) (config: configuration) :bool=
  match li with
  |hd::[] ->true
  |hd::tl -> est_saut (hd) (List.hd tl) config && est_saut_multiple tl config
  |_-> false;;

(*Q25*)
let rec est_coup_valide2 (config:configuration) (turn:coup): bool = 
  let _,cl,dim = config in 
  match turn with 
  |Du (c1,c2) -> (quelle_couleur c1 config) = (List.hd cl)
                 && (quelle_couleur c2 config) = Libre && (est_dans_losange c2 dim) && (sont_cases_voisines c1 c2)
  |Sm(li) ->
      match li with
      |hd::[] ->true
      |hd::tl -> est_saut (hd) (List.hd tl) config && (est_coup_valide2 (appliquer_coup config (Du((hd),(List.hd tl)))) (Sm(tl)))
      |_-> false;;

let rec appliquer_coup2 (config:configuration) (turn: coup) :configuration = 
  match turn with 
  |Du (c1,c2) -> let li,cl,dim = (supprime_dans_config config c1) in 
      ((c2,List.hd cl)::li,cl,dim)
  |Sm(cali) -> match cali with 
    |[]-> config
    |hd::[] -> config
    |_->
        let c1 = (List.hd cali) in
        let c2 = (List.hd (List.tl cali)) in 
        appliquer_coup2 (appliquer_coup2 config (Du(c1,c2))) (Sm(List.tl cali));;

let mettre_a_jour_configuration2 (config:configuration) (turn: coup) :configuration =
  if (est_coup_valide2 config turn) then appliquer_coup2 config turn else failwith "Coup invalide";;

(*Q26*)
let takepoint (cl: couleur) (a:case_coloree)  :int=
  let coords,colo = a in 
  let i,_,_ = coords in 
  if colo = cl then i 
  else 0;;

let score (a:configuration) :int = 
  let li,coli,_ = a in 
  let cl = List.hd coli in 
  List.fold_left (+) 0 (List.map (takepoint cl) li) ;;

let score_gagnant (dim:int) :int = 
  score((colorie Rouge (remplir_triangle_haut dim (dim+1,-dim,-1))),[Rouge],dim);; 

(*Q27*)
let gagne (config:configuration) :bool= 
  let _,_,dim = config in 
  (score config) = score_gagnant dim ;;

(*Q28*)
let check (config:configuration) (cl: couleur) :couleur = 
  match cl with 
  |Libre -> if (gagne config) then let _,clist,_ = config in (List.hd clist) else Libre
  |x -> x;; 

let give_config_list (configs: configuration list) (turn: coup) :configuration list = 
  tourner_config (mettre_a_jour_configuration2  (List.hd configs) turn)::configs;;

let est_partie (config: configuration) (turns:coup list) :couleur = 
  let everyturn = List.fold_left give_config_list [config] turns in 
  List.fold_right check (List.map tourner_config (List.map tourner_config everyturn)) Libre;;

(*Q29*)
let rec direction_check (ca:case) (i,j,k:vecteur) (n:int) (config:configuration) :coup list =  
  let _,_,dim = config in 
  if n >= 2*dim+1 then 
  [] else
  if n = 1 then 
    if est_coup_valide2 config (Du(ca,translate ca (i*n,j*n,k*n))) then [(Du(ca,translate ca (i*n,j*n,k*n)))]@(direction_check (ca) (i,j,k) (n+1) config) else
      (direction_check (ca) (i,j,k) (n+1) config)
    else
  if est_coup_valide2 config (Sm([ca;translate ca (i*n,j*n,k*n)])) then [(Sm[ca;translate ca (i*n,j*n,k*n)])] else
    (direction_check (ca) (i,j,k) (n+1) config);;

let sixpath (ca:case) (config:configuration) (n:int) :coup list = 
  direction_check ca (1,0,-1) n config @
  direction_check ca (-1,0,1) n config @
  direction_check ca (1,-1,0) n config @
  direction_check ca (-1,1,0) n config @
  direction_check ca (0,1,-1) n config @
  direction_check ca (0,-1,1) n config ;;

let (>>) (a:coup) (b:coup) :coup = 
  match a with |Sm(x) -> match b with |Sm(_::y) -> Sm(x@y);;
let rec dernier (a:coup) :case = 
  match a with
  |Du(c1,c2) -> c2
  |Sm(li) -> match li with 
    |c1::[] -> c1
    |hd::tl -> dernier (Sm(tl));;

let rec countsm (li: coup list) :int = 
  match li with
  |[]-> 0 
  |hd::tl -> match hd with 
    |Du(_,_) -> 0 + countsm tl
    |Sm(_) -> 1 + countsm tl;;

let extend (turn:coup) (config:configuration) :coup list =
  match turn with 
  |Du(_,_) -> []
  |Sm(li) -> let cases,cl,dim = config in 
              List.map ((>>) turn) (sixpath (dernier turn) (cases@[(dernier turn),(List.hd cl)],cl,dim) 2);;

let rec extender (turns:coup list) (config:configuration): coup list = 
  match turns with
  |[]->[]
  |turn::tl -> turn::(extend turn config)@(extender tl config);;

let rec existin (a:'a) (li: 'a list) :bool = 
  match li with
  |[] -> false
  |hd::tl -> if a = hd then true else existin a tl;;

let rec alreadyin (turn:'coup') (turnlist: coup list) :bool = 
  let c1 = dernier turn in 
  match turnlist with 
  |[] -> false
  |hd::tl -> ((c1 = dernier hd) && (not(turn = hd)))|| alreadyin turn tl;;

let rec isextendable (turns: coup list) (turnlist: coup list) (config:configuration):bool =
  if (extender turns config) = turns then false else
  match turns with 
  |[]-> false
  |hd::tl -> match hd with 
  |Du(_,_)->isextendable tl turnlist config
  |Sm(_) ->if alreadyin hd turnlist then isextendable tl turnlist config else true;;

let rec extsup (turns: coup list) (config:configuration) :coup list =
  if isextendable turns turns config then let ext = extender turns config in  
  extsup ext config else turns;;

let rec duplicate (li:'a list) :bool = 
  match li with 
  |[] -> false
  |hd::tl -> (existin hd tl) || (duplicate tl)

let is_a_loop (turn:coup) :bool =
  match turn with
  |Du(_,_) -> false
  |Sm(li) -> duplicate li;;

let sweep (turns: coup list) (config:configuration) : (case*coup) list =
  let rec sweeper (turns:coup list) (turnlist: coup list) (config:configuration):(case*coup) list=
    match turns with 
    |[]->[] 
    |hd::tl -> if (existin hd tl) || (is_a_loop hd) ||(not (est_coup_valide2 config hd)) then (sweeper tl turnlist config)
    else ((dernier hd),hd)::(sweeper tl turnlist config) in 
    sweeper turns turns config;;

let coup_possibles (config:configuration) (c1:case) :(case*coup) list = 
  sweep (extsup (sixpath c1 config 1) config) config;;


(*Q30*)
let rec tous_coup_possibles (config:configuration) (config2:configuration):((case*coup) list)  = 
  let cases,cl,dim=config in 
  let color = List.hd cl in
  match cases with
  |[] -> []
  |hd::tl -> let c1,coul = hd in
    if coul = color then (coup_possibles config2 c1)@(tous_coup_possibles (tl, cl,dim) config2) else 
    []@(tous_coup_possibles (tl, cl,dim) config2);;

let rec calcul_scores (li:(case*coup) list) (config: configuration) :(int*coup) list = 
  match li with
  |[]->[]
  |hd::tl -> let c1,coup = hd in
    (score (mettre_a_jour_configuration2 config coup),coup)::calcul_scores tl config;;

let strategie_gloutonne (config:configuration) :coup = 
  let compare_score (a,b: int*coup) (c,d:int*coup) :int*coup= 
    if a>c then a,b else c,d in
  let li = calcul_scores (tous_coup_possibles config config) config in
  let scr,coup = List.fold_left compare_score (List.hd li) li in coup;;

(*AFFICHAGE (fonctionne si les fonctions au dessus sont remplies)*)
(*transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case (i,j,k)*)
let transfo x y = (y, (x-y)/2,(-x-y)/2);;

let couleur2string (coul:couleur):string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M ";;

let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
  if m = (4 * dim) + 1 then " " (*fin de ligne*)
  else
    let c = transfo m n in
    if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
      "   "^ affiche_ligne n (m + 1) config
    else (*ceci est une case ou bien en dehors du plateau*)
      (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


let affiche (config:configuration):unit =
  let (_,_,dim)=config in
  let rec affiche_aux n =
    if n = - 2 * dim - 1 then ()
    else
      begin
        print_endline (affiche_ligne n (-4*dim-1) config);
        print_endline "\n";
        affiche_aux (n - 1)
      end
  in
  affiche_aux (2*dim+1);;

let conf_1=([((0,0,0),Jaune)],[Jaune],2);;
affiche conf_1;;
let conf_reggae=([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],[Vert;Jaune;Rouge],1);;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;

(*POUR TESTER, NE PAS TOUCHER*)
let cool=([(( 0,1,-1),Vert);((4,-2,-2),Rouge);((2,-1,-1),Rouge);((-1, 0, 1),Rouge);((-3, 0, 3),Vert);((-1, 1, 0),Jaune)],[Vert;Jaune;Rouge],3);;
affiche cool;;
let defa = remplir_init [Rouge;Vert;Jaune] 3;;
let test1 = sixpath (2,-1,-1) cool 1;;
let test2 = extender test1 cool;;
let test3 = extender test2 cool;;
let test4 = extender test3 cool;;
let test5 = sixpath (-5,3,2) defa 1;;
 

let rec testercoups (li:(case*coup) list) (config: configuration) :(bool*coup) list = 
  match li with
  |[]->[]
  |hd::tl -> let c1,coup = hd in
    [((est_coup_valide2 config coup),coup)]@testercoups tl config;;
let game1 = [Du((-4, 2, 2), (-3, 2, 1));
             Du((-4, 2, 2), (-3, 1, 2));
             Sm([(-5, 3, 2); (-3, 1, 2)]);
             Sm([(-4, 3, 1); (-2, 1, 1)]);
             Sm([(-4, 1, 3); (-2, 1, 1)]);
             Sm([(-6, 3, 3); (-2, 3, -1)]);
             Sm([(-6, 3, 3); (-4, 3, 1)]);
             Sm([(-5, 3, 2); (-3, 3, 0)]);
             Sm([(-4, 2, 2); (-2, 0, 2)]);
             Sm([(-3, 2, 1); (-1, 0, 1)]);
             Sm([(-6, 3, 3); (-4, 1, 3)]);
             Sm([(-5, 2, 3); (-3, 0, 3); (-1, 0, 1)]);
             Sm([(-5, 3, 2); (-3, 3, 0)]);
             Sm([(-4, 3, 1); (0, -1, 1); (2, -1, -1)]);
             Sm([(-4, 3, 1); (0, 3, -3); (2, 1, -3); (4, -1, -3)]);
             Sm([(-2, 1, 1); (0, -1, 1)]);
             Du((-5, 2, 3), (-4, 2, 2));
             Sm([(-4, 1, 3); (-2, 1, 1); (0, -1, 1); (2, -1, -1)]);
             Sm([(-5, 2, 3); (-3, 0, 3)]);
             Sm([(-4, 2, 2); (-2, 0, 2)]);
             Sm([(-2, 0, 2); (0, 0, 0); (2, -2, 0); (2, 0, -2)]);
             Du((-3, 3, 0), (-2, 2, 0));
             Sm([(-3, 1, 2); (-1, -1, 2)]);
             Sm([(-2, 3, -1); (0, 1, -1); (2, 1, -3)]);
             Sm([(-2, 2, 0); (0, 2, -2)]);
             Du((2, -1, -1), (3, -1, -2));
             Du((4, -1, -3), (5, -2, -3));
             Sm([(-3, 0, 3); (1, 0, -1); (3, -2, -1)]);
             Du((3, -1, -2), (4, -2, -2));
             Sm([(-3, 1, 2); (-1, -1, 2)]);
             Du((3, -2, -1), (4, -2, -2));
             Du((4, -2, -2), (5, -2, -3));
             Sm([(2, 1, -3); (4, -1, -3)]);
             Sm([(0, -1, 1); (4, -1, -3)]);
             Sm([(-2, 1, 1); (2, 1, -3)]);
             Du((2, -1, -1), (3, -2, -1));
             Sm([(-4, 3, 1); (-2, 1, 1); (0, -1, 1)]);
             Du((-1, -1, 2), (-1, 0, 1));
             Du((3, -2, -1), (4, -2, -2));
             Sm([(0, -1, 1); (2, -1, -1)]);
             Sm([(-3, 3, 0); (-1, 1, 0); (1, 1, -2)]);
             Sm([(-1, -1, 2); (3, -1, -2)]);
             Du((2, -1, -1), (3, -2, -1));
             Sm([(-2, 0, 2); (0, 0, 0); (0, 2, -2); (2, 0, -2)]);
             Du((4, -2, -2), (5, -3, -2));
             Sm([(3, -2, -1); (5, -2, -3)]);
             Sm([(1, 1, -2); (3, -1, -2)]);
             Sm([(2, 0, -2); (4, -2, -2)]);
             Du((-1, 0, 1), (0, 0, 0));
             Sm([(-1, 0, 1); (1, 0, -1)]);
             Sm([(-1, 0, 1); (1, 0, -1)]);
             Du((0, 0, 0), (1, 0, -1));
             Sm([(2, 0, -2); (4, -2, -2)]);
             Sm([(4, -1, -3); (6, -3, -3)]);
             Sm([(-4, 1, 3); (2, 1, -3)]);
             Du((1, 0, -1), (2, 0, -2));
             Du((4, -2, -2), (4, -3, -1));
             Du((0, 2, -2), (0, 3, -3));
             Du((2, 0, -2), (3, 0, -3));
             Du((1, 0, -1), (2, -1, -1));
             Du((2, 1, -3), (2, 0, -2));
             Du((3, -1, -2), (4, -1, -3));
             Sm([(2, -1, -1); (4, -1, -3)]);
             Du((2, 0, -2), (2, 1, -3));
             Du((2, 1, -3), (2, 0, -2));
             Du((3, -1, -2), (4, -2, -2))];;




(*A essayer apres avoir fait remplir_init
affiche (remplir_init [Code "red"; Code "blue";Jaune;Bleu;Vert;Code "bob"] 3);;
*)


