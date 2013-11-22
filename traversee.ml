
(*
 * Delauney Cynthia
 * 3009454
 *
 * traversee.ml
 *
 *)

(* ============================================================================= *)
(*                          Declaration des types                                *)
(* ============================================================================= *)

(*
 * configurations = (nE, nA, p, père, liste des configurations voisines) 
 * p est la position de la barque et vaut : 
 *    soit true pour I  (si la barque se situe sur la rive de l'ile)
 *    soit false pour D (si la barque se situe sur la rive de depart) 
 *)

type configuration = int * int * bool * pere * liste
and pere = Null | Pere of configuration 
and liste = Vide | Liste of (configuration list) ;; 
type tab = int array ;;

(* ============================================================================= *)
(*                                 UTILITAIRES                                   *)
(* ============================================================================= *)

(*
 *  exemple de numerotation pour n = 3, k = 2 et taille = 10
 *
 *  3     9  13       19
 *  2   5 8  12    15 18 
 *  1 4   7  11 14    17
 *  0     6  10       16
 *)

let get_num (conf : configuration) taille n = 
    let (nE, nA, p, _, _) = conf in 
        if (nE = 0) then 
            if p then nA else taille + nA
        else 
            if (nE = n) then
                if p then 2 * n + nA else taille + 2 * n + nA
            else if p then n + nA else taille + n + nA ;;

(*  
 * tab -> int -> bool
 * est_visite renvoit true si la configuration a déjà été visité, sinon false 
 *)

let est_visite (tab : tab) (conf : configuration) taille n =
    let i = get_num conf taille n in
        if tab.(i) = 1 then true else false 

exception PasdeVNV ;;

let rec liste_conf_voisines (tab : tab) (noeud_init : configuration) taille n k = 
    let (nE, nA, p, _, _) = noeud_init in
    let liste = ref [] in
    if p then ( for i = 0 to k do
                    for j = 0 to k do
                        let nE' = nE - i in 
                        let nA' = nA - j in
                        if (nE - nE') >= 0 && (nA - nA') >= 0 && ((nE - nE') + (nA - nA') <= k)
                            && (0 < (nE - nE') + (nA - nA')) && (nE' = 0 || nE' = nA' || nE' = n) 
                            && nE' >= 0  && nA' >= 0 
                            && (not (est_visite tab (nE', nA', false, Null, Vide) taille n))
                        then liste := (nE', nA', false, Null, Vide)::(!liste) 
                    done 
                done )
     else ( for i = 0 to k do
                for j = 0 to k do
                    let nE' = nE + i in 
                    let nA' = nA + j in
                    if (nE' - nE) >= 0 && (nA' - nA) >= 0 && ((nE' - nE) + (nA' - nA) <= k) 
                        && (0 < (nE' - nE) + (nA' - nA)) && (nE' = 0 || nE' = nA' || nE' = n) 
                        && nE' <= n  && nA' <= n 
                        && (not (est_visite tab (nE', nA', true, Null, Vide) taille n))
                    then liste := (nE', nA', true, Null, Vide)::(!liste)
                done 
            done ) ; !liste ;; 

(*
 * choisir_voisin_nonvisite renvoie une configuration voisine valide de conf
 *)

let choisir_voisin_nonvisite conf file tab taille n k : configuration =
    let (nE, nA, p, pere, lst) = conf in 
    let liste_voisins = ( match lst with
                          | Vide       -> liste_conf_voisines tab conf taille n k
                          | Liste(lst) -> lst ) in
    if liste_voisins = [] then raise PasdeVNV
    else 
        let nconf = (nE, nA, p, pere, Liste(List.tl liste_voisins)) in
        let _ = DeQueue.enleve_tete file in
        let _ = DeQueue.insere_tete nconf file in
            List.hd liste_voisins ;;
(*
 * get_chemin recupère le chemin de la configuration initiale à la configuration conf
 *)

let get_chemin = function
    | None -> failwith "problème dans get_chemin, pas supposé arrivé"
    | Some((nE, nA, p, pere, _) as c ) -> if p &&  pere = Null then failwith "il n'existe pas de chemin"
                                          else ( let rec get_chemin_aux (conf : configuration) aux =
                                                    let (nA, nE, p, pere, _) = conf in 
                                                        ( match pere with
                                                          | Null                               -> aux
                                                          | Pere((nEp, nAp, pp, _, _) as prec) -> 
                                                              ( get_chemin_aux prec ((nEp, nAp, pp)::aux)) )
                                                 in get_chemin_aux c [(nE, nA, p)] ) ;;

(* ============================================================================= *)
(*                              Utilitaires output                               *)
(* ============================================================================= *)

let output_int = function oc -> function x ->
    output_string oc (string_of_int x) ;;

let output_float oc f = output_string oc (string_of_float f) ;;

let affiche_conf conf =
    let (nA, nE, p) = conf in 
        Printf.printf "(%d, %d, %s) " nA nE ((fun p -> if p then "I" else "D") p) ;;

let message () = Printf.printf 
    "Entré incorrecte :\n\
    (1) -courbe <n>     : pour generer une courbe temps cpu en fonction de n (k est fixé à 4)\n\
    (2) -chemin <n> <p> : pour generer un chemin (k < 2n)\n" ;;

(* ============================================================================= *)
(*                          Fonction d'initialisations                           *)
(* ============================================================================= *)

(*
 * tab.(i) = 0 : configuration pas visité, sinon 1 
 *)

let init_tab t =
    Array.create (2 * t) 0 ;;

let initialisation () =
    let file_sommets = DeQueue.init () in
    let _ = (DeQueue.insere (0, 0, false, Null, Vide) file_sommets) in   
        file_sommets ;;

(* ============================================================================ *)
(*                                Algorithme                                    *)
(* ============================================================================ *)

(*
 * algorithme principal : le parcours en largeur
 *)

let rec traversee file_sommets tab conf_final taille n k =
    if (!conf_final <> None) || ((DeQueue.get_premier file_sommets) = DeQueue.FVide) 
    then ()(* fin *)
    else 
        let premier_sommet_file = (DeQueue.get_sommet_info file_sommets) in
        (try
            let x = (choisir_voisin_nonvisite premier_sommet_file file_sommets tab taille n k) in
            let (nE, nA, p, pere, l) = x in 
            (* on met le bit à 1 et on actualise le nouveau pere *)
            let nx = (nE, nA, p, Pere(premier_sommet_file), l) in
            let _ = if nE = nA && nE = n && p then conf_final := Some(nx) else () in
            let _ = tab.(get_num x taille n) <- 1 in
                (DeQueue.insere nx file_sommets)
        with
        | PasdeVNV -> (DeQueue.enleve_tete file_sommets) ) ;
        (traversee file_sommets tab conf_final taille n k) ;;
        
(* ============================================================================ *)
(*                                  main                                        *)
(* ============================================================================ *)

let () =
    let conf_final = ref None in 
    match Sys.argv.(1) with
    | "-courbe" -> if (Array.length Sys.argv) < 3 then message () else
                  (*
                   * Generation d'une courbe temps cpu en fonction de n , k est fixé à 4
                   *)
                  let oc = open_out "donnees.dat" in
                  let k = 4 in (* capacite de la barque *)
                      for i = 4 to (int_of_string Sys.argv.(2)) do
                          let _ = Printf.printf "Traversee n = %d\n" i in
                          let _ = flush stdout in
                          let file_sommets = initialisation () in
                          let n = i in (* n est le nombre d'explorateurs/adorateurs *)
                          let taille = (3 * (n + 1) - 2) in
                          let tab = init_tab taille in
                          let n1 = Sys.time () in
                          let _ = traversee file_sommets tab conf_final taille n k in
                          conf_final := None ;
                          let n2 = Sys.time () in
                          let temps_cpu = (n2 -. n1) in
                              begin 
                                  output_int oc i ;
                                  output_string oc " " ;
                                  output_float oc temps_cpu ;
                                  output_string oc "\n"
                               end
                      done ;
                      close_out oc ;
                      if Sys.command "gnuplot genere_cpu_courbe.gnu" = 0 then () 
                      else print_string "Erreur lors de la génération de la courbe.\n" ;
    | "-chemin" -> if (Array.length Sys.argv) < 4 then message () else 
                  (*
                   * Generation d'un chemin, avec n et k passés en parametre 
                   *)
                  let file_sommets = initialisation () in
                  let n = (int_of_string Sys.argv.(2)) in (* n est le nombre d'explorateurs/adorateurs *)
                  let k = (int_of_string Sys.argv.(3)) in (* capacite de la barque *) 
                  let taille = (3 * (n + 1) - 2) in
                  let tab = init_tab taille in
                  let _ = tab.(taille) <- 1 in
                  let _ = traversee file_sommets tab conf_final taille n k in 
                  let _ = List.iter affiche_conf (get_chemin !conf_final) in
                      print_string "\n"
    | _        -> message () ;; 
