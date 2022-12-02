open Graph
(*type de chemin liste de couple de nodes*)
type chemin= (id*id) list 

type flot={
  actuel:int;
  capacite:int;
} 

type 'a reseau_flot = {
  gr: 'a graph;
  source : id;(*id ou int *)
  puit : id;
} 

(* etapes : 
    1- transformer les labels en flot et mettre la capacité a 0
    2- on cherche un chemin augmentant dans un reseau residuel
    3- on augmente les flots au maximum sur les arcs de ce chemin 
    4- on met a jour le graph residuel
    on s'arrete quand il n'ya plus de chemins augmentant 
   fonctionnement: 
   - on convertit le graph int en flot graph 
   - on boucle avec le graph residuel 
   - on trouve les  chemins avec un match sur le chemin qui affiche le gr quand plus de chemin 
   - si il trouve des chemins : trouve la capacité min / et on augmente 
   - on retourne ensuite le nouveau graph residuel
*)

(*fonction qui permet de prendre un graph int et d'y appliquer le principe des flots et mettre a 0 la capcite*)

val int_to_flot : int graph -> flot graph

val flot_to_string : flot graph -> string graph

(*ajouter un arc dans le graph residuel quand sa valeur dif de 0*)

val add_arc_res : int graph -> id -> id ->int -> int graph

(*fonction pour trouver un chemin dans le graph residuel *) (*pas mieux de creer une struct qui contient graph dest et orig?*)

val trouver_chemin :  int graph -> id -> id-> chemin option 

(*fonction pour trouver la plus petite capacité du chemin*)

val cap_min : int graph -> chemin -> int 

(*fonction pour construire un graph residuel*)

val construire_res : flot graph ->int graph

(*augmentation des capacité au maximum sur le graph flot*)

val flot_update : flot graph -> chemin ->int -> flot graph

