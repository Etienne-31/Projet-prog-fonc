open Graph 
open Tools 

type chemin= (id*id)list

type flot={
  actuel:int;
  capacite:int;
} 

type 'a reseau_flot = {
  gr: 'a graph;
  source : id;(*id ou int *)
  puit : id;
} 


let int_to_flot gr = gmap gr (fun c-> {actuel=0; capacite=c})
let flot_to_string gr = gmap gr (fun f -> (string_of_int f.actuel)^"/"^(string_of_int f.capacite))
let add_arc_res gr origine dest = function  
  |0->gr 
  |nb-> new_arc gr origine dest nb 

let construire_res gr = 
  let graph_res_vide = clone_nodes gr in 
  e_fold gr (fun gra o d flot -> 
      let fdroite = flot.capacite - flot.actuel in 
      let fgauche = flot.actuel in 
      let nv_graph = add_arc_res gra o d fdroite in 
      add_arc_res nv_graph d o fgauche) graph_res_vide

let cap_min gr chemin = List.fold_left (fun min_cap (o ,d) ->
    match (find_arc gr o d) with
    |None->raise Not_found 
    |Some x -> min x min_cap) 10000 chemin

let flot_update gr chemin nb = List.fold_left (fun graph (o,d)->
    match (find_arc graph o d) with 
    |None->raise Not_found 
    |Some f -> new_arc graph o d {actuel = f.actuel+nb; capacite = f.capacite }) 
    gr chemin 

(*
arc(x, y).

chemin(x, y) :- arc(x, a), chemin(a, y).
chemin(x, y) 

let rec trouver_chemin gr orig dest = 

  e_fold gr (fun graph o d-> 
  match (find_arc gr orig d) with
  |None->raise Not_found 
  |Some f -> trouver_chemin gr d dest) []



**)



let rec trouver_chemin gr orig dest = 
  (*   renvoie chemin de orig à dest   ou None si pas de chemmin*)
  let rec loop gr orig dest acu =
    (*let rec loop acu graph o d flot = *)
    (Printf.printf "loop %d\n" orig ;
     match (out_arcs gr orig) with 
     |[]-> (Printf.printf "  None\n "; None )
     |(d,flot)::rest-> if d=dest then Some (List.rev ((orig,dest)::acu)) else 
         (Printf.printf "  -> %d\n" d ;
          match(loop gr d dest ((orig,d)::acu)) with 
          |None -> None
          |Some chemindepuisd -> Some (chemindepuisd)))

  in loop gr orig dest []





