open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = n_fold gr (fun acu id -> new_node acu id ) empty_graph



let gmap gr f = e_fold gr (fun acu id1 id2 l-> (new_arc acu id1 id2 (f l)))  (clone_nodes gr)
let add_arc gr id1 id2 nb = 
  let result = find_arc gr id1 id2 in 
  match result with 
  |None-> new_arc gr id1 id2 nb
  |Some x-> new_arc gr id1 id2 (nb+x)  