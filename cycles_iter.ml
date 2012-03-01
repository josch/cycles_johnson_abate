
open Graph
open ExtLib
open ExtString

module G = Pack.Digraph

let find_all_cycles_johnson g =
  if not G.is_directed then
    assert false;
    (*  stack of nodes in current path *)
  let path = Stack.create () in
    (* vertex: blocked from search *)
  let blocked = Hashtbl.create 1023 in
    (* graph portions that yield no elementary circuit *)
  let b = Hashtbl.create 1023 in
    (* list to accumulate the circuits found  *)
  let result = ref [] in

  let rec circuit thisnode startnode component = 
    let stack_to_list s = 
      let l = ref [] in
      Stack.iter (fun e -> l:= e::!l) s;
      !l
    in

    let rec unblock thisnode =
      Printf.eprintf "unblock %d\n" (G.V.label thisnode);
      if Hashtbl.find blocked thisnode then begin
        Hashtbl.replace blocked thisnode false;
        List.iter unblock (Hashtbl.find b thisnode);
        Hashtbl.replace b thisnode []
      end
    in
    let closed = ref false in
    Stack.push thisnode path;
    Hashtbl.replace blocked thisnode true;
    G.iter_succ (fun nextnode ->
      Printf.eprintf "startnode %d\n" (G.V.label startnode);
      Printf.eprintf "nextnode %d\n" (G.V.label nextnode);
      if G.V.equal nextnode startnode then begin
        result := ((stack_to_list path))::!result;
        closed := true;
        Printf.eprintf "closed = true 1\n";
      end else begin if not(Hashtbl.find blocked nextnode) then
        if circuit nextnode startnode component then begin
          closed := true;
          Printf.eprintf "closed = true 2\n";
        end
      end
    ) component thisnode;
    if !closed then begin
      Printf.eprintf "closed = true 3\n";
      unblock thisnode
    end
    else
      G.iter_succ (fun nextnode ->
        if List.mem thisnode (Hashtbl.find b nextnode) then
          Hashtbl.replace b nextnode (thisnode::(Hashtbl.find b nextnode))
      ) component thisnode;
    ignore(Stack.pop path);
    !closed
  in
  let module SV = Set.Make(G.V) in
  let subgraph_ g s =
    let sg = G.create () in
    G.iter_edges (fun v1 v2 ->
      if SV.mem v1 s then G.add_vertex sg v1;
      if SV.mem v2 s then G.add_vertex sg v2;
      if SV.mem v1 s && SV.mem v2 s then
        G.add_edge sg v1 v2
    ) g;
    sg
  in
 (* Johnson's algorithm requires some ordering of the nodes. 
  * They might not be sortable so we assign an arbitrary ordering. 
  *)
  let to_set l = List.fold_right SV.add l SV.empty in
  let vertex_set = G.fold_vertex SV.add g SV.empty in
  let part s w = snd(SV.partition (fun e -> e >= w) s) in
  let print_set s = 
    String.join " " (List.map (fun e ->
      string_of_int (G.V.label e)
      ) (SV.elements s))
  in
  Printf.eprintf "inital set %s\n" (print_set vertex_set);
  SV.iter (fun s ->
    (* Build the subgraph induced by s and following nodes in the ordering *)
    Printf.eprintf "selected element %d\n" (G.V.label s);
    let subset = SV.add s (part vertex_set s) in
    Printf.eprintf "subset %s\n" (print_set subset);
    let subgraph = subgraph_ g subset in
    if G.nb_edges subgraph > 0 then begin
      let scc = G.Components.scc_list subgraph in
      (* Find the strongly connected component in the subgraph
       * that contains the least node according to the ordering *)
      let minnode = SV.min_elt subset in
      Printf.eprintf "minnode %d\n" (G.V.label minnode);
      let mincomp = List.find (fun l -> List.mem minnode l) scc in
      Printf.eprintf "mincomp %s\n" (print_set ((to_set mincomp)));
      (* smallest node in the component according to the ordering *)
      let startnode = minnode in
      let component = subgraph_ subgraph (to_set mincomp) in
      G.dot_output component (Printf.sprintf "test-component%d.dot" (G.V.label s));
      G.iter_vertex (fun node ->
        Hashtbl.add blocked node false;
        Hashtbl.add b node [];
      ) component;
      ignore(circuit startnode startnode component);
    end else
      Printf.eprintf "No edges to consider\n"
  ) vertex_set;

  !result
;;

let g = G.Rand.graph ~v:5 ~e:10 () in
G.dot_output g "test.dot";
let ll = find_all_cycles_johnson g in
List.iter (fun path ->
  Printf.printf "path : %s\n" 
  (String.join " " (List.map (fun e -> string_of_int (G.V.label e)) path))
) ll
