
open Graph
open ExtLib
open ExtString

module G = Pack.Digraph

module SV = Set.Make(G.V)

let to_set l = List.fold_right SV.add l SV.empty ;;

let partition s w = snd(SV.partition (fun e -> e >= w) s);;

let print_set s = 
  String.join " " (List.map (fun e ->
    string_of_int (G.V.label e)
    ) (SV.elements s))
;;

let extract_subgraph g s =
  let sg = G.create () in
  G.iter_edges (fun v1 v2 ->
    if SV.mem v1 s then G.add_vertex sg v1;
    if SV.mem v2 s then G.add_vertex sg v2;
    if SV.mem v1 s && SV.mem v2 s then
      G.add_edge sg v1 v2
  ) g;
  sg
;;

let stack_to_list s = 
  let l = ref [] in
  Stack.iter (fun e -> l:= e::!l) s;
  !l
;;

type block = {
  blocked : (G.V.t,bool) Hashtbl.t;
  notelem : (G.V.t,G.V.t list) Hashtbl.t
}

let init_block g =
  let t = {
    blocked = Hashtbl.create 1023;
    notelem = Hashtbl.create 1023;
  } in
  G.iter_vertex (fun node ->
    Hashtbl.add t.blocked node false;
    Hashtbl.add t.notelem node [];
  ) g;
  t
;;

let get_notelem t n =
  Hashtbl.find t.notelem n
;;

let rec unblock t n =
  Printf.eprintf "unblock %d\n" (G.V.label n);
  if Hashtbl.find t.blocked n then begin
    Hashtbl.replace t.blocked n false;
    let l = get_notelem t n in
    List.iter (unblock t) l;
    Hashtbl.replace t.notelem n []
  end
;;

let block t n =
  Hashtbl.replace t.blocked n true
;;

let is_bloked t n =
  Hashtbl.find t.blocked n
;;

let find_all_cycles_johnson g =
  if not G.is_directed then
    assert false;

  (*  stack of nodes in current path *)
  let path = Stack.create () in

  let rec circuit t result thisnode startnode component = 

    Stack.push thisnode path;
    block t thisnode;

    let (closed,result) = 
      G.fold_succ (fun nextnode (c,r) ->
        Printf.eprintf "startnode %d\n" (G.V.label startnode);
        Printf.eprintf "nextnode %d\n" (G.V.label nextnode);
        if G.V.equal nextnode startnode then begin
          Printf.eprintf "closed = true 1\n";
          (true,(Stack.copy path)::r)
        end else begin 
          if not(is_bloked t nextnode) then
            circuit t r nextnode startnode component 
          else
            (c,r)
        end 
      ) component thisnode (false,result)
    in
    if closed then begin
      Printf.eprintf "closed = true 3\n";
      unblock t thisnode
    end else
      G.iter_succ (fun nextnode ->
        let l = get_notelem t nextnode in
        if List.mem thisnode l then
          Hashtbl.replace t.notelem nextnode (thisnode::l)
      ) component thisnode;

    ignore(Stack.pop path);
    (closed,result)
  in

 (* Johnson's algorithm requires some ordering of the nodes. *)
  let vertex_set = G.fold_vertex SV.add g SV.empty in
  Printf.eprintf "inital vertex set %s\n" (print_set vertex_set);
  SV.fold (fun s result ->
    (* Build the subgraph induced by s and following nodes in the ordering *)
    Printf.eprintf "selected element %d\n" (G.V.label s);
    let subset = SV.add s (partition vertex_set s) in
    Printf.eprintf "subset %s\n" (print_set subset);
    let subgraph = extract_subgraph g subset in

    if G.nb_edges subgraph > 0 then begin
      (* Find the strongly connected component in the subgraph
       * that contains the least node according to the ordering *)
      let scc = G.Components.scc_list subgraph in
      let minnode = SV.min_elt subset in
      Printf.eprintf "minnode %d\n" (G.V.label minnode);
      let mincomp = List.find (fun l -> List.mem minnode l) scc in
      Printf.eprintf "mincomp %s\n" (print_set ((to_set mincomp)));

      (* smallest node in the component according to the ordering *)
      let startnode = minnode in
      let component = extract_subgraph subgraph (to_set mincomp) in

      G.dot_output component (Printf.sprintf "test-component%d.dot" (G.V.label s));

      (* init the block table for this component *)
      let t = init_block component in

      snd(circuit t result startnode startnode component);
    end else begin
      Printf.eprintf "No edges to consider\n";
      result
    end
  ) vertex_set []
;;

let g = G.Rand.graph ~v:5 ~e:10 () in
G.dot_output g "test.dot";
let ll = find_all_cycles_johnson g in
List.iter (fun path ->
  let path = stack_to_list path in
  Printf.printf "path : %s\n" 
  (String.join " " (List.map (fun e -> string_of_int (G.V.label e)) path))
) ll
