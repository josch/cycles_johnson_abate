(**************************************************************************)
(*  Copyright (C) 2012 Pietro Abate <pietro.abate@pps.jussieu.fr>         *)
(*  Copyright (C) 2012 Johannes Schauer <j.schauer@email.de>              *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.                       *)
(**************************************************************************)

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

  let rec unblock n =
    if Hashtbl.find blocked n then begin
      Hashtbl.replace blocked n false;
      List.iter unblock (Hashtbl.find b n);
      Hashtbl.replace b n [];
    end
  in

  let stack_to_list s = 
    let l = ref [] in
    Stack.iter (fun e -> l:= e::!l) s;
    !l
  in

  let rec circuit thisnode startnode component = 
    let closed = ref false in
    Stack.push thisnode path;
    Hashtbl.replace blocked thisnode true;
    G.iter_succ (fun nextnode ->
      if G.V.equal nextnode startnode then begin
        result := ((stack_to_list path))::!result;
        closed := true;
      end else begin if not(Hashtbl.find blocked nextnode) then
        if circuit nextnode startnode component then begin
          closed := true;
        end
      end
    ) component thisnode;
    if !closed then begin
      unblock thisnode
    end
    else
      G.iter_succ (fun nextnode ->
        let l = Hashtbl.find b nextnode in
        if not(List.mem thisnode l) then
          Hashtbl.replace b nextnode (thisnode::l)
      ) component thisnode;
    ignore(Stack.pop path);
    !closed
  in
  let module SV = Set.Make(G.V) in
  let extract_subgraph g s =
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
  let part s w = fst(SV.partition (fun e -> e >= w) s) in
  SV.iter (fun s ->
    (* Build the subgraph induced by s and following nodes in the ordering *)
    let subset = SV.add s (part vertex_set s) in
    let subgraph = extract_subgraph g subset in
    let scc = G.Components.scc_list subgraph in
    (* Find the strongly connected component in the subgraph
     * that contains the least node according to the ordering *)
    let minnode = SV.min_elt subset in
    let mincomp = List.find (fun l -> List.mem minnode l) scc in
    (* smallest node in the component according to the ordering *)
    let component = extract_subgraph subgraph (to_set mincomp) in
    if G.nb_edges component > 0 then begin
      G.iter_vertex (fun node ->
        Hashtbl.replace blocked node false;
        Hashtbl.replace b node [];
      ) component;
      ignore(circuit minnode minnode component);
    end
  ) vertex_set;

  List.rev !result
;;

if Array.length Sys.argv < 3 then begin
  Printf.printf "usage: %s num_vertices [v1,v2...]\n" Sys.argv.(0);
  exit 1;
end;

let v = int_of_string (Sys.argv.(1)) in
let g = G.create ~size:v () in

let a = Array.init v G.V.create in

for i = 2 to Array.length Sys.argv - 1 do
  let v1, v2 = String.split Sys.argv.(i) "," in
  G.add_edge g a.(int_of_string v1) a.(int_of_string v2);
done;

let ll = find_all_cycles_johnson g in
List.iter (fun path ->
  Printf.printf "%s\n" 
  (String.join " " (List.map (fun e -> string_of_int (G.V.label e)) path))
) ll
