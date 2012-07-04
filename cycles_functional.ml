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

module SV = Set.Make(G.V)

let to_set l = List.fold_right SV.add l SV.empty ;;

let partition s w = fst(SV.partition (fun e -> e >= w) s);;

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

let rec unblock t n =
  if Hashtbl.find t.blocked n then begin
    Hashtbl.replace t.blocked n false;
    List.iter (unblock t) (Hashtbl.find t.notelem n);
    Hashtbl.replace t.notelem n [];
  end
;;

let block t n =
  Hashtbl.replace t.blocked n true
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
        if G.V.equal nextnode startnode then begin
          (true, (stack_to_list path)::r)
        end else begin
          if not(Hashtbl.find t.blocked nextnode) then begin
            let c2, r2 = circuit t r nextnode startnode component in
            (c || c2, r2)
          end else
            (c,r)
        end
      ) component thisnode (false,result)
    in
    if closed then begin
      unblock t thisnode
    end else
      G.iter_succ (fun nextnode ->
        let l = Hashtbl.find t.notelem nextnode in
        if not(List.mem thisnode l) then
          Hashtbl.replace t.notelem nextnode (thisnode::l)
      ) component thisnode;
    ignore(Stack.pop path);
    (closed, result)
  in

 (* Johnson's algorithm requires some ordering of the nodes. *)
  let vertex_set = G.fold_vertex SV.add g SV.empty in
  let result = SV.fold (fun s result ->
    (* Build the subgraph induced by s and following nodes in the ordering *)
    let subset = SV.add s (partition vertex_set s) in
    let subgraph = extract_subgraph g subset in

    (* Find the strongly connected component in the subgraph
     * that contains the least node according to the ordering *)
    let scc = G.Components.scc_list subgraph in
    let minnode = SV.min_elt subset in
    let mincomp = List.find (fun l -> List.mem minnode l) scc in

    (* smallest node in the component according to the ordering *)
    let component = extract_subgraph subgraph (to_set mincomp) in

    if G.nb_edges component > 0 then begin
      (* init the block table for this component *)
      let t = init_block component in

      snd(circuit t result minnode minnode component);
    end else
      result
  ) vertex_set []
  in
  List.rev result
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
