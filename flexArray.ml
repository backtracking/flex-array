(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** A flexible array is a Braun tree (a cute data structure that can
   be used for other purposes, e.g. to implement priority queues).

*)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

type 'a t = {
  size: int;
  tree: 'a tree;
}

let empty =
  { size = 0; tree = Empty }

let length a =
  a.size

let rec get_tree t i = match t with
  | Empty -> assert false
  | Node (l, x, r) ->
    if i = 0 then x
    else if i mod 2 = 1 then get_tree l (i / 2) else get_tree r (i / 2 - 1)

let get a i =
  if i < 0 || i >= a.size then invalid_arg "get";
  get_tree a.tree i

let rec set_tree t i v = match t with
  | Empty -> assert false
  | Node (l, x, r) ->
    if i = 0 then Node (l, v, r)
    else if i mod 2 = 1 then Node (set_tree l (i / 2) v, x, r)
    else Node (l, x, set_tree r (i / 2 - 1) v)

let set a i v =
  if i < 0 || i >= a.size then invalid_arg "get";
  { a with tree = set_tree a.tree i v }

(* low extension *)
let rec cons_aux v = function
  | Empty -> Node (Empty, v, Empty)
  | Node (l, x, r) -> Node (cons_aux x r, v, l)

let cons v a =
  { size = a.size + 1; tree = cons_aux v a.tree }

(* low removal *)
let rec tail_aux = function
  | Empty -> assert false
  | Node (Empty, _, Empty) -> Empty
  | Node (l, _, r) -> Node (r, get_tree l 0, tail_aux l)

let tail a =
  if a.size = 0 then invalid_arg "tail";
  { size = a.size - 1; tree = tail_aux a.tree }

(* high extension *)
let rec snoc_aux s t v = match t with
  | Empty -> Node (Empty, v, Empty)
  | Node (l, x, r) -> if s mod 2 = 1 then Node (snoc_aux (s / 2) l v, x, r)
                                     else Node (l, x, snoc_aux (s / 2 - 1) r v)

let snoc a v =
  { size = a.size + 1; tree = snoc_aux a.size a.tree v }

(* high removal *)
let rec liat_aux s = function
  | Empty -> assert false
  | Node (Empty, _, Empty) -> Empty
  | Node (l, x, r) -> if s mod 2 = 0 then Node (liat_aux (s / 2) l, x, r)
                                     else Node (l, x, liat_aux (s / 2) r)

let liat a =
  if a.size = 0 then invalid_arg "liat";
  { size = a.size - 1; tree = liat_aux a.size a.tree }

let iter f a =
  let add t q = if t <> Empty then Queue.add t q in
  let rec loop n current (left, right as next) =
    if n = 0 then
      assert (Queue.is_empty current &&
              Queue.is_empty left && Queue.is_empty right)
    else if Queue.is_empty current then begin
      Queue.transfer right left;
      loop n left (current, right)
    end else begin match Queue.pop current with
      | Empty ->
          assert false
      | Node (l, x, r) ->
          f x; add l left; add r right; loop (n - 1) current next
    end in
  if a.size > 0 then begin
    let start = Queue.create () in
    Queue.add a.tree start;
    loop a.size start (Queue.create (), Queue.create ())
  end

