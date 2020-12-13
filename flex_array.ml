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

    This is an implementation of flexible arrays using Braun trees,
    following

      Rob Hoogerwoord
      A logarithmic implementation of flexible arrays
      http://alexandria.tue.nl/repository/notdare/772185.pdf

    See also

      Three Algorithms on Braun Trees (Functional Pearl)
      Chris Okasaki
      J. Functional Programming 7 (6) 661–666, November 1997
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

(* returns (l,r) with l of size n+1 and r of size n *)
let rec make_tree2 n v =
  if n = 0 then
    Node (Empty, v, Empty), Empty
  else if n mod 2 = 1 then
    let l, r = make_tree2 (n / 2) v in
    Node (l, v, r), Node (r, v, r)
  else
    let l, r = make_tree2 (n / 2 - 1) v in
    Node (l, v, l), Node (l, v, r)

let make n v =
  if n < 0 then invalid_arg "make";
  { size = n; tree = snd (make_tree2 n v) }

let rec init_tree n f a b = (* flexible array for f(ai+b) *)
  if n = 0 then
    Empty
  else
    let r = (n - 1) / 2 in
    let l = n - 1 - r in
    Node (init_tree l f (2*a) (a+b),
          f b,
          init_tree r f (2*a) (2*a+b))

let init n f =
  { size = n; tree = init_tree n f 1 0 }

let of_array a =
  init (Array.length a) (Array.get a)

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
  if i < 0 || i >= a.size then invalid_arg "set";
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

let map f a =
  let rec map = function
    | Empty -> Empty
    | Node (l, x, r) -> Node (map l, f x, map r) in
  { a with tree = map a.tree }


let foldi f acc a =
  let add t q = if t <> Empty then Queue.add t q in
  let rec loop acc i current (left, right as next) =
    if i = a.size then begin
      assert (Queue.is_empty current &&
              Queue.is_empty left && Queue.is_empty right);
      acc
    end else if Queue.is_empty current then begin
      Queue.transfer right left;
      loop acc i left (current, right)
    end else begin match Queue.pop current with
      | Empty ->
          assert false
      | Node (l, x, r) ->
          let acc = f acc i x in
          add l left;
          add r right;
          loop acc (i + 1) current next
    end in
  if a.size > 0 then begin
    let start = Queue.create () in
    Queue.add a.tree start;
    loop acc 0 start (Queue.create (), Queue.create ())
  end else
    acc

let iteri f a =
  foldi (fun () i x -> f i x) () a

let iter f a =
  foldi (fun () _ x -> f x) () a

let fold f acc a =
  foldi (fun acc _ x -> f acc x) acc a

