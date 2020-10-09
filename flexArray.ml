
(** Flexible arrays

    Flexible arrays are arrays whose size can be changed by adding or
    removing elements at either end (one at a time).

    This is an implementation of flexible arrays using Braun trees,
    following

      Rob Hoogerwoord
      A logarithmic implementation of flexible arrays
      http://alexandria.tue.nl/repository/notdare/772185.pdf

    All operations (get, set, le, lr, he, hr) have logarithmic complexity.

    Note: Braun trees can also be used to implement priority queues.
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

let is_empty a =
  a.size = 0

let rec get_tree t i = match t with
  | Empty -> invalid_arg "get"
  | Node (l, x, r) ->
    if i = 0 then x
    else if i mod 2 = 1 then get_tree l (i / 2) else get_tree r (i / 2 - 1)

let get a i =
  get_tree a.tree i

let rec set_tree t i v = match t with
  | Empty -> invalid_arg "set"
  | Node (l, x, r) ->
    if i = 0 then Node (l, v, r)
    else if i mod 2 = 1 then Node (set_tree l (i / 2) v, x, r)
       else Node (l, x, set_tree r (i / 2 - 1) v)

let set a i v =
  { a with tree = set_tree a.tree i v }

(* low extension *)
let rec le_aux v = function
  | Empty -> Node (Empty, v, Empty)
  | Node (l, x, r) -> Node (le_aux x r, v, l)

let le v a =
  { size = a.size + 1; tree = le_aux v a.tree }

(* low removal *)
let rec lr_aux = function
  | Empty -> invalid_arg "lr"
  | Node (Empty, _, Empty) -> Empty
  | Node (l, _, r) -> Node (r, get_tree l 0, lr_aux l)

let lr a =
  { size = a.size - 1; tree = lr_aux a.tree }

(* high extension *)
let rec he_aux s t v = match t with
  | Empty -> Node (Empty, v, Empty)
  | Node (l, x, r) -> if s mod 2 = 1 then Node (he_aux (s / 2) l v, x, r)
                                     else Node (l, x, he_aux (s / 2 - 1) r v)

let he a v =
  { size = a.size + 1; tree = he_aux a.size a.tree v }

(* high removal *)
let rec hr_aux s = function
  | Empty -> invalid_arg "hr"
  | Node (Empty, _, Empty) -> Empty
  | Node (l, x, r) -> if s mod 2 = 0 then Node (hr_aux (s / 2) l, x, r)
                                   else Node (l, x, hr_aux (s / 2) r)

let hr a =
  { size = a.size - 1; tree = hr_aux a.size a.tree }

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

