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

(** Flexible arrays

    Flexible arrays are arrays whose size can be changed by adding or
    removing elements at either end (one at a time).

    This is an implementation of flexible arrays using Braun trees,
    following

      Rob Hoogerwoord
      A logarithmic implementation of flexible arrays
      http://alexandria.tue.nl/repository/notdare/772185.pdf

    All operations (get, set, cons, tail, snoc, liat) have logarithmic
    complexity.
*)

type 'a t
(** This is an immutable data structure. *)

val empty: 'a t

val length: 'a t ->  int

val get: 'a t -> int -> 'a

val set: 'a t -> int -> 'a -> 'a t

val cons: 'a -> 'a t -> 'a t

val tail: 'a t -> 'a t

val snoc: 'a t -> 'a -> 'a t

val liat: 'a t -> 'a t

val iter: ('a -> unit) -> 'a t -> unit

