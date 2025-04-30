
(* Build all the rotations of a string, e.g.

      abcd
      bcda
      cdab
      dabc

  simultaneously in memory.

  Time and space O(n log n), where n is the length of the string. *)

open Format
open Flex_array

let n = int_of_string Sys.argv.(1)

let a = init n (fun _ -> Char.chr (32 + Random.int 95))

let r = Array.make n a
let () = for i = 0 to n - 2 do r.(i+1) <- snoc (tail r.(i)) (get r.(i) 0) done

let () = if n < 80 then Array.iteri (fun i a ->
  printf "%2d: " i; iter (fun c -> printf "%c" c) a; printf "@.") r

let () =
  Gc.(let st = stat () in
      let m = float (st.top_heap_words lsl 3) /. 1e6 in
      printf "top heap = %.2f Mb@." m)

(* space ~ n log n kb *)
