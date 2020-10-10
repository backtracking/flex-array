
open Flex_array

let eqint = Alcotest.(check int)

let pp fmt a =
  Format.fprintf fmt "@[[";
  for i = 0 to length a - 1 do
    Format.fprintf fmt "%d" (get a i);
    if i < length a - 1 then Format.fprintf fmt ",@ "
  done;
  Format.fprintf fmt "@]]"

let fa : int t Alcotest.testable = Alcotest.testable pp (=)
let eq_fa = Alcotest.(check fa)

let test1 m =
  let rec fill a i = if i = m then a else fill (snoc a i) (i + 1) in
  let a = fill empty 0 in
  eqint "length" m (length a);
  let a = cons 0 a in
  eqint "length cons" (m + 1) (length a);
  let a = tail a in
  eqint "length tail" m (length a);
  let a = snoc a m in
  eqint "length snoc" (m + 1) (length a);
  let a = liat a in
  eqint "length snoc" m (length a);
  for i = 0 to m - 1 do eqint "get" i (get a i) done;
  let next = ref 0 in
  (iter (fun j -> eqint "iter" !next j; incr next)) a

let test1 () =
  for m = 0 to 42 do test1 m done

let test2 () =
  let a = make 0 42 in
  eqint "length make" 0 (length a);
  let a = make 1729 42 in
  eqint "length make" 1729 (length a);
  for i = 0 to 1728 do eqint "get" 42 (get a i) done;
  let b = cons 1 (snoc a 2) in
  eqint "length cons snoc" 1731 (length b);
  eqint "get cons" 1 (get b 0);
  eqint "get snoc" 2 (get b 1730);
  eq_fa "liat tail" (liat (tail b)) a;
  ()

let test3 () =
  let n = 1_000_000 in
  let rec loop a i = if i = n then a else loop (set a i i) (i + 1) in
  let a = loop (make n 0) 0 in
  eqint "length" n (length a);
  let s = ref 0 in
  iter (fun x -> s := !s + x) a;
  eqint "sum" (n*(n-1)/2) !s

let () =
  Alcotest.run "Flex_array"
    ["quick tests",
      [Alcotest.test_case "test1" `Quick test1;
       Alcotest.test_case "test2" `Quick test2;];
     "long tests",
      [Alcotest.test_case "test3" `Slow test3;];
    ]
