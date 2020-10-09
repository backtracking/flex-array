
open FlexArray

let () =
  let test m =
    let rec fill a i = if i = m then a else fill (he a i) (i + 1) in
    let a = fill empty 0 in
    assert (length a = m);
    let a = le 0 a  in
    assert (length a = m + 1);
    let a = lr a in
    assert (length a = m);
    let a = he a m in
    assert (length a = m + 1);
    let a = hr a in
    assert (length a = m);
    for i = 0 to m - 1 do assert (get a i = i) done;
    let next = ref 0 in
    (iter (fun j -> assert (j = !next); incr next)) a;
  in
  for m = 0 to 257 do test m done
  (* test 1000000 *)

