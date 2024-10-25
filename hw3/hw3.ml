type ichar = char * int
type regexp =
| Epsilon
| Character of ichar
| Union of regexp * regexp
| Concat of regexp * regexp
| Star of regexp

module Cset = Set.Make(struct type t = ichar let compare = Stdlib.compare end)

let null (r: regexp) : bool =
  let rec null_rec r =
    match r with
    | Epsilon -> true
    | Character _ -> false
    | Union (r1, r2) -> null_rec r1 || null_rec r2
    | Concat (r1, r2) -> null_rec r1 && null_rec r2
    | Star _ -> true
  in
  null_rec r

let first (r: regexp) : Cset.t = 
  let rec first_rec r =
    match r with
    | Epsilon -> Cset.empty
    | Character c -> Cset.singleton c
    | Union (r1, r2) -> Cset.union (first_rec r1) (first_rec r2)
    | Concat (r1, r2) -> if null r1 then Cset.union (first_rec r1) (first_rec r2) else first_rec r1
    | Star r -> first_rec r
  in
  first_rec r

let last (r: regexp) : Cset.t = 
  let rec last_rec r = 
    match r with
    | Epsilon -> Cset.empty
    | Character c -> Cset.singleton c
    | Union (r1, r2) -> Cset.union (last_rec r1) (last_rec r2)
    | Concat (r1, r2) -> if null r2 then Cset.union (last_rec r1) (last_rec r2) else last_rec r2
    | Star r -> last_rec r
  in
  last_rec r

let follow (c: ichar) (r: regexp) : Cset.t = 
  let rec follow_rec r = 
    match r with
    | Epsilon -> Cset.empty
    | Character ch -> Cset.empty 
    | Union (r1, r2) -> Cset.union (follow_rec r1) (follow_rec r2)
    | Concat (r1, r2) -> 
      if Cset.mem c (last r1) 
        then Cset.union (Cset.union (follow_rec r2) (follow_rec r1)) (first r2) 
        else Cset.union (follow_rec r1) (follow_rec r2)
    | Star r -> 
      if Cset.mem c (last r) 
        then Cset.union (first r) (follow_rec r) 
        else follow_rec r
  in
  follow_rec r

let () =
  (* ex1 *)
  let a = Character ('a', 0) in
  assert (not (null a));
  assert (null (Star a));
  assert (null (Concat (Epsilon, Star Epsilon)));
  assert (null (Union (Epsilon, a)));
  assert (not (null (Concat (a, Star a))));
  Printf.printf "ex1 passed\n";
  (* ex2 *)
  let ca = ('a', 0) and cb = ('b', 0) in
  let a = Character ca and b = Character cb in
  let ab = Concat (a, b) in
  let eq = Cset.equal in
  assert (eq (first a) (Cset.singleton ca));
  assert (eq (first ab) (Cset.singleton ca));
  assert (eq (first (Star ab)) (Cset.singleton ca));
  assert (eq (last b) (Cset.singleton cb));
  assert (eq (last ab) (Cset.singleton cb));
  assert (Cset.cardinal (first (Union (a, b))) = 2);
  assert (Cset.cardinal (first (Concat (Star a, b))) = 2);
  assert (Cset.cardinal (last (Concat (a, Star b))) = 2);
  Printf.printf "ex2 passed\n";
  (* ex3 *)
  let ca = ('a', 0) and cb = ('b', 0) in
  let a = Character ca and b = Character cb in
  let ab = Concat (a, b) in
  assert (Cset.equal (follow ca ab) (Cset.singleton cb));
  assert (Cset.is_empty (follow cb ab));
  let r = Star (Union (a, b)) in
  assert (Cset.cardinal (follow ca r) = 2);
  assert (Cset.cardinal (follow cb r) = 2);
  let r2 = Star (Concat (a, Star b)) in
  assert (Cset.cardinal (follow cb r2) = 2);
  let r3 = Concat (Star a, b) in
  assert (Cset.cardinal (follow ca r3) = 2);
  Printf.printf "ex3 passed\n";
