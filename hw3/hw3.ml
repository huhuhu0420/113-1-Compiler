type ichar = char * int
type regexp =
| Epsilon
| Character of ichar
| Union of regexp * regexp
| Concat of regexp * regexp
| Star of regexp

let null r =
  let rec null' r =
    match r with
    | Epsilon -> true
    | Character _ -> false
    | Union (r1, r2) -> null' r1 || null' r2
    | Concat (r1, r2) -> null' r1 && null' r2
    | Star _ -> true
  in
  null' r

let () =
  let a = Character ('a', 0) in
  assert (not (null a));
  assert (null (Star a));
  assert (null (Concat (Epsilon, Star Epsilon)));
  assert (null (Union (Epsilon, a)));
  assert (not (null (Concat (a, Star a))));
