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

(* define a type for automata *)
type state = Cset.t (* a state is a set of characters *)
module Cmap = Map.Make(Char) (* dictionary whose keys are characters *)
module Smap = Map.Make(Cset) (* dictionary whose keys are states *)
type autom = {
  start : state;
  trans : state Cmap.t Smap.t (* state dictionary -> (character dictionary -> state) *)
}
let eof = ('#', -1)

(* visualize an automaton *)
let fprint_state fmt q =
Cset.iter (fun (c,i) ->
if c = '#' then Format.fprintf fmt "# " else Format.fprintf fmt "%c%i " c i) q
let fprint_transition fmt q c q' =
  Format.fprintf fmt "\"%a\" -> \"%a\" [label=\"%c\"];@\n"
  fprint_state q
  fprint_state q'
  c
let fprint_autom fmt a =
  Format.fprintf fmt "digraph A {@\n";
  Format.fprintf fmt " @[\"%a\" [ shape = \"rect\"];@\n" fprint_state a.start;
  Smap.iter (fun q t -> Cmap.iter (fun c q' -> fprint_transition fmt q c q') t)
  a.trans;
  Format.fprintf fmt "@]@\n}@."
let save_autom file a =
  let ch = open_out file in
  Format.fprintf (Format.formatter_of_out_channel ch) "%a" fprint_autom a;
  close_out ch

(* Define a module for sets of characters *)
module CharSet = Set.Make(Char)

(* Function to extract all characters from the regular expression *)
let chars (r: regexp) : CharSet.t =
  let rec chars_rec r =
    match r with
    | Epsilon -> CharSet.empty
    | Character (c, _) -> CharSet.singleton c
    | Union (r1, r2) -> CharSet.union (chars_rec r1) (chars_rec r2)
    | Concat (r1, r2) -> CharSet.union (chars_rec r1) (chars_rec r2)
    | Star r1 -> chars_rec r1
  in
  chars_rec r

let next_state (r: regexp) (state: Cset.t) (c: char) : Cset.t = 
  Cset.fold (fun ci acc ->
    if fst ci = c then
      Cset.union acc (follow ci r)
    else
      acc
  ) state Cset.empty

let make_dfa (r: regexp) : autom =
  let r = Concat (r, Character eof) in
  let sigma = chars r in  (* The set of characters in the regex *)
  (* transitions under construction *)
  let trans = ref Smap.empty in
  let rec transitions q =
(* the transitions function constructs all the transitions of the state q,
if this is the first time q is visited *)
    if Smap.mem q !trans then
      ()  (* State q has already been processed *)
    else begin
      (* Build the transition map for state q *)
      let trans_q = CharSet.fold (fun c cmap ->
        let q' = next_state r q c in
        if not (Cset.is_empty q') then begin
          (* transitions q';  Recursively process q' *)
          Cmap.add c q' cmap  (* Add transition from q to q' on c *)
        end else
          cmap
      ) sigma Cmap.empty in
      trans := Smap.add q trans_q !trans;  (* Update transitions *)
      Cmap.iter (fun _ q' -> transitions q') trans_q (* Recursively process all new states *)
    end
  in
  let q0 = first r in
  transitions q0;
  { start = q0; trans = !trans }

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

let () = 
  (* (a|b)*a(a|b) *)
  let r = Concat (Star (Union (Character ('a', 1), Character ('b', 1))),
  Concat (Character ('a', 2),
  Union (Character ('a', 3), Character ('b', 2)))) in
  let a = make_dfa r in
  save_autom "autom.dot" a in
  Printf.printf "ex4 passed\n"