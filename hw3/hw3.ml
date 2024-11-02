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

let recognize (a: autom) (s: string) : bool =
  let rec recognize_rec q s =
    match s with
    | [] -> Cset.mem eof q
    | c :: s' ->
      try
        let trans_q = Smap.find q a.trans in
        try
          let q' = Cmap.find c trans_q in 
          recognize_rec q' s'
        with Not_found -> false
      with Not_found -> false
  in
  recognize_rec a.start (List.init (String.length s) (String.get s))

let generate (filename: string) (a: autom) : unit =
  (* Step 1: Number all states *)
  let state_numbers = ref Smap.empty in
  let counter = ref 0 in
  let rec number_states q =
    if not (Smap.mem q !state_numbers) then begin
      state_numbers := Smap.add q !counter !state_numbers;
      incr counter;
      try
        let trans_q = Smap.find q a.trans in
        Cmap.iter (fun _ q' -> number_states q') trans_q
      with Not_found -> ()
    end
  in
  number_states a.start;

  (* Step 2: Open the output file *)
  let ch = open_out filename in
  let fmt = Format.formatter_of_out_channel ch in

  (* Step 3: Write the buffer type and next_char function *)
  Format.fprintf fmt "type buffer = { text: string; mutable current: int; mutable last: int }@\n@\n";
  Format.fprintf fmt "let next_char b =@\n";
  Format.fprintf fmt "  if b.current >= String.length b.text then raise End_of_file;@\n";
  Format.fprintf fmt "  let c = b.text.[b.current] in@\n";
  Format.fprintf fmt "  b.current <- b.current + 1;@\n";
  Format.fprintf fmt "  c@\n@\n";

  (* Step 4: Write the state functions *)
  let printed_states = ref Smap.empty in

  let rec print_state q is_first =
    if not (Smap.mem q !printed_states) then begin
      printed_states := Smap.add q () !printed_states;
      let state_num = Smap.find q !state_numbers in
      if is_first then
        Format.fprintf fmt "let rec state%d b =@\n" state_num
      else
        Format.fprintf fmt "and state%d b =@\n" state_num;

      (* Check if the state is accepting *)
      if Cset.mem eof q then
        Format.fprintf fmt "  b.last <- b.current;@\n";

      (* Get the transitions for this state *)
      let trans_q =
        try Smap.find q a.trans
        with Not_found -> Cmap.empty
      in

      (* Begin try block *)
      Format.fprintf fmt "  try@\n";
      Format.fprintf fmt "    let c = next_char b in@\n";

      if Cmap.is_empty trans_q then
        (* No transitions, raise lexical error *)
        Format.fprintf fmt "    failwith \"lexical error\"@\n"
      else begin
        (* Match on the character *)
        Format.fprintf fmt "    match c with@\n";
        Cmap.iter (fun c q' ->
          let c_repr =
            if c = '\'' then "'\\''"
            else if c = '\\' then "'\\\\'"
            else Printf.sprintf "'%c'" c
          in
          let q'_num = Smap.find q' !state_numbers in
          Format.fprintf fmt "    | %s -> state%d b@\n" c_repr q'_num;
          (* Recursively print the next state *)
          (* print_state q' false *)
        ) trans_q;
        Format.fprintf fmt "    | _ -> failwith \"lexical error\"@\n"
      end;

      (* End of try block, begin with block *)
      Format.fprintf fmt "  with@\n";
      Format.fprintf fmt "  | End_of_file ->@\n";
      (* If End_of_file is raised, check if in accepting state *)
      if Cset.mem eof q then
        Format.fprintf fmt "    raise End_of_file@\n"
      else
        Format.fprintf fmt "    if b.last >= 0 then raise End_of_file else failwith \"lexical error\"@\n";

      Format.fprintf fmt "@\n";
      Cmap.iter (fun _ q' -> print_state q' false) trans_q;
    end
    (* recursively print the next states *)
  in

  (* Start printing from the start state *)
  print_state a.start true;

  (* Step 5: Write the start function *)
  let start_num = Smap.find a.start !state_numbers in
  Format.fprintf fmt "let start = state%d@\n" start_num;

  (* Close the output file *)
  close_out ch

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

  (* ex4 *)
  (* (a|b)*a(a|b) *)
  let r = Concat (Star (Union (Character ('a', 1), Character ('b', 1))),
  Concat (Character ('a', 2),
  Union (Character ('a', 3), Character ('b', 2)))) in
  let a = make_dfa r in
  save_autom "autom.dot" a;
  Printf.printf "ex4 passed\n";

  (* ex5 *)
  assert (recognize a "aa");
  assert (recognize a "ab");
  assert (recognize a "abababaab");
  assert (recognize a "babababab");
  assert (recognize a (String.make 1000 'b' ^ "ab"));
  assert (not (recognize a ""));
  assert (not (recognize a "a"));
  assert (not (recognize a "b"));
  assert (not (recognize a "ba"));
  assert (not (recognize a "aba"));
  assert (not (recognize a "abababaaba"));

  let r = Star (Union (Star (Character ('a', 1)),
  Concat (Character ('b', 1),
  Concat (Star (Character ('a',2)),
  Character ('b', 2))))) in
  let a = make_dfa r in
  save_autom "autom2.dot" a;

  assert (recognize a "");
  assert (recognize a "bb");
  assert (recognize a "aaa");
  assert (recognize a "aaabbaaababaaa");
  assert (recognize a "bbbbbbbbbbbbbb");
  assert (recognize a "bbbbabbbbabbbabbb");
  assert (not (recognize a "b"));
  assert (not (recognize a "ba"));
  assert (not (recognize a "ab"));
  assert (not (recognize a "aaabbaaaaabaaa"));
  assert (not (recognize a "bbbbbbbbbbbbb"));
  assert (not (recognize a "bbbbabbbbabbbabbbb"));
  Printf.printf "ex5 passed\n";

  (* ex6 *)
  (*  (a*\)b *)
  let r3 = Concat (Star (Character ('a', 1)), Character ('b', 1)) in
  let a = make_dfa r3 in
  save_autom "a.dot" a;
  generate "a.ml" a;

  (* (epsilon | ab)* (a|epsilon) *)
  (* let r4 = Concat (Star (Union (Epsilon, Concat (Character ('a', 1), Character ('b', 1)))), Union (Character ('a', 2), Epsilon)) in
  let a2 = make_dfa r4 in
  save_autom "a2.dot" a2;
  generate "a2.ml" a2; *)

  Printf.printf "ex6 passed\n"