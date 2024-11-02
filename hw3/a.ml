type buffer = { text: string; mutable current: int; mutable last: int }

let next_char b =
  if b.current >= String.length b.text then raise End_of_file;
  let c = b.text.[b.current] in
  b.current <- b.current + 1;
  c

let rec state0 b =
  try
    let c = next_char b in
    match c with
    | 'a' -> state0 b
    | 'b' -> state1 b
    | _ -> failwith "lexical error"
  with
  | End_of_file ->
    if b.last >= 0 then raise End_of_file else failwith "lexical error"

and state1 b =
  b.last <- b.current;
  try
    let c = next_char b in
    failwith "lexical error"
  with
  | End_of_file ->
    raise End_of_file

let start = state0
