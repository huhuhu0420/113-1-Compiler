type 'a seq =
| Elt of 'a
| Seq of 'a seq * 'a seq

let (@@) x y = Seq(x, y)

let rec string_of_seq (s: 'a seq) : string = 
  match s with
  | Elt x -> string_of_int x
  | Seq (s1, s2) -> string_of_seq s1 ^ "," ^ string_of_seq s2

let rec hd (s: 'a seq) : 'a seq = 
  match s with 
  | Elt x -> Elt x
  | Seq (s1, s2) -> hd s1

let rec tl (s: 'a seq) : 'a seq = 
  match s with
  | Elt _ -> failwith "Empty"
  | Seq (Elt _, s2) -> s2
  | Seq (s1, s2) -> tl s1 @@ s2
    

let rec mem (target: 'a seq) (s: 'a seq) : bool = 
  match s with 
  | Elt x -> Elt x = target
  | Seq (s1, s2) -> mem target s1 || mem target s2

let rec rev (s: 'a seq) : 'a seq = 
  match s with
  | Elt x -> Elt x
  | Seq (s1, s2) -> rev s2 @@ rev s1

let rec map (func: 'a -> 'b) (s: 'a seq) : 'b seq = 
  match s with
  | Elt x -> Elt (func x)
  | Seq (s1, s2) -> map func s1 @@ map func s2

let rec fold_left (func: 'a -> 'b -> 'a) (acc: 'a) (s: 'b seq) : 'a = 
  match s with
  | Elt x -> 
    (func acc x) ;
  | Seq (s1, s2) -> 
      let acc = fold_left func acc s1 in 
      fold_left func acc s2;;

let rec fold_right (func: 'a -> 'b -> 'a) (acc: 'a) (s: 'b seq) : 'a = 
  match s with
  | Elt x -> 
    (func acc x) ;
  | Seq (s1, s2) -> 
      let acc = fold_right func acc s2 in 
      fold_left func acc s1;;

let seq2list (s: 'a seq) : 'a list = 
  let rec seq2list_aux (s: 'a seq) (acc: 'a list) : 'a list = 
    match s with
    | Elt x -> x::acc
    | Seq (s1, s2) -> seq2list_aux s1 (seq2list_aux s2 acc)
  in
  seq2list_aux s []

let find_opt (target: 'a seq) (l: 'a seq) : int option =
  let rec find_opt_aux (target: 'a seq) (l: 'a seq) (index: int) : (int option * int) = 
    match l with
    | Elt x -> 
      if Elt x = target then (Some index, (index + 1))
      else (None, (index + 1))
    | Seq (s1, s2) -> 
      match find_opt_aux target s1 index with
      | Some x, index -> Some x, index
      | None, index' -> find_opt_aux target s2 index'
  in
  find_opt_aux target l 0 |> fst

let string_of_option opt =
    match opt with
    | Some value -> string_of_int value
    | None -> "None"

let nth (s: 'a seq) (n: int) : 'a seq = 
  let s_list = seq2list s in
  try 
    let ele = List.nth s_list n in
    Elt ele
  with
  | _ -> raise (Failure "nth")


let () = 
  let e1 = Elt 1 in
  let e2 = Elt 2 in
  let e3 = Elt 3 in
  let e4 = Elt 4 in
  let s = Seq (Seq (e1, e2), e3) in
  let s_string = string_of_seq s in
  Printf.printf "original: %s\n" s_string;
  Printf.printf "head: %s\n" (string_of_seq (hd s));
  Printf.printf "tail: %s\n" (string_of_seq (tl s));
  Printf.printf "rev %s\n" (string_of_seq (rev s));
  Printf.printf "is_mem 4: %s\n" (string_of_bool (mem e4 s));
  Printf.printf "is_mem 3: %s\n" (string_of_bool (mem e3 s));
  Printf.printf "map : %s\n" (string_of_seq (map (fun x -> x*2) s));
  Printf.printf "fold left: %d\n" (fold_left(fun acc x->acc+x) 0 s);
  Printf.printf "fold right: %d\n" (fold_right(fun acc x->acc-x) 0 s);
  Printf.printf "map: %s\n" (String.concat " " (List.map string_of_int (seq2list s)));
  Printf.printf "find 1: index %s\n" (string_of_option (find_opt e1 s));
  Printf.printf "find 2: index %s\n" (string_of_option (find_opt e2 s));
  Printf.printf "find 3: index %s\n" (string_of_option (find_opt e3 s));
  Printf.printf "find 4: index %s\n" (string_of_option (find_opt e4 s));
  try
    Printf.printf "nth 4: %s\n" (string_of_seq (nth s 4));
  with
  | Failure e -> Printf.printf "index out of range %s\n" e;
  try
    Printf.printf "nth 2: %s\n" (string_of_seq (nth s 2));
  with
  | Failure e -> Printf.printf "index out of range %s\n" e;