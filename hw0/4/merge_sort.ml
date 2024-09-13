let split (l: int list) : (int list * int list) =
  let n = List.length l / 2 in
  let rec split_aux lst acc n = 
    if n <= 0 then (lst, List.rev acc)
    else 
      split_aux (List.tl lst) ((List.hd lst)::acc) (n - 1)
  in
  split_aux l [] n;;

let merge (l1: int list) (l2: int list) : int list =
  let rec merge_aux l1 l2 acc = 
    match (l1, l2) with
    | ([], []) -> List.rev acc
    | ([], h::t) -> merge_aux [] t (h::acc)
    | (h::t, []) -> merge_aux t [] (h::acc)
    | (h1::t1, h2::t2) -> if h1 < h2 then merge_aux t1 l2 (h1::acc)
                          else merge_aux l1 t2 (h2::acc)
  in
  merge_aux l1 l2 [];;

let sort (lst: int list) : int list = 
  let rec sort_aux l = 
    if List.length l <= 1 then l
    else 
      let (l1, l2) = split l in
      let sorted_l1 = sort_aux l1 in
      let sorted_l2 = sort_aux l2 in 
      merge sorted_l1 sorted_l2 in
  sort_aux lst

let () =
  (* let (l1, l2) = split [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
  Printf.printf "%s\n" (String.concat " " (List.map string_of_int l1));
  Printf.printf "%s\n" (String.concat " " (List.map string_of_int l2));
  Printf.printf "%s\n" (String.concat " " (List.map string_of_int (merge l1 l2)));; *)
  let lst = [2; 1; 5; 4; 6; 8; 7; 2] in
  Printf.printf "%s\n" (String.concat " " (List.map string_of_int (sort lst)));;