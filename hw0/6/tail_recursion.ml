
let rev (l: int list) : (int list) = 
  let rec rev_aux l acc = 
    match l with
    | [] -> acc
    | h::t -> rev_aux t (h::acc)
  in
  rev_aux l [];;

let square (x: int) : int = 
  x * x;;

let map (func: 'a -> 'b) (l: int list) : (int list) = 
  let rec map_aux l acc = 
    match l with
    | [] -> rev acc
    | h::t -> map_aux t ((func h)::acc)
  in 
  map_aux l [];;

let () =
  let l = List.init 10 (fun x -> x) in
  let rev_l = rev l in
  Printf.printf "%s\n" (String.concat " " (List.map string_of_int rev_l));
  let map_l = map square l in
  Printf.printf "%s\n" (String.concat " " (List.map string_of_int map_l)) 
