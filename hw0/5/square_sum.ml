let square (x: int) : int = 
  x * x;;

let rec sum (l: int list) : int = 
  match l with
  | [] -> 0
  | hd::remain -> hd + sum remain;;

let square_sum (l: int list) : int = 
  sum (List.map square l)

let () = 
  let list = [1;2;3;4] in
  Printf.printf "%d\n" (square_sum list);;

