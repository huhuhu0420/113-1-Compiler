let rec fact (n: int) : int =
  if n = 0 
    then 1 
    else n * fact (n - 1);;

let fact_tail (n: int) : int =
  let rec fact_aux (n: int) (acc: int) : int =
    if n = 0 
      then acc 
      else fact_aux (n - 1) (n * acc) in
    fact_aux n 1;;
  
let () =
  Printf.printf "%d\n" (fact 5);;
  Printf.printf "%d\n" (fact_tail 5);;

