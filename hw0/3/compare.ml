let compare (m1: string) (m2: string) : bool =
  let rec aux index =
    if m1.[index] < m2.[index] then true
    else if m1.[index] > m2.[index] then false
    else if index = String.length m1 - 1 then false
    else aux (index + 1)
  in
  aux 0;;

let () = 
  Printf.printf "%b\n" (compare "hello" "world");;
  Printf.printf "%b\n" (compare "world" "hello");;