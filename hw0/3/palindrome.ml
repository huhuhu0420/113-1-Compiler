let palindrome (s: string) : bool =
  let rec aux i j = 
    if i >= j then true
    else if s.[i] = s.[j] then aux (i + 1) (j - 1)
    else false
  in
  aux 0 (String.length s - 1);; 

let () =
  Printf.printf "%b\n" (palindrome "racecar");;
  Printf.printf "%b\n" (palindrome "hello");;