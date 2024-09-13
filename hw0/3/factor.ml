let factor (m1: string) (m2: string) : bool = 
  let len_m1 = String.length m1 in
  let rec check_at_pos pos = 
    if pos + len_m1 > String.length m2 then false
    else if String.sub m2 pos len_m1 = m1 then true
    else check_at_pos (pos + 1)
  in
  check_at_pos 0;;

let () =
  Printf.printf "%b\n" (factor "hello" "hello world");;
  Printf.printf "%b\n" (factor "cat" "hello world");;