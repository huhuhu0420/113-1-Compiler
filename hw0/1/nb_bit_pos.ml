let rec nb_bit_pos (n: int) : int =
  if n = 0 then 0
  else (1 land n) + nb_bit_pos (n lsr 1);;

let () =
  Printf.printf "%d\n" (nb_bit_pos 11);;