let rec nb_bit_pos (n: int) : int =
  if n = 0 then 0
  else (1 land n) + nb_bit_pos (n lsr 1);;

  let rec nb_bit_pos_tail (n: int) (acc: int) : int =
    if n = 0 then acc
    else nb_bit_pos_tail (n lsr 1) (acc + (1 land n));;

let () =
  Printf.printf "%d\n" (nb_bit_pos 11);;
  Printf.printf "%d\n" (nb_bit_pos_tail 11 0);;