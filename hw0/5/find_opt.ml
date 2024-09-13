let find_opt (x: 'a) (l: 'a list) : 'a option = 
  let is_equal x y = 
    x == y in
  List.find_index (is_equal x) l 

let print_option opt =
    match opt with
    | Some value -> Printf.printf "Some %d\n" value
    | None -> Printf.printf "None\n"

let () = 
  let l = [1; 2; 3; 4] in
  let result = find_opt 3 l in
  print_option result;
  let result = find_opt 9 l in
  print_option result;
