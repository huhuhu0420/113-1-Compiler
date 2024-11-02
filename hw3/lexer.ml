(* lexer.ml *)

open A

let tokenize (input_string: string) =
  (* Initialize the buffer *)
  let buffer = { text = input_string; current = 0; last = -1 } in

  (* Main loop to tokenize the input *)
  let rec lexer_loop () =
    buffer.last <- -1;
    (* Printf.printf "current: %d\n" buffer.current; *)
    let token_start = buffer.current in
    try
      start buffer
    with
    | End_of_file ->
        if buffer.last = -1 then
          Printf.printf "exception End of file\n"
        else begin
          let token_length = buffer.last - token_start in
          let token = String.sub buffer.text token_start token_length in
          Printf.printf "--> \"%s\"\n" token;
          buffer.current <- buffer.last;
          if buffer.current < String.length buffer.text then
            lexer_loop ()
        end
    | Failure msg ->
      if buffer.last = -1 then
        Printf.printf "exception Failure: %s\n" msg
      else begin
        let token_length = buffer.last - token_start in
        let token = String.sub buffer.text token_start token_length in
        Printf.printf "--> \"%s\"\n" token;
        buffer.current <- buffer.last;
        if buffer.current < String.length buffer.text then
          lexer_loop ()
      end
  in
  lexer_loop ();
  Printf.printf "Done\n"

let () =
  let input_string = "abbaaab" in 
  tokenize input_string;

  let input_string = "aba" in
  tokenize input_string