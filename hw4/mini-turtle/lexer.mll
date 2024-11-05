
(* Lexical analyser for mini-Turtle *)

{
  open Lexing
  open Parser

  (* raise exception to report a lexical error *)
  exception Lexing_error of string

  (* note : remember to call the Lexing.new_line function
at each carriage return ('\n' character) *)

}

rule token = parse
  | "(*" { comment lexbuf }
  | "//" { single_line_comment lexbuf }
  | [' ' '\t' '\n']+ { token lexbuf }
  | eof { EOF }

and comment = parse
  | "*)" { token lexbuf }
  | _ { comment lexbuf }
  | eof { raise (Lexing_error "Unterminated comment") }

and single_line_comment = parse
  | '\n' { token lexbuf }
  | _ { single_line_comment lexbuf }
  | eof { EOF }