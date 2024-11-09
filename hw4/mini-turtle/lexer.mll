
(* Lexical analyser for mini-Turtle *)

{
  open Lexing
  open Parser

  (* raise exception to report a lexical error *)
  exception Lexing_error of string

  (* note : remember to call the Lexing.new_line function
at each carriage return ('\n' character) *)
  let id_or_kwd = 
    let h = Hashtbl.create 32 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok)
      [
        "forward", FORWARD; "penup", PENUP; "pendown", PENDOWN; "color", COLOR;
        "black", BLACK; "white", WHITE; "red", RED; "green", GREEN; "blue", BLUE;
        "turnleft", TURNLEFT; "turnright", TURNRIGHT; "if", IF; "else", ELSE; "def", DEF; "repeat", REPEAT;
        "IF", IF; "ELSE", ELSE; "DEF", DEF; "REPEAT", REPEAT;
      ];
    fun s -> try Hashtbl.find h s with Not_found -> IDENT s


}


let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter|digit|'_')*

rule token = parse
  | "(*" { comment lexbuf }
  | "//" { single_line_comment lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r'] { token lexbuf }
  | digit+ as n { INT (int_of_string n) }
  | ident as id { id_or_kwd id }
  | '+' { ADD }
  | '-' { SUB }
  | '*' { MUL }
  | '/' { DIV }
  | '(' { LP }
  | ')' { RP }
  | '{' { LCURL }
  | '}' { RCURL }
  | ',' { COMMA }
  | ':' { COLON }
  | eof { EOF }

and comment = parse
  | "*)" { token lexbuf }
  | _ { comment lexbuf }
  | eof { raise (Lexing_error "Unterminated comment") }

and single_line_comment = parse
  | '\n' { token lexbuf }
  | _ { single_line_comment lexbuf }
  | eof { EOF }