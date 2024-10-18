
open Ast
open Format

(* Exception raised to signal a runtime error *)
exception Error of string
let error s = raise (Error s)

(* Values of Mini-Python.

   Two main differences wrt Python:

   - We use here machine integers (OCaml type `int`) while Python
     integers are arbitrary-precision integers (we could use an OCaml
     library for big integers, such as zarith, but we opt for simplicity
     here).

   - What Python calls a ``list'' is a resizeable array. In Mini-Python,
     there is no way to modify the length, so a mere OCaml array can be used.
*)
type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

(* Print a value on standard output *)
let rec print_value = function
  | Vnone -> printf "None"
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vstring s -> printf "%s" s
  | Vlist a ->
    let n = Array.length a in
    printf "[";
    for i = 0 to n-1 do print_value a.(i); if i < n-1 then printf ", " done;
    printf "]"

(* Boolean interpretation of a value

   In Python, any value can be used as a Boolean: None, the integer 0,
   the empty string, and the empty list are all considered to be
   False, and any other value to be True.
*)
let is_false v = 
    match v with
    | Vnone -> true
    | Vbool false -> true
    | Vstring "" -> true
    | Vlist [||] -> true
    | Vint n -> n = 0
    | _ -> false

let is_true v = 
    let b = is_false v in
    not b

(* We only have global functions in Mini-Python *)

let functions = (Hashtbl.create 16 : (string, ident list * stmt) Hashtbl.t)

(* The following exception is used to interpret `return` *)

exception Return of value

(* Local variables (function parameters and local variables introduced
   by assignments) are stored in a hash table that is passed to the
   following OCaml functions as parameter `ctx`. *)

type ctx = (string, value) Hashtbl.t

(* Interpreting an expression (returns a value) *)

let string_of_binop = function
    | Badd -> "Badd"
    | Bsub -> "Bsub"
    | Bmul -> "Bmul"
    | Bdiv -> "Bdiv"
    | Bmod -> "Bmod"
    | Beq -> "Beq"
    | Bneq -> "Bneq"
    | Blt -> "Blt"
    | Ble -> "Ble"
    | Bgt -> "Bgt"
    | Bge -> "Bge"
    | Band -> "Band"
    | Bor -> "Bor"

let rec expr ctx = function
    | Ecst Cnone ->
        Vnone
    | Ecst (Cstring s) ->
        Vstring s
    (* arithmetic *)
    | Ecst (Cint n) ->
        Vint (Int64.to_int n)
    | Ebinop (Badd | Bsub | Bmul | Bdiv | Bmod |
                Beq | Bneq | Blt | Ble | Bgt | Bge as op, e1, e2) ->
        let v1 = expr ctx e1 in
        let v2 = expr ctx e2 in
        begin match op, v1, v2 with
            | Badd, Vint n1, Vint n2 -> 
                Vint (n1 + n2)
            | Bsub, Vint n1, Vint n2 -> 
                Vint (n1 - n2)
            | Bmul, Vint n1, Vint n2 -> 
                Vint (n1 * n2)
            | Bdiv, Vint n1, Vint n2 -> 
                if n2 = 0 then error "division by zero" else Vint (n1 / n2)
            | Bmod, Vint n1, Vint n2 -> 
                if n2 = 0 then error "division by zero" else Vint (n1 mod n2)
            | Beq, _, _  -> 
                Vbool (v1 = v2)
            | Bneq, _, _ -> 
                Vbool (v1 <> v2)
            | Blt, _, _  -> 
                Vbool (v1 < v2)
            | Ble, _, _  -> 
                Vbool (v1 <= v2)
            | Bgt, _, _  -> 
                Vbool (v1 > v2)
            | Bge, _, _  ->
                Vbool (v1 >= v2)
            | Badd, Vstring s1, Vstring s2 ->
                Vstring (s1 ^ s2)
            | Badd, Vlist l1, Vlist l2 ->
                Vlist (Array.append l1 l2)
            | _ -> 
                error "unsupported operand types"
        end
    | Eunop (Uneg, e1) ->
        let v1 = expr ctx e1 in
        begin match v1 with
            | Vint n -> Vint (-n)
            | _ -> error "unsupported operand type"
        end
    (* Boolean *)
    | Ecst (Cbool b) ->
        Vbool b
    | Ebinop (Band, e1, e2) ->
        let v1 = expr ctx e1 in
            if is_false v1 then v1 else expr ctx e2
    | Ebinop (Bor, e1, e2) ->
        let v1 = expr ctx e1 in
            if is_true v1 then v1 else expr ctx e2
    | Eunop (Unot, e1) ->
        let v1 = expr ctx e1 in
            Vbool (is_false v1)
    | Eident {id} ->
        Hashtbl.find ctx id
    (* function call *)
    | Ecall ({id="len"}, [e1]) ->
        let v1 = expr ctx e1 in
        begin match v1 with
            | Vstring s -> Vint (String.length s)
            | Vlist l -> Vint (Array.length l)
            | _ -> error "unsupported type"
        end
    | Ecall ({id="list"}, [Ecall ({id="range"}, [e1])]) ->
        let n = expr ctx e1 in
        begin match n with
            | Vint vn -> Vlist (Array.init (max 0 vn) (fun i -> Vint i))
            | _ -> error "unsupported type"
        end 
    | Ecall ({id=f}, el) ->
        let (pl, s) = Hashtbl.find functions f in
        let ctx' = Hashtbl.create 16 in
        List.iter2 (fun {id} v -> Hashtbl.add ctx' id (expr ctx v)) pl el;
        begin try stmt ctx' s; Vnone with Return v -> v end
    | Elist el ->
        Vlist (Array.of_list (List.map (expr ctx) el))
    | Eget (e1, e2) ->
        let v1 = expr ctx e1 in
        let v2 = expr ctx e2 in
        begin match v1, v2 with
            | Vlist l, Vint i -> 
                if i < 0 || i >= Array.length l then error "index out of bounds" else l.(i)
            | _, _ -> error "unsupported type"
        end

(* Interpreting a statement

   returns nothing but may raise exception `Return` *)

and stmt ctx = function
    | Seval e ->
        ignore (expr ctx e)
    | Sprint e ->
        print_value (expr ctx e); printf "@."
    | Sblock bl ->
        block ctx bl
    | Sif (e, s1, s2) ->
        let v = expr ctx e in
            if is_true v then stmt ctx s1 else stmt ctx s2
    | Sassign ({id}, e1) ->
        Hashtbl.replace ctx id (expr ctx e1)
    | Sreturn e ->
        let v = expr ctx e in
            raise (Return v)
    | Sfor ({id}, e, s) ->
        let v = expr ctx e in
        begin match v with
        | Vlist lst -> 
            Array.iter (fun v -> Hashtbl.replace ctx id v;
            stmt ctx s) lst;
        | _ -> error "list expected" end
    | Sset (e1, e2, e3) ->
        let v1 = expr ctx e1 in
        let v2 = expr ctx e2 in
        let v3 = expr ctx e3 in
        begin match v1, v2 with 
            | Vlist v, Vint e -> v.(e) <- v3
            | _, _ -> error "unsupported type" 
        end

(* Interpreting a block (a sequence of statements) *)

and block ctx = function
  | [] -> ()
  | s :: sl -> stmt ctx s; block ctx sl

(* Interpreting a file
   - `dl` is a list of function definitions (see type `def` in ast.ml)
   - `s` is a statement (the toplevel code)
*)

let file (dl, s) =
    let ctx = Hashtbl.create 16 in
    List.iter (fun (f, pl, s) -> Hashtbl.add functions f.id (pl, s)) dl;
    stmt ctx s

