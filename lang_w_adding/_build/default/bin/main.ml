(* This is a compact type *)
(* Term is sort of like an enum 
   w diff predefined possible values *)
open Core
open Stdlib (*many of the List methods are from Stdlib; make sure you 
                include this library*)


(* Your AST (Abstract syntax tree) *)
type term = 
| Num of int 
| Bol of bool
| Vector of term list 
| Add of term * term
| Cond of term * term * term (*1st term = condition; 2nd term = then; 3rd term = else*)
;;

(* This function is for writing test cases for the eval function *)


let rec eq (t1 : term) (t2 : term) : bool = 
  match (t1, t2) with
  | (Num n1, Num n2) -> 
    (* In an If-statement, the "then" and "else" branch must 
        return the same type.*)
    (* If-statements without an else branch must return unit *)
    if n1 = n2 then 
      true
    else 
      false
  | (Vector v1, Vector v2) -> 
    
    let result = Core.List.map2_exn ~f: (fun x1 x2 -> eq x1 x2) v1 v2 in
    if List.mem false result then
      false
    else 
      true
  | (Bol b1, Bol b2) ->
    if b1 && b2 then true
    else false
  | _ -> failwith "Mega fail!!!"
;;

(* Add(Add((Num(4), Num(5))), Num(5)) *)
let is_num n = 
  match n with
  | Num _ -> true
  | _ -> false
;;
"(1 2 3 4 5) + (1 2 3 4 5)"
let rec eval (t : term) : term =
  match t with
  | Num _ -> t
  | Bol _ -> t
  | Add(t1, t2) -> (*note that adding requires two arguments*)
    let evald_t1 = eval t1 in
    let evald_t2 = eval t2 in
    
    (match (evald_t1, evald_t2) with
    | (Num n1 , Num n2) -> Num(n1 + n2)
    | (Vector v1, Vector v2) -> 
      let nums_only1 = List.for_all (is_num) v1 in
      let nums_only2 = List.for_all (is_num) v2 in
      if nums_only1 && nums_only2 then Vector (Core.List.map2_exn ~f: (fun x1 x2 -> eval (Add (x1, x2))) v1 v2)
      else Vector ([])
      
    | _ -> failwith "Error: adding")
    
  | Cond (c, thn, els) ->
    (match c with
    | Bol b ->
      if b then (eval thn) else (eval els)
    | _ -> failwith "Error: not a bool")
  | Vector _ -> t
  (* | _ -> failwith "idk"  *)
;;




   
(* What do s-expressions look like*)
(* Ocaml s-expressions include 
  "Atoms" or "Lists"

  "lists" are parentheses that include a series of "atoms"
  *)

(* They look sort of like strings in parentheses 
   ex.) (func 1 2)
    *)

(* Why is it important to use s-expressions: then you can interpret things into your own language *)
(* We have to take in a string to start and convert it to an s-expression *)


  
let rec parse (s_expr : Sexp.t) : term = 
  match s_expr with
  | Sexp.List l -> 
    (match l with
    | [Sexp.Atom "?"; condition ; thn ; els] ->
      Cond ((parse condition), (parse thn), (parse els))
    | [t1 ; Sexp.Atom "+"; t2] -> (*parsing from s-expression to Add*)
      Add ((parse t1), (parse t2))
    | [t1; Sexp.Atom "="; t2] -> (*parsing from s-expression to Condition*)
      Bol ((eq (parse t1) (parse t2)))
    | [t1; Sexp.Atom "%"; t2] -> 
      let prsd1 = parse t1 in
      let prsd2 = parse t2 in
      (match prsd1 with
      | Num n1 -> 
        (match prsd2 with
        | Num n2 ->
          Num ((n1 mod n2))
        | _ -> failwith "can't perform modulus")
      | _ -> failwith "not two numbers")
    | _ -> 
      Vector(List.map parse l))
       
  | Sexp.Atom a ->

    if (Str.string_match (Str.regexp "^[0-9]+$") a 0) then
      Num (Stdlib.int_of_string a)
    else
      let check_bool s = 
        if s = "true" then true 
        else if s = "false" then false  
        else failwith "not bool" 
      in
      let possible_bool = check_bool a in
      if possible_bool || not possible_bool then 
        Bol (possible_bool)
      else
        failwith "error making bool"
;;

let print_term t =
  match t with
  | Num n -> Caml.print_endline "Num (" + (Int.to_string n) + ") "

let pretty_print x = 
  Caml.print_endline x;
;;

let str_expr = "1";;
let s_expr = Sexp.of_string str_expr;;
pretty_print (Bool.to_string (eq (Num (1)) (eval (parse s_expr))));;
pretty_print (Bool.to_string (eq (Vector([Num (1); Num (2); Num (3); Num (4)])) (Vector([Num (1); Num (2); Num (3); Num (4)]))) );;
pretty_print (Bool.to_string (eq (Num(631)) (eval(Add(Num(400), Add(Num(1), Num(230)))))));; 
pretty_print (Bool.to_string (eq (Num(631)) (eval (Add(Num(400), Num(231))))));;
let s_exprTest = Sexp.of_string "(1 2 3 4 5)";;
pretty_print (Bool.to_string (eq (Vector([Num (1); Num (2); Num (3); Num (4); Num(5)])) (eval (parse s_exprTest))));;



(* Checking addition from s-expressions *)
Caml.print_endline "";;
Caml.print_endline "Checking addition from s-expressions to AST";;
let s_expr1 = Sexp.of_string "(1 + 2)";;



pretty_print (Bool.to_string (eq (eval (parse s_expr1)) (Num (3))));;

Caml.print_endline "";
pretty_print "testing associative property addition" ;;
let s_expr1 = Sexp.of_string "(1 + (1 + 2))";;
pretty_print (Bool.to_string (eq (eval (parse s_expr1)) (eval(Num (4)))));;

Caml.print_endline "";
pretty_print "testing conditional";;
let s_expr_condition = Sexp.of_string "(? ((10 % 2) = 0) 123 4)";;
pretty_print (Bool.to_string (eq (eval (parse s_expr_condition)) (eval(Num 123))));;

Caml.print_endline "";;
pretty_print "testing vector equality";;

let s_expr_testing_vec = Sexp.of_string "((1 2 3 4 5) = (1 2 3 4 5))";;
pretty_print (Bool.to_string (eq (eval (parse s_expr_testing_vec)) (eval (Bol(true)) )));;


let sexp_st1 = "((1 2 3 4 5 6) + (1 2 3 4 5 6))" in
let s_expr_testing_vec_add = Sexp.of_string sexp_st1 in

let sexp_st2 = "(2 4 6 8 10 12)" in
let s_expr_answer = Sexp.of_string sexp_st2 in
pretty_print (Bool.to_string (eq (eval (parse s_expr_testing_vec_add)) ((eval (parse s_expr_answer) )) ) );;