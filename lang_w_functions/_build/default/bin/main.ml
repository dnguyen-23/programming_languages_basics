open Core;;

let pretty_print inp_str = 
  Caml.print_endline "";
  Caml.print_endline inp_str;

type term = 
| Tnum of int
| Tbool of bool
(* Control Structures *)
| If_else of term * term * term (* 1: condition; 2: body1; 3: body2*)
| For_loop of term * term 
(* Binary Operators *)
| Add of term * term
| Equals of term * term
(* | ID of string * term 1.) name of the identifier; 2.) a body of terms or simply a term *)
(* Body expression *)
| Body of term list



(* Takes in Sexp.t as input; recursive, so it cannot be a string *)
(* Parses Sexp.t to type term *)
let rec parse (inp_sexp : Sexp.t) : term =
  match inp_sexp with
  | Sexp.List l -> (* "l" represents a list type of Sexp.t *)
    ( match l with 
    (* 1.) parse control structures *)
    | [Sexp.Atom "if"; condition; body_expr1; Sexp.Atom "else"; body_expr2 ] ->
      let p_cond = parse condition in 
      let p_body_expr1 = parse body_expr1 in 
      let p_body_expr2 = parse body_expr2 in 
      If_else (p_cond, p_body_expr1, p_body_expr2)
    | [Sexp.Atom "for"; num_iterations; body_expr] ->
      let p_num_iter = parse num_iterations in
      let p_body_expr = parse body_expr in
      For_loop (p_num_iter, p_body_expr)
    (* 2.) parse binary operators *)
    | [t1; operator; t2] ->
      let p_t1 = parse t1 in
      let p_t2 = parse t2 in
      (match operator with
      | Sexp.Atom "+" -> Add(p_t1, p_t2)
      | Sexp.Atom "==" -> Equals(p_t1, p_t2)
      | _ -> failwith "not a binary operator"
      )
    (* 3.) body of expressions *)
    | _ -> Body (List.map ~f:parse l) (* parse each element in the list*)
    
    
    )
  | Sexp.Atom a -> (* "a" a string*)
    if (Str.string_match (Str.regexp "-?[0-9]+$") a 0) then Tnum (int_of_string a)
    else if (String.equal a "false") || (String.equal a "true") then Tbool (Bool.of_string a)
    else failwith "neither bool or num"
  ;;


let rec string_parse_result buffer t =
  match t with
  | Tnum n -> buffer ^ "Num (" ^ (Int.to_string n) ^ ")"
  | Tbool b -> buffer ^ "Bool (" ^ (Bool.to_string b) ^ ")"
  | If_else (condition, body_expr1, body_expr2) -> 
    let temp_buffer = buffer ^ "if (" in
    let temp_buffer_w_cond = (string_parse_result temp_buffer condition) ^ ")\n" in
    let temp_buffer_w_body1 = (string_parse_result temp_buffer_w_cond body_expr1) ^ "\nelse\n" in
    let final_buffer = string_parse_result temp_buffer_w_body1 body_expr2 in
    let final = buffer ^ final_buffer in
    final
  | For_loop (num_iterations, body_expr) ->
    let temp_buffer = buffer ^ "for (" in
    let temp_buffer_w_iter = (string_parse_result temp_buffer num_iterations) ^ ")\n" in
    let final_buffer = string_parse_result temp_buffer_w_iter body_expr in
    let final = buffer ^ final_buffer in
    final
  (* printing binary operator expressions *)
  | Add (t1, t2) -> 
    let temp_buffer = buffer ^ "(" in
    let buffer_w_t1 = string_parse_result temp_buffer t1 in
    let buffer_w_op = buffer_w_t1 ^ " + " in
    let final_buffer = string_parse_result buffer_w_op t2 in
    let final = final_buffer ^ ")" in
    final
  | Equals(t1, t2)-> 
    let temp_buffer = buffer ^ "(" in
    let buffer_w_t1 = string_parse_result temp_buffer t1 in
    let buffer_w_op = buffer_w_t1 ^ " == " in
    let final_buffer = string_parse_result buffer_w_op t2 in
    let final = final_buffer ^ ")" in
    final
  | Body l -> 
    let rec body_to_string buffer expr_list =
      (match expr_list with
      | [] -> buffer
      | hd::tl -> 
        let top = (string_parse_result buffer hd ) ^ "\n" in
        let final = body_to_string top tl in
        final
      )
    in 
    body_to_string buffer l
    ;;
;;

(* Caml.print_endline "";
let bufFor = For_loop(Tnum(5), Body([Add(Tnum(5), Tnum(1))])) in
let r = string_parse_result "" bufFor in
Caml.print_endline r;
Caml.print_endline "\n\nfinished";

let bufBool = Tbool(true) in
let bufBody = Body([bufFor; bufBool]) in
let buffer_1 = "" in
let result_1 = string_parse_result buffer_1 bufBody in
Caml.print_endline result_1 ;;
Caml.print_endline "\n\n" ;;


let expression = If_else (Equals(Tnum (1), Tnum (2)), Add(Tnum(12), Tnum(23)), Add(Tnum(100), Tnum(200))) in
let buffer = "" in
let result = string_parse_result buffer expression in
Caml.print_endline result ;; *)

let test_str = "(for 5 ((123 + 234) (234 + 345)))" in
let test_sexp = Sexp.of_string test_str in
let term_result = parse test_sexp in
pretty_print (string_parse_result "" term_result);;
  