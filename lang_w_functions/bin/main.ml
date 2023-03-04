open Core;;

let pretty_print inp_str = 
  Caml.print_endline "";
  Caml.print_endline inp_str;
;;


type term = 
(* Primitive datatypes *)
| Tnum of int
| Tbool of bool
(* Control Structures *)
| If_else of term * term * term (* 1: condition; 2: body1; 3: body2*)
| For_loop of term * term 
(* Binary Operators *)
| Add of term * term
| Equality of term * term
| Assign of string * term
(* Identifier constructor -> for variables *)
(* 1.) name of the identifier 
   2.) a body of terms or simply a term *)
(* Should create a List for all identifiers created *)
| ID of string * term 
| Null of term option 
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
      (match operator with
      | Sexp.Atom "+" -> 
        let p_t1 = parse t1 in
        let p_t2 = parse t2 in
        Add(p_t1, p_t2)
      | Sexp.Atom "==" -> 
        let p_t1 = parse t1 in
        let p_t2 = parse t2 in
        Equality(p_t1, p_t2)
      | Sexp.Atom "=" -> 
        let p_t2 = parse t2 in
        (match t1 with
        | Sexp.Atom a ->
          Assign(a, p_t2)
        | _ -> failwith "Error: Invalid assignment"
        )
      | _ -> failwith "not a binary operator"
      )
    (* 3.) variable *)
    | [Sexp.Atom "let"; Sexp.Atom name; Sexp.Atom "="; value] ->
      let p_val = parse value in
      ID(name, p_val)
    (* 4.) body of expressions *)
    | _ -> Body (List.map ~f:parse l) (* parse each element in the list*)
    
    
    )
  | Sexp.Atom a -> (* "a" a string*)
    if (Str.string_match (Str.regexp "-?[0-9]+$") a 0) then Tnum (int_of_string a)
    else if (String.equal a "false") || (String.equal a "true") then Tbool (Bool.of_string a)
    else ID(a, Null(None)) (*otherwise, assume that it is a variable call*)
  ;;


let rec string_parse_result buffer t =
  match t with
  | Tnum n -> buffer ^ "Num (" ^ (Int.to_string n) ^ ")"
  | Tbool b -> buffer ^ "Bool (" ^ (Bool.to_string b) ^ ")"
  | Null _ -> buffer ^ "Null"
  | ID(name, literal) -> 
    let temp_buffer = buffer ^ "ID(" ^ name ^ ", " in
    let buffer_w_literal = string_parse_result temp_buffer literal in
    let final = buffer_w_literal ^ ")" in 
    final
  | If_else (condition, body_expr1, body_expr2) -> 
    let temp_buffer = buffer ^ "if (" in
    let temp_buffer_w_cond = (string_parse_result temp_buffer condition) ^ ")\n" in
    let temp_buffer_w_body1 = (string_parse_result temp_buffer_w_cond body_expr1) ^ "\nelse\n" in
    let final = string_parse_result temp_buffer_w_body1 body_expr2 in
    final
  | For_loop (num_iterations, body_expr) ->
    let temp_buffer = buffer ^ "for (" in
    let temp_buffer_w_iter = (string_parse_result temp_buffer num_iterations) ^ ")\n" in
    let final_buffer = string_parse_result temp_buffer_w_iter body_expr in
    let final = final_buffer in
    final
  (* printing binary operator expressions *)
  | Add (t1, t2) -> 
    let temp_buffer = buffer ^ "(" in
    let buffer_w_t1 = string_parse_result temp_buffer t1 in
    let buffer_w_op = buffer_w_t1 ^ " + " in
    let final_buffer = string_parse_result buffer_w_op t2 in
    let final = final_buffer ^ ")" in
    final
  | Equality(t1, t2)-> 
    let temp_buffer = buffer ^ "(" in
    let buffer_w_t1 = string_parse_result temp_buffer t1 in
    let buffer_w_op = buffer_w_t1 ^ " == " in
    let final_buffer = string_parse_result buffer_w_op t2 in
    let final = final_buffer ^ ")" in
    final
  | Assign(name, t1) -> 
    let temp_buffer = buffer ^ "(" ^ name ^ " = " in
    let buffer_w_literal = string_parse_result temp_buffer t1 in
    let final = buffer_w_literal ^ ")" in
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
    "Body(\n\n" ^ (body_to_string buffer l) ^ "\n)"
    ;;
;;

let find_var (name: string) (variables: (string * term) list) : (string * term) option =
  let search_result = Stdlib.List.find_opt (fun x -> match x with 
                                  | (possib_match, _ ) ->
                                    if String.equal name possib_match 
                                    then true
                                    else false) variables in
  search_result
                                  ;;
  


(* t is a series of term types; but it still registers as a term *)
(* variables starts off as an empty list *)
let rec eval (t : term) (variables : (string * term) list) : (term * (string * term) list) =
  match t with
  | Tnum _ -> (t, variables)
  | Tbool _ -> (t, variables)
  | ID (name, literal) ->
    let (e_literal, new_variables) = eval literal variables in
    (match find_var name new_variables with
    | Some (id_name, value) -> 
      (match e_literal with
      | Null(None) -> 
        (* You have to look for the latest value for that variable name *)
        (* let (var_name, var_literal) = find_var name new_variables in *)
        (ID(id_name, value), new_variables)
      | _ -> (t, new_variables)

      )
    | None -> 
      let new_variables2 = new_variables @ [(name, e_literal)] in
      (ID(name, e_literal), new_variables2))
   
      
  | Null _ -> (t, variables)
  | Assign(name, term_literal) ->
    (match find_var name variables with
    | Some (_, _) -> 
      let (new_literal, new_variables) = eval term_literal variables in
      
      
      let update_variables (x : string*term) : string*term =
        match x with 
        | (possib_name, old_literal) -> 
          if String.equal possib_name name then (name, new_literal)
          else (possib_name, old_literal)
        in
      let new_variables2 = List.map ~f:update_variables new_variables in
      let res_term = ID(name, new_literal) in      
      (res_term, new_variables2)
    | None -> failwith "Error: the variable doesn't exist"

    )
    
  | Add (t1, t2) ->
    
    let (e_t1, new_variables) = eval t1 variables in
    let (e_t2, new_variables2) = eval t2 new_variables in
    (match (e_t1, e_t2) with
      | (Tnum n1, Tnum n2) -> 
        
        (Tnum(n1 + n2), new_variables2)
        | (Tnum n1, ID(_, Tnum n2)) | (ID(_, Tnum n1 ), Tnum n2) ->
        (Tnum (n1 + n2), new_variables2)
        
      | _ -> failwith "Error: Adding"
    )
    
  | Equality (t1, t2) -> 
    let (e_t1, new_variables) = eval t1 variables in
    let (e_t2, new_variables2) = eval t2 new_variables in
    (match (e_t1, e_t2) with
    | (Tnum n1, Tnum n2) -> 
      if n1 = n2 then (Tbool(true), new_variables2) else (Tbool(false), new_variables2)
    | (Tbool b1, Tbool b2) ->
      if Bool.equal b1 b2 then (Tbool(true), new_variables2) else (Tbool(false), new_variables2)
    | _ -> failwith "Error: Equality"
    
    )
  | Body l -> (*Body(List.iter ~f:(fun x -> eval x) l) *)(* This is only a temporary solution *)
                                          (* Body should return a single value *)
      let rec eval_body (current_body : term list) (current_variables : (string * term) list) : (term * (string * term) list) =
        (match current_body with
        | [] -> (Null(None), current_variables)
        | [last_expr] -> 
          eval last_expr current_variables
        | hd::tl -> 
          let (_, new_variables) = eval hd current_variables in
          eval_body tl new_variables
        )
      in
      eval_body l variables
  | If_else (condition, body_expr1, body_expr2) -> 
    let (e_condition, new_variables) = eval condition variables in
    (match e_condition with
    | Tbool b -> 
      if b then (eval body_expr1 new_variables) else (eval body_expr2 new_variables)
    | _ -> failwith "Error: Condition is not a boolean"
    )
  | For_loop (num_iterations, body_expr) ->
    let (e_iterations, new_variables) = eval num_iterations variables in
    (match e_iterations with
    | Tnum n -> 
      let rec for_loop_func (i : int) (body_expr: term) (current_variables: (string * term) list) : (term * (string * term) list) =
        if i = 1 then
          eval body_expr current_variables
        else 
          let next_idx = i - 1 in
          let (_, e_current_variables) = for_loop_func next_idx body_expr current_variables in
          eval body_expr e_current_variables
          (* true *)
      in
      for_loop_func n body_expr new_variables
    | _ -> failwith "Error: iterations expects a number"

    ) 
;;


let print_result expression = 
  let s_expr = Sexp.of_string expression in
  let p_expr = parse s_expr in
  let result = string_parse_result "" p_expr in
  pretty_print "**Printing out AST**" ;
  pretty_print result;
  
  let (evald_result, _) = eval p_expr [] in
  pretty_print "**This is the result**";
  pretty_print (string_parse_result "" evald_result)
;;

pretty_print "";
(*
(* Testing if else expressions *)
pretty_print "*****Testing if-else expressions with variable definition*****"; 
let expression = "((let x = 0) (if ((2 + 2) == (3 + 1)) (x = (x + 5)) else (x = (x + 100)) ) )" in
let s_expr = Sexp.of_string expression in
(* let expression = If_else (Equality(Tnum (1), Tnum (2)), Add(Tnum(12), Tnum(23)), Add(Tnum(100), Tnum(200))) in *)
let p_expr = parse s_expr in
let result = string_parse_result "" p_expr in 
pretty_print "**Printing out AST**";
pretty_print result;
let (evald_result, _) = eval p_expr [] in
pretty_print "**This is the result**";
pretty_print (string_parse_result "" evald_result);; *)


pretty_print "*****Testing if-else expressions with variable definition*****"; 
let expression = "((let x = 0) (if ((2 + 2) == (3 + 1)) (x = (x + 5)) else (x = (x + 100)) ) )" in
print_result expression;

pretty_print "----------------------";;

(* Testing for loop expressions *)
(* pretty_print "*****Testing for loop expressions with variable definition*****";
let expression = "((let x = 0) (for 3 (x = (x + 10))))" in
let s_expr = Sexp.of_string expression in
let p_expr = parse s_expr in
pretty_print "**Printing out AST**" ;
pretty_print (string_parse_result "" p_expr);
let (evald_result, _ ) = eval p_expr [] in
pretty_print "**This is the result**";
pretty_print (string_parse_result "" evald_result);; *)

pretty_print "*****Testing for loop expressions with variable definition*****";
let expression = "((let x = 0) (for 3 (x = (x + 10))))" in
print_result expression;












(* let test_str = "((let x = 5) (x = (x + 2)))" in
let test_sexp = Sexp.of_string test_str in
let term_result = parse test_sexp in
pretty_print (string_parse_result "" term_result);

let (e_term, _) = eval term_result [] in
pretty_print (string_parse_result "" e_term);; *)
  
(* pretty_print "----------------";;

let test_str2 = "((let x = 10) (let y = (x + 199)))" in
let test_sexp2 = Sexp.of_string test_str2 in
let term_result2 = parse test_sexp2 in
pretty_print (string_parse_result "" term_result2);
pretty_print "before eval ^^^"; *)

(* let (e_term2, _) = eval term_result2 [] in 
pretty_print (string_parse_result "" e_term2);; *)
(* This case shows that you have to evaluate the body one more time *)

