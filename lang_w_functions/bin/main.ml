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
| CallID of string (*meant for function calls and a call to a variable*)

(* Function identifier constructor  *)
(* 1.) name of the function
   2.) parameter list
   3.) body of the function *)
(* Should be added to the list of all identifiers created *)
| FuncID of string * term * term 
(* FuncID composed of: funcName, Params, Body *)
| CallFuncID of string * term
| Formal_Params of string list
(* Parameters: temporarily hold values for the parameters
   Values must be reset after the function has been run *)
| Actual_Params of term list
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
    (* 4.) function definition *)
    | [Sexp.Atom "function"; Sexp.Atom name; Sexp.List formal_params; body] ->
      let p_body = parse body in
      let parse_params p = (*"p" is/should be a single Sexp.atom*)
        (match p with
        | Sexp.Atom p_name -> 
          p_name
        | _ -> failwith "This is not a valid parameter name"
        )
      in
      let p_list = List.map ~f:parse_params formal_params in (*list of strings = parameter names*)
      let p_params = Formal_Params(p_list) in
      FuncID(name, p_params, p_body)
    | [Sexp.Atom func_name; Sexp.Atom "|"; Sexp.List actual_params; Sexp.Atom "|"] -> (*function call*) (*NOTE: THERE MUST BE SMTH IN FOR actual_params; so even if function has no parameters, then there still must be an empty () in place*)
      let parse_params p = (*"p" is/should be a single Sexp.atom*)
        (match p with
        | Sexp.Atom _ -> 
          parse p
        | _ -> failwith "This is not a valid parameter name"
        )
      in
      let p_list = List.map ~f:parse_params actual_params in
      let p_actual_params = Actual_Params(p_list) in
      CallFuncID(func_name, p_actual_params)


    (* 6.) body of expressions *)
    | _ -> Body (List.map ~f:parse l) (* parse each element in the list*)
    
    
    )
  | Sexp.Atom a -> (* "a" a string*)
    if (Str.string_match (Str.regexp "-?[0-9]+$") a 0) then Tnum (int_of_string a)
    else if (String.equal a "false") || (String.equal a "true") then Tbool (Bool.of_string a)
    else CallID(a) (*ID(a, Null(None))*) (*otherwise, assume that it is a variable call*)
  ;;

(* Parses the AST tree created from the parser to a string*)
(* TODO: CallID and funcID *)
let rec string_parse_result buffer t =
  match t with
  | Tnum n -> buffer ^ "Num (" ^ (Int.to_string n) ^ ")"
  | Tbool b -> buffer ^ "Bool (" ^ (Bool.to_string b) ^ ")"
  | Null _ -> buffer ^ "Null"
  | ID(name, literal) -> 
    let temp_buffer = buffer ^ "ID(" ^ name ^ ", " in
    let buffer_w_literal = string_parse_result temp_buffer literal in
    let final = buffer_w_literal ^ ")\n" in 
    final
  | FuncID(name, parameters, body) ->
    let temp_buffer = buffer ^ "Function " ^ name ^ " | " in
    let param_buffer = string_parse_result temp_buffer parameters ^ " | \n" in
    let final = string_parse_result param_buffer body in
    final
  | CallID(name) ->
    let final = buffer ^ "Calling (" ^ name ^ ")" in
    final
  | CallFuncID(name, params) ->
    let name_buffer = buffer ^ "Calling function (" ^ name ^ ") | Parameters passed in (" in
    let final = string_parse_result name_buffer params ^ " )\n" in
    final
  | Formal_Params(list_of_names) ->
    let bufferParams init elem =
      let temp = " " ^ init ^ " " ^ elem ^ " " in
      temp
    in 
    let final = List.fold list_of_names ~f:bufferParams ~init:"" in
    final ^ "\n"
  | Actual_Params(list_of_terms) ->
    let bufferParams init elem = 
      let temp = " " ^ init ^ " " ^ (string_parse_result "" elem) ^ " " in
      temp  
    in
    let final = List.fold list_of_terms ~f:bufferParams ~init:"" in
    final ^ "\n"
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


(* TODO: will be replaced; this is unnecessary when you have a hashtbl *)
(* let find_var (name: string) (variables: (string * term) list) : (string * term) option =
  let search_result = Stdlib.List.find_opt (fun x -> match x with 
                                  | (possib_match, _ ) ->
                                    if String.equal name possib_match 
                                    then true
                                    else false) variables in
  search_result
                                  ;; *)
  


(* TODO: 
1.) implement hashtbl functionality for variables and obtaining their values 
-originaly functionality: list of tuples -> (var name: string, literal: term)
-new functionality: use hashtbl with keys and values -> hashtbl[key] -> value
2.) implement functions
instead of using an array   

*)





(* using hash tables
   let my_hash = Hashtbl.create (module key_type);;
   Hashtbl.add my_hash "key" keyvalue;;
   Hashtbl.find my_hash "key";;
   --returns a Some variant--
*)

let variables = Hashtbl.create (module String);;
let functions = Hashtbl.create (module String);;
let formal_w_actual_params = Hashtbl.create (module String)

let print_hash ~key ~data buffer = 
  buffer ^ "Name: " ^ key ^ " |||| Value: " ^ (string_parse_result "" data) ^ "\n"
;;

let find_var tbl name = 
  Caml.print_endline ("\n\n** Printing out variable: " ^ name ^ " **\n");
  Caml.print_endline (Hashtbl.fold tbl ~f:print_hash ~init:"");
  (match (Hashtbl.find tbl name) with
  | Some t ->
    Caml.print_endline(string_parse_result "" t)
  | _ -> failwith ("variable not found: " ^ name)
  )
;;
(* t is a series of term types; but it still registers as a term *)
(* variables starts off as an empty Hashtbl *)
(* Must always carry variables because the ones created must be remembered
   so that they can be used in the future *)

let rec eval (t : term) : (term) =
  match t with
  | Tnum _ -> t (*(t, variables)*)
  | Tbool _ -> t (*(t, variables)*)
  | ID (name, literal) -> (*this is used to create a new variable*)
    let e_literal = eval literal in
    ignore(Hashtbl.add variables ~key:name ~data:e_literal);
    
    (* find_var variables name; *)


    e_literal
  | CallID (name) ->
    (* find_var variables "x"; *)
    if Hashtbl.length formal_w_actual_params <> 0 then
      (match (Hashtbl.find formal_w_actual_params name) with
      | Some term_value -> eval term_value
      | None -> 
        (match (Hashtbl.find variables name) with 
        | Some term_value -> eval term_value
        | None -> 
          let error_string = "\n\nError: Variable called was not found -> " ^ name ^ " \n These are the variables:\n" in

          let variables_buff = Hashtbl.fold variables ~f:print_hash ~init:error_string in
          let params_buff = Hashtbl.fold formal_w_actual_params ~f:print_hash ~init:variables_buff in
          Caml.print_endline params_buff;
          failwith "error"

          
        )    
      )
    else 
      (match (Hashtbl.find variables name) with 
      | Some term_value -> eval term_value
      | None ->  

        let error_string = "\n\nError: Variable called was not found -> " ^ name ^ " \n These are the variables:\n" in

        Caml.print_endline (Hashtbl.fold variables ~f:print_hash ~init:error_string);
        failwith "error"
      )
  
  | FuncID (name, formal_params, body) -> (*must store the function name, parameters, and body*)
    (* Don't evaluate anything: there's nothing to evaluate, just store things in hashtable 'functions' *)
    ignore(Hashtbl.add functions ~key:name ~data:(formal_params, body));
    FuncID(name, formal_params, body)
    (* TODO: CallID will handle calls to a variable or function 
      1.) Must be able to parse a CallID from s-expressions into term
      2.) then you can evaluate   
    *)
    

    (* (match find_var name new_variables with
    | Some (id_name, value) -> (*if the variable was found*)
      (match e_literal with
      | Null(None) -> 
        (* You have to look for the latest value for that variable name *)
        (* let (var_name, var_literal) = find_var name new_variables in *)
        (ID(id_name, value), new_variables) these both act as calls to a variable
      | _ -> (t, new_variables)

      )
    | None -> (*if the variable was not found*)
      let new_variables2 = new_variables @ [(name, e_literal)] in
      (ID(name, e_literal), new_variables2)) *)
   
    (* TODO: funcID and CallID *)
  | CallFuncID(name, actual_params) -> (*actual_params is a list of terms*)
    (*1. look up the function in the functions hashtbl *)
    (*2. create a hash table with each parameter name matched to the actual parameter*)
    (*3. create an instance of CallFuncID*)
    (match (Hashtbl.find functions name) with (*remember that Params is a list of strings*)
    | Some(formal_params, body) ->
      (match formal_params with
      | Formal_Params form_list ->
        (match actual_params with
        | Actual_Params actual_list ->
          ignore(Hashtbl.clear formal_w_actual_params);
          if List.length form_list = List.length actual_list then
            let zip_params (name:string) (value:term) : (unit) = 
              ignore(Hashtbl.add formal_w_actual_params ~key:name ~data:value)
            in
            ignore (List.iter2 ~f:zip_params form_list actual_list);
          else 
            failwith ("Lists are not the same length\n " ^
                    "# formal params: " ^ (string_of_int (List.length form_list)) ^
                    "# actual params: " ^ (string_of_int (List.length actual_list)))
          
        | _ -> failwith "Error: Actual parameters DNE; Must have empty list in place"
        )
      | _ -> failwith "Error: Formal parameters DNE; Must have empty list in place"

      );
      let result = eval body in
      ignore (Hashtbl.clear formal_w_actual_params);
      result (*the pipe operator |> can condense operations*)
      
      
      (*when evaluating a function, formal_w_actual_params will not be empty; meaning that there are
         parameters that should be looked at
         -When eval function and there are no params -> then look at variables*)


      
    | None -> failwith ("Error: Function called is not found -> " ^ name)
    
    
    
    )
  | Null _ -> t
  | Assign(name, term_literal) ->
    (* (match find_var name variables with
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

    ) *)

    (match (Hashtbl.find variables name) with 
      | Some _ ->
        let new_literal = eval term_literal in
        Hashtbl.remove variables name;
        ignore(Hashtbl.add variables ~key:name ~data:new_literal);
        new_literal
      | None -> failwith "Error: the variable was never created"
    )

    
  | Add (t1, t2) ->
    
    let e_t1 = eval t1 in
    let e_t2 = eval t2 in
    (match (e_t1, e_t2) with
      | (Tnum n1, Tnum n2) -> 
        Tnum(n1 + n2)
      (* | (Tnum n1, ID(_, Tnum n2)) | (ID(_, Tnum n1 ), Tnum n2) -> *)
      (* TODO: Defunct -> CallID term when evaluated should just return the literal held by the variable *)
      
      (* Tnum (n1 + n2) *)
        
      | _ -> failwith "Error: Adding"
    )
    
  | Equality (t1, t2) -> 
    let e_t1 = eval t1 in
    let e_t2 = eval t2 in
    (match (e_t1, e_t2) with
    | (Tnum n1, Tnum n2) -> 
      if n1 = n2 then Tbool(true) else Tbool(false)
    | (Tbool b1, Tbool b2) ->
      if Bool.equal b1 b2 then Tbool(true) else Tbool(false)
    | _ -> failwith "Error: Equality"
    
    )
  | Body l -> (*Body(List.iter ~f:(fun x -> eval x) l) *)(* This is only a temporary solution *)
                                          (* Body should return a single value *)
      let rec eval_body (current_body : term list) : (term) =
        (match current_body with
        | [] -> Null(None)
        | [last_expr] -> 
          eval last_expr
        | hd::tl -> 
          ignore(eval hd); (*only want to return the final result*)
          eval_body tl
        )
      in
      eval_body l
  | If_else (condition, body_expr1, body_expr2) -> 
    let e_condition = eval condition in
    (match e_condition with
    | Tbool b -> 
      if b then eval body_expr1 else eval body_expr2
    | _ -> failwith "Error: Condition is not a boolean"
    )
  | For_loop (num_iterations, body_expr) ->
    let e_iterations = eval num_iterations in
    (match e_iterations with
      | Tnum n -> 
        let rec for_loop_func (i : int) (body_expr: term) : (term) =
          if i = 1 then
            eval body_expr
          else 
            let next_idx = i - 1 in
            ignore(for_loop_func next_idx body_expr);
            eval body_expr 
            (* true *)
        in
        for_loop_func n body_expr 
      | _ -> failwith "Error: iterations expects a number"

    )
  | _ -> failwith "Fatal error in eval; Parameters do not have branch in match expression because they are \"evaluated\" at the CallFuncID branch"
;;


let print_result expression = 
  let s_expr = Sexp.of_string expression in
  let p_expr = parse s_expr in
  let result = string_parse_result "" p_expr in
  pretty_print "**Printing out AST**" ;
  pretty_print result;
  
  let evald_result = eval p_expr in
  pretty_print "**This is the result**";
  Hashtbl.clear variables; (*must reset the hash table *)
  pretty_print (string_parse_result "" evald_result)
;;

pretty_print "";

pretty_print "*****Testing if-else expressions with variable definition*****"; 
let expression = "((let x = 0) (if ((2 + 2) == (3 + 1)) (x = (x + 5)) else (x = (x + 100)) ) )" in
print_result expression;

pretty_print "----------------------";;


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

