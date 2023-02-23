(* This file holds the types used for constructing an AST for C++ *)

(* C++ programs consist of:
  a main function
  classes which have (member variables; methods; and subclasses)
  variables and pointers
  *)

(* Consider pointers and references *)
type expr =
| Tint of int
| Tfloat of float
| Tchar of char
| Tstring of string
| Tbool of bool

(* Specifying types of IDs; functions, methods, 
  variables, and classes all have identifiers *)
module type ID = sig
  type t
  
  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

module Function_Name : ID
module Var_Name : ID
module Lib_Name : ID

type expr_type = (* Consider that a pointer may be returned too*)
| ETint
| ETfloat
| ETbool
| ETvoid


type block_expr =
| Block of loc * expr list

type param = 
| TParam of expr_type * Var_Name.t
(* functions have: 
   1.) method name (an Identifier type (ID))
   2.) parameters (maybe none)
   3.) {
   4.) body
   5.) }
   *)
type function_defn = 
| Tfunction of 
    Function_Name.t
    expr_type
    param list
    block_expr (* specifies the return type *)


type library_import = 
| TLibImp of Lib_Name.t

type program =
| TProgram of library_import list * function_defn list