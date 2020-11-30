module Stack = 
struct
  type stack = int list
  let empty () : stack = [] 
  let push (i: int) (s: stack) = i::s
  let is_empty (s:stack) = match s with
    | [] -> true
    | _::_ -> false
  let pop (s:stack) = match s with
    | [] -> None
    | _::t -> Some t
  let top (s:stack) = match s with
    | [] -> None
    | h::_ -> Some h
  let stack2list (s:stack) = s
end ;;

module type INTSTACK =
sig
  type stack
  val empty : unit -> stack
  val push : int -> stack -> stack
  val is_empty : stack -> bool
  val pop : stack -> stack option
  val top : stack -> int option
  val size : stack -> int
  val stack2list : stack -> int list
end;;

module Stack : INTSTACK =
struct
  type stack = int list
  let empty () : stack = [] 
  let push (i: int) (s: stack) = i::s
  let is_empty (s:stack) = match s with
    | [] -> true
    | _::_ -> false
  let pop (s:stack) = match s with
    | [] -> None
    | _::t -> Some t
  let top (s:stack) = match s with
    | [] -> None
    | h::_ -> Some h
  let size (s: stack) = 
    let rec getsize stack n = match stack with
      | [] -> n
      | _::xs -> getsize xs (n+1)
    in getsize s 0
  let stack2list (s:stack) = s
end;;

module type STACK =
sig
  type stack
  type e1
  val empty : unit -> stack
  val push : e1 -> stack -> stack
  val is_empty : stack -> bool
  val pop : stack -> stack option
  val top : stack -> e1 option
  val size : stack -> int
  val stack2list : stack -> e1 list
end;;

module IntStack : (STACK with type e1 = int) =
struct 
  type e1 = int
  type stack = int list
  let empty () : stack = [] 
  let push (i: int) (s: stack) = i::s
  let is_empty (s:stack) = match s with
    | [] -> true
    | _::_ -> false
  let pop (s:stack) = match s with
    | [] -> None
    | _::t -> Some t
  let top (s:stack) = match s with
    | [] -> None
    | h::_ -> Some h
  let size (s: stack) = 
    let rec getsize stack n = match stack with
      | [] -> n
      | _::xs -> getsize xs (n+1)
    in getsize s 0
  let stack2list (s:stack) = s
end ;; 

module type RESOURCE =
sig 
  type resource
  val unit : resource
  val increase : int -> resource -> resource
  val decrease : int -> resource -> resource
  val toString : resource -> string
end;;

module IntResource : RESOURCE =
struct 
  type resource = int
  let unit = 0
  let increase (x: int) (r: resource) = r+x
  let decrease (x: int) (r: resource) = r-x
  let toString (r: resource) = string_of_int r
end;;

module type MANAGER =
sig
  type manager
  type resource
  val create : unit -> manager
  val increment : manager -> manager
  val decrement : manager -> manager
  val getCount : manager -> resource
  val setCount : int -> manager
  val toString : manager -> string
end;;

module Exp = struct
  type primop = Equals | LessThan | Plus | Minus | Times | Negate 
              | Divide | MoreThan | And | Or | Not | Xor

  type exp =
    | Int of int                      (* 0 | 1 | 2 | ... *)
    | Bool of bool                    (* true | false *)
    | If of exp * exp * exp           (* if e then e1 else e2 *)
    | Primop of primop * exp list     (* e1 <op> e2  or  <op> e *)
end        

module Eval = struct 
  open Exp 
      
  exception Stuck of string 
      
  let evalOp op = match op with
    | (Equals,   [Int i; Int i']) -> Some (Bool (i = i'))
    | (LessThan, [Int i; Int i']) -> Some (Bool (i < i'))
    | (Plus,     [Int i; Int i']) -> Some (Int (i + i'))
    | (Minus,    [Int i; Int i']) -> Some (Int (i - i'))
    | (Times,    [Int i; Int i']) -> Some (Int (i * i'))
    | (Negate,   [Int i])         -> Some (Int (-i))
    | (Divide,   [Int i; Int i']) -> if i' = 0 then None else Some (Int (i/i'))
    | (MoreThan, [Int i; Int i']) -> Some (Bool (i > i'))
    | (And,      [Bool b; Bool b']) -> Some (Bool (b && b'))
    | (Or,       [Bool b; Bool b']) -> Some (Bool (b || b'))
    | (Not,      [Bool b])          -> Some (Bool (not b))
    | (Xor,      [Bool b; Bool b']) -> Some (Bool (not (b = b')))
    | _                           -> None
                                   
  let rec eval e = match e with 
    | Int _ -> e
    | Bool _ -> e
    | If(e, e1, e2) ->
        (match eval e with
         | Bool true -> eval e1
         | Bool false -> eval e2
         | _ -> raise (Stuck "guard is not a bool"))
    (* primitive operations +, -, *, <, = *)
    | Primop (po, args) ->
        let argvalues = List.map eval args in
        (match evalOp (po, argvalues) with
         | None -> raise (Stuck "Bad arguments to primitive operation")
         | Some v -> v)
end

module Types = struct
  module E = Exp
           
  type tp = Int | Bool
                
  let typ_to_string t = match t with 
    | Int -> "Int"
    | Bool -> "Bool"
            
  exception TypeError of string
                       
  let fail message = raise (TypeError message)
                   
  (* primopType p = (argTypes, returnType) *)
  let primopType p = match p with
    | E.Equals   -> ([Int; Int], Bool)
    | E.LessThan -> ([Int; Int], Bool)
    | E.Plus     -> ([Int; Int], Int)
    | E.Minus    -> ([Int; Int], Int)
    | E.Times    -> ([Int; Int], Int)
    | E.Negate   -> ([Int], Int)
    | E.Divide   -> ([Int; Int], Int)
    | E.MoreThan -> ([Int; Int], Bool)
    | E.And      -> ([Bool; Bool], Bool)
    | E.Or       -> ([Bool; Bool], Bool)
    | E.Not      -> ([Bool], Bool)
    | E.Xor      -> ([Bool; Bool], Bool)
                  
                  
  let rec infer e = match e with 
    | E.Int _ -> Int
    | E.Bool _ -> Bool
    | E.If (e, e1, e2) -> 
        (match infer e with 
         | Bool -> let t1 = infer e1 in 
             let t2 = infer e2 in 
             if t1 = t2 then t1 
             else fail ("Expected " ^ typ_to_string t1 ^ 
                        " - Inferred " ^ typ_to_string t2)
         | t -> fail ("Expected Bool\nInferred " ^ typ_to_string t))
    | E.Primop (po, args) -> 
        let (expected_arg_types, resultType) = primopType po in 
        let inferred_arg_types = List.map infer args in 
       
        let rec compare tlist1 tlist2 = match tlist1, tlist2 with 
          | [] , [] -> resultType 
          | t::tlist , s::slist -> 
              if t = s then compare tlist slist
              else fail ("Expected " ^ typ_to_string t ^ 
                         " - Inferred " ^ typ_to_string s)
          | _ , _ -> fail ("Error: Primitve operator used with incorrect number of arguments")
        in 
        compare expected_arg_types inferred_arg_types 
       
end 

module E = Exp

let e3 = Exp.Primop (Exp.MoreThan, [Exp.Int 3; Exp.Int 2])
let e4 = Exp.Primop (Exp.Divide, [Exp.Int 3; Exp.Int 2])
let e5 = Exp.Primop (Exp.Divide, [Exp.Int 3; Exp.Int 0])
let e6 = E.Primop (E.And, [E.Bool true; E.Primop (E.Equals, [E.Int 3; E.Int 3])])
let e7 = E.Primop (E.Or, [E.Bool false; E.Primop (E.Equals, [E.Int 3; E.Int 3])])
let e8 = E.Primop (E.Not, [E.Primop (E.Equals, [E.Int 3; E.Int 3])])
let e9 = E.Primop (E.Xor, [E.Bool true; E.Primop (E.Equals, [E.Int 3; E.Int 3])])
