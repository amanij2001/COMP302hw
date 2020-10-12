exception NotImplemented
exception Fail

exception Msg of string

type passwd = string
type bank_account = {update_passwd  : passwd -> passwd -> unit;
                     retrieve       : passwd -> int -> unit;
                     deposit        : passwd -> int -> unit;
                     print_balance  : passwd -> int }

(* Bank account errors *)
let wrong_pass = Msg "Wrong Password"
let too_many_attempts = Msg "Change your password"
let no_money = Msg "Insufficient funds"


let rec fib n = if n = 0 then 0
                else (if n = 1 then 1 else fib (n-2) + fib (n-1))
     
type fib_result =
  { num_rec : int;
    result  : int }

type stats =
  { entries : int ref;
    lkp : int ref }

let store : (int, int) Hashtbl.t = Hashtbl.create 1000


(* ------------------------------------------------------------------------*)
(* Q 1 : Money in the bank (25 points)                                     *)
(* ------------------------------------------------------------------------*)

let new_account (p: passwd) : bank_account =
  raise NotImplemented
;;


(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)

(* Q 2.1 : Counting how many function calls are made *)

let rec fib_I (n: int) : fib_result =
  raise NotImplemented
;;


(* Q 2.2 : Memoization with a global store *)

let fib_memo (n: int) : int =
  let rec fib n =
    raise NotImplemented
  in
  fib n
;;


(* Q 2.3 : General memoization function *)

let memo (f: (('a -> 'b) -> 'a -> 'b)) (stats: stats) : ('a -> 'b) =
  raise NotImplemented
;;


(* Q 2.4 : Using memo to efficiently compute the Fibonacci number *)
(* We also accept let fibM = raise NotImplemented ;; *)
let fibM (n: int) : (int * stats) =
  raise NotImplemented
;;
