(* ------------------------------------------------------------------------*)
(* Q 1 : Money in the bank (25 points)                                     *)
(* ------------------------------------------------------------------------*)

let new_account (p: passwd) : bank_account =
  let balance = ref 0 in
  let correct_passwd = ref p in
  let wrong_counter = ref 0 in
  {
    update_passwd = (fun (old:passwd) -> fun (neww:passwd) -> 
        if old = !correct_passwd then 
          (wrong_counter := 0;
           correct_passwd := neww)
        else (wrong_counter := !wrong_counter + 1;
              raise wrong_pass)
      );
      
    retrieve = (fun (givenp:passwd) -> fun (x:int) -> 
        if !wrong_counter >= 3 then raise too_many_attempts 
        else if givenp = !correct_passwd then
          (wrong_counter :=0;
           if x <= !balance then balance := !balance - x
           else raise no_money)
        else (wrong_counter := !wrong_counter + 1;
              if !wrong_counter > 3 then raise too_many_attempts
              else raise wrong_pass)
      );
    
    deposit = (fun (givenp:passwd) -> fun (x:int) -> 
        if !wrong_counter >= 3 then raise too_many_attempts 
        else if givenp = !correct_passwd then
          (balance := !balance + x;
           wrong_counter := 0)
        else (wrong_counter := !wrong_counter + 1;
              if(!wrong_counter > 3) then raise too_many_attempts
              else raise wrong_pass)
          
      );
    
    print_balance = (fun (givenp:passwd) -> 
        if !wrong_counter >= 3 then raise too_many_attempts 
        else if givenp = !correct_passwd then
          (wrong_counter := 0; !balance)
        else (wrong_counter := !wrong_counter + 1;
              if(!wrong_counter > 3) then raise too_many_attempts
              else raise wrong_pass) 
      ); 
  }
;;


(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)

(* Q 2.1 : Counting how many function calls are made *)

let rec fib_I (n: int) : fib_result =
  let counter_rec = ref 0 in 
  if n = 0 then (counter_rec := !counter_rec + 1;
                 {num_rec = !counter_rec; result = 0})
  else (if n = 1 then (counter_rec := !counter_rec + 1;
                       {num_rec = !counter_rec; result = 1})
        else (counter_rec := !counter_rec + 1;
              {num_rec = !counter_rec + (fib_I (n-2)).num_rec + (fib_I (n-1)).num_rec;
               result = (fib_I (n-2)).result + (fib_I (n-1)).result}))
;;


(* Q 2.2 : Memoization with a global store *)

let fib_memo (n: int) : int =
  let rec fib n = match (Hashtbl.find_opt store n) with
    | Some v -> v
    | None -> (Hashtbl.add store n (fib (n-2) + fib (n-1));
               (fib (n-2) + fib (n-1))) 
  in
  Hashtbl.add store 0 0;
  Hashtbl.add store 1 1;
  fib n
;;


(* Q 2.3 : General memoization function *) 

let memo (f: (('a -> 'b) -> 'a -> 'b)) (stats: stats) : ('a -> 'b) =
  let store : ('a, 'b) Hashtbl.t = Hashtbl.create 1000 in 
  fun (a:'a) -> 
    let rec f_memo (a: 'a) : 'b = match (Hashtbl.find_opt store a) with 
      | Some v -> (stats.lkp := !(stats.lkp) + 1; v)
      | None -> (stats.entries := !(stats.entries) + 1;
                 let v = f (f_memo) a in 
                 Hashtbl.add store a v; v)
    in f_memo a
;;


(* Q 2.4 : Using memo to efficiently compute the Fibonacci number *)
(* We also accept let fibM = raise NotImplemented ;; *)
let fibM = 
  let stats = {entries = ref 0; lkp = ref 0} in 
  let f_memo = memo (fun g x->
      if x = 0 then 0
      else if x = 1 then 1
      else (g(x-2) + g(x-1))) stats in 
  fun (n: int) ->
    f_memo n, stats
;;
