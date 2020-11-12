(* REVIEW 
 *)

(* Rewriting functions tail-recursively *)
let rec zip l1 l2 = match l1 , l2 with
  | [] , _ -> l2
  | _  , [] -> l1
  | x::xs , y::ys -> x::y::zip xs ys


let rec zip' l1 l2 sc = match l1, l2 with
  | [] , _ -> sc l2
  | _ , [] -> sc l1
  | x::xs , y::ys -> zip' xs ys (fun r -> sc (x::y::r))
                   
let listToString  l =
   List.fold_right (fun x r -> if r = "" then (string_of_int x) else (string_of_int x) ^ ", " ^ r) l ""


(* Using continuations for backtracking and for building a result *)  
exception NoChange
exception Change        

(* change : int list -> int -> int list *)
let rec change coins amt = 
  match coins with
  (* If the amount of change to make is zero, then we're done. *)
  | _ when amt = 0 -> []
  | [] ->
     (* if we run out of available coins but amt is nonzero, then we
        fail with an exception to jump back to the nearest handler. *)
     raise Change
  | coin :: cs when coin > amt ->
     (* if this coin is too large, we forget about it for now and
     consider the other coins *)
     change cs amt
  | coin :: cs ->
	   try 
       (* otherwise, we know this coin is good, so we add it to the
          return list and recursively make change for the remaining sum.
        *)
	     coin :: change coins (amt - coin)
	   with
     | Change ->
        (* If it turns out that by using `coin` we were unable to make
           the remaining change, then we forget about `coin` and try to
           make change for the full amount with the remaining coins.
         *)
        change cs amt
        

        
let rec change' coins amt sc  = 
  if amt = 0 then sc [] 
  else 
    begin match coins with 
      | [] -> raise NoChange
      | coin::coins ->  
	  if coin > amt then
	    change' coins amt sc
	  else 
	      try change' (coin::coins) (amt - coin) 
		    (fun r -> sc (coin::r))
	      with NoChange  -> change' coins amt sc
    end

  
let rec cchange coins amt sc fc = 
  if amt = 0 then sc [] 
  else 
    begin match coins with 
      | [] -> fc ()
      | coin::coins ->  
	  if coin > amt then
	    cchange coins amt sc fc
	  else 
	      cchange (coin::coins) (amt - coin) 
		(fun r -> sc (coin::r))
 		(fun () -> cchange coins amt sc fc)
    end

let give_change coins amt =
  begin try 
    let c = cchange coins amt (fun r -> r) (fun () -> raise NoChange) in
      print_string ("Return coins: " ^ listToString c ^ "\n")
    with NoChange -> print_string ("Sorry, I cannot give change\n")
  end 
