exception NotImplemented

let fact n =
  let rec factorial n =
    if n = 0 then 1
    else  n * factorial (n - 1)
  in
  if n <= 0 then 1 else factorial n

let binom (n, k) =
  if n < k then 0.0
  else float (fact n) /. (float (fact k) *. float (fact (n - k)))

let dist_black n x (marblesTotal, marblesDrawn) =
  (binom (n, x) *. binom (marblesTotal - n, marblesDrawn - x))
  /. (binom (marblesTotal, marblesDrawn))

let tabulate f n =
  let rec tab n acc =
    if n < 0 then acc
    else tab (n-1) ((f n)::acc)
  in
  tab n []

let max_in_list l =
  let rec max_in_list' pos l =
    match l with
    | [] -> assert false
    | [h]  -> (pos, h)
    | h::t ->
      let (q, mx) = max_in_list' (pos + 1) t in
      if h < mx then (q, mx)
      else (pos, h)
  in
  let (pos, _) = max_in_list' 0 l in
  pos


 type ingredients = Chocolate | Orange | Almonds | Vanilla | Flour | BlackBeans

 type cake = Slice of ingredients list | Cake of cake * cake

                                            
let rec insert x l = match l with
  | [] -> [x]
  | y::ys -> if x = y then l else y::(insert x ys)
           
let rec union l1 l2 = match l2 with
  | [] -> l1
  | x::xs -> union (insert x l1) xs




(* TODO: Write some tests for tabulate. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let tabulate_tests: (((int -> int) * int) * int list) list = [
  (* Remember: your test cases should have this form:
     ((f, n), output)
     The following test case asserts that:
       tabulate (fun x -> x) (-1)
     should have the output []
  *)
  (((fun x -> x), -1), [])
]

(* TODO: Implement dist_table: (int * int) -> int -> float list *)
let dist_table ((marblesTotal, marblesDrawn): (int * int)) (x: int) : float list =
  raise NotImplemented

(* TODO: Write some test cases for is_empty. *)
let is_empty_tests: (float list list * bool) list = [
]

(* TODO: Implement is_empty: 'a list list -> bool *)
let is_empty (matrix: 'a list list) : bool =
  raise NotImplemented

(* TODO: Implement dist_matrix: int * int -> int list -> float list list *)
let dist_matrix ((total, drawn): int * int) (resultList: int list) : float list list =
  raise NotImplemented

(* TODO: Implement combined_dist_table: float list list -> float list *)
let rec combined_dist_table (matrix: float list list) =
  raise NotImplemented

(* Once you have implemented all the above functions, you can
   use this function to compute the maximum likelihood.
   You can try it on the given example by running:
     max_likelihood (6, 3) [2; 0; 1]
*)
let max_likelihood (total, drawn) resultList =
  max_in_list
    (combined_dist_table
       (dist_matrix (total, drawn) resultList))


(* TODO: Implement all: (ingredients list -> bool) -> cake -> bool *)
let rec all (p: (ingredients list -> bool)) (c: cake) : bool = 
  raise NotImplemented

(* TODO: Write some test cases for is_chocolate_cake. *)
let is_chocolate_cake_tests = [
]

(* TODO: Implement is_chocolate_cake: cake -> bool *)
let is_chocolate_cake (c: cake) : bool = 
  raise NotImplemented


(* TODO: Implement map: (ingredients list -> ingredients list) -> cake -> cake *)
let rec map (p: (ingredients list -> ingredients list)) (c: cake) = 
  raise NotImplemented  

(* TODO: Write some test cases for add_ingredient. *)
let add_ingredient_tests = [
]

(* TODO: Implement add_ingredient: ingredients -> cake -> cake *)
let add_ingredient (x: ingredients) (c: cake) : cake = 
  raise NotImplemented    

(* TODO: Implement fold_cake: (ingredients list -> 'a -> 'a) -> 'a -> cake -> 'a  *)
let rec fold_cake (f: (ingredients list -> 'a -> 'a)) (base: 'a) (c: cake) : 'a = 
  raise NotImplemented    


(* TODO: Implement get_all_ingredients: cake -> ingredients list *)
let get_all_ingredients (c: cake) : ingredients list = 
  raise NotImplemented