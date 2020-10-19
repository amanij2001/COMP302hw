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
  (((fun x -> x), -1), []);
  (((fun x -> x*x), 0), [0]);
  (((fun x -> x*5), 5), [0; 5;10;15;20;25]);
]

(* TODO: Implement dist_table: (int * int) -> int -> float list *)
let dist_table ((marblesTotal, marblesDrawn): (int * int)) (x: int) : float list = 
  tabulate (fun n-> (binom(n, x) *. 
                     binom((marblesTotal-n), 
                           (marblesDrawn-x)))/.
                    binom(marblesTotal,
                          marblesDrawn)) marblesTotal 
(* TODO: Write some test cases for is_empty. *)
let is_empty_tests: (float list list * bool) list = [
  (([[]]), true);
  (([[];[];[];[]]), true);
  (([[];[4.; 6.7]]), false);
  (([[5.]; []; []; []]), false);
  (([[4.6;8.2]]), false);
  (([[6.2; 34.7;12.3];[7.4];[5563.2; 2387.3; 2483.]]), false)
]

(* TODO: Implement is_empty: 'a list list -> bool *)
let is_empty (matrix: 'a list list) : bool =
  List.for_all (fun (x: 'a list) -> x=[]) matrix

(* TODO: Implement dist_matrix: int * int -> int list -> float list list *)
let dist_matrix ((total, drawn): int * int) (resultList: int list) : float list list =
  List.map (dist_table (total, drawn)) resultList

(* TODO: Implement combined_dist_table: float list list -> float list *)
let rec combined_dist_table (matrix: float list list) =
  if is_empty matrix then []
  else
    List.fold_left (fun x1-> fun x2 -> x1*.x2) (List.hd (List.map List.hd matrix)) 
      (List.tl (List.map List.hd matrix))::combined_dist_table (List.map List.tl matrix)

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
let rec all (p: (ingredients list -> bool)) (c: cake) : bool = match c with
  | Slice s -> p s
  | Cake (c1, c2) -> (all p c1) && (all p c2)

(* TODO: Write some test cases for is_chocolate_cake. *)
let is_chocolate_cake_tests = [
  ((Slice []), false);
  ((Slice [Flour; Orange; Almonds]), false);
  ((Slice [Vanilla; Flour; Chocolate; BlackBeans]), true);
  ((Cake(Slice [Flour; Vanilla] , Slice [Almonds])), false);
  ((Cake (Slice [Chocolate; Almonds], Slice [Flour; Orange])), false);
  ((Cake (Slice [BlackBeans; Chocolate; Flour], Slice [Chocolate; Vanilla])), true);
  ((Cake (Cake(Slice [Almonds], Slice[Chocolate]) , 
          Cake(Slice[Vanilla;Chocolate], Slice[Chocolate; Flour]))), false);
  ((Cake (Cake(Slice [Almonds; Chocolate], Slice[Chocolate]) , 
          Cake(Slice[Vanilla;Chocolate], Slice[Chocolate; Flour]))), true);
  
]

(* TODO: Implement is_chocolate_cake: cake -> bool *)
let is_chocolate_cake (c: cake) : bool = 
  let rec check (i: ingredients) (l: ingredients list) = match l with
    | [] -> false
    | x::xs -> (x = i) || (check i xs)
  in
  all (check Chocolate) c


(* TODO: Implement map: (ingredients list -> ingredients list) -> cake -> cake *)
let rec map (p: (ingredients list -> ingredients list)) (c: cake) = match c with
  | Slice s -> Slice (p s)
  | Cake (c1, c2) -> Cake (map p c1, map p c2)

(* TODO: Write some test cases for add_ingredient. *)
let add_ingredient_tests = [
  ((Chocolate, (Slice [])), Slice [Chocolate]);
  ((Chocolate, (Slice [Orange; Almonds])), Slice [Orange; Almonds; Chocolate]);
  ((Chocolate, (Slice [Chocolate; Almonds; Vanilla])), Slice [Chocolate; Almonds; Vanilla]);
  ((Chocolate, (Cake (Slice [Almonds], Slice [Vanilla; Flour]))), 
   Cake (Slice [Almonds; Chocolate], Slice [Vanilla; Flour; Chocolate]));
  ((Chocolate, (Cake (Slice [Chocolate], Slice [Chocolate; Vanilla; Flour]))), 
   Cake (Slice [Chocolate], Slice [Chocolate; Vanilla; Flour]));
]

(* TODO: Implement add_ingredient: ingredients -> cake -> cake *)
let add_ingredient (x: ingredients) (c: cake) : cake = 
  map (insert x) c 

(* TODO: Implement fold_cake: (ingredients list -> 'a -> 'a) -> 'a -> cake -> 'a  *)
let rec fold_cake (f: (ingredients list -> 'a -> 'a)) (base: 'a) (c: cake) : 'a = 
  match c with
  | Slice s -> f s base
  | Cake (c1, c2) -> fold_cake f (fold_cake f base c1) c2


(* TODO: Implement get_all_ingredients: cake -> ingredients list *)
let get_all_ingredients (c: cake) : ingredients list = 
  let rec get_first (c:cake) = match c with 
    | Slice s-> s
    | Cake (c1, c2) -> get_first c1
  in
  fold_cake union (get_first c) c