(* Q1
  
  Ex: Input: List 𝑙: [1 ; 2; ]
      Function 𝑓: fun x -> fun y -> x + y
        
      Output: scanLeft 𝑓 0 𝑙 ➔ [0; 1; 3] 
*)

let rec scanLeft (f: 'a -> 'a -> 'a) (a: 'a) (l: 'a list) : int list =
  match l with
  | [] -> [a]
  | x::xs -> a::scanLeft (f) (f a x) xs ;;

(* Q2
   Ex: Input: List 𝑙: [1 ; 2; 3]
              Function 𝑓: fun x -> x + x
         
      Output: map 𝑓 𝑙 ➔ [ 2; 4; 6]
        
     In general, output is [ 𝑓 𝑙0; 𝑓 𝑙1; 𝑓 𝑙2 ; … ]
*)

let rec map (f: 'a -> 'a) (l: 'a list) : 'a list = match l with
  | [] -> []
  | x:: xs -> (f x)::(map f xs) ;;

(* Q3
   Ex: Input: List 1 (𝑙1): [1; 2]
              List 2 (𝑙2) : [3; 4]
              Function 𝑓 : fun x -> fun y -> x + y
                
      Output: listCross 𝑓 𝑙1 𝑙2 ➔ [ 4 ; 5; 5; 6]
        
In general, output is [𝑓 𝑙10𝑙20 ; 𝑓 𝑙10𝑙21 ; … ; 
                                    𝑓 𝑙11𝑙20 ; 𝑓 𝑙11𝑙21 ; …]
*)



let rec listCross (f: 'a->'a->'a) (l1: 'a list) (l2: 'a list) : 'a list=
  match (l1, l2) with
  | ([], _)-> []
  | (x::xs, y) -> (map (f x) y) @ listCross f xs y



