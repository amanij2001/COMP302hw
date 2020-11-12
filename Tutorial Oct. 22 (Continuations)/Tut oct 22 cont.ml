let rec sum_cont (l: int list) (cont: int -> int) : int = 
  match l with
  | [] -> cont 0 
  | x::xs -> sum_cont xs (fun s -> cont (s+x));;

let rec fold_right (f: 'a -> 'b -> 'b) (al: 'a list) (b: 'b) : 'b = 
  let rec fold_right' l k = match l with
    | [] -> k b
    | x::xs -> fold_right' xs (fun r -> k (f x r))
  in fold_right' al (fun x -> x);;

let rec fold_right2 (f: 'a -> 'b -> 'b) (al: 'a list) (b: 'b) (cont: 'b -> 'b): 'b = match al with
  | [] -> cont b
  | x::xs -> fold_right2 f xs b (fun r -> cont (f x r)) ;;

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree;;

let rec sum_tr tree target sum fail sc = match tree with
  | Empty -> fail ()
  | Node (r,d,l)-> if (sum + d) = target then sc ()
      else sum_tr l target (sum+d) (fun () -> sum_tr r target (sum+d) fail sc) sc  
  
let sum_tr' tree target = sum_tr tree target 0 (fun () -> false) (fun () -> true);;

let t = Node (Node (Node (Empty, 4, Empty), 2, Node (Empty, 5, Empty)), 1,
              Node (Node (Empty, 6, Empty), 3, Node (Empty, 7, Empty)));;