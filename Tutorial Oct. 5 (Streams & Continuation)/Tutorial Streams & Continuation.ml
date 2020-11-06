(*Harmonic*)

type 'a susp = Susp of (unit -> 'a)
type 'a str = {hd: 'a ; tl : ('a str ) susp }
let force (Susp f) = f ()
let rec take n s = match n with
  | 0 -> []
  | n -> s.hd :: take (n-1) (force s.tl)
           
let rec reciprocal (n:int) : float str =
  {hd = 1./.(float) n;
   tl = Susp (fun () -> reciprocal (n+1))};;

let harmonic_number (n:int) (str: float str): float =
  let rec harmonic n str acc =
    if n=0 then acc
    else harmonic (n-1) (force str.tl) (str.hd +.acc)
  in harmonic n str 0.;;

let rec addFltStr (s1: float str) (s2: float str) : float str =
  {hd = s1.hd +. s2.hd;
   tl = Susp (fun () -> addFltStr (force s1.tl) (force s2.tl))};;

let rec partial_sums (str: float str) : float str =
  {hd = str.hd;
   tl = Susp (fun () -> addFltStr (force str.tl) (partial_sums str))};;

type 'a binary_tree =
  | Empty
  | Node of 'a * ('a binary_tree) * ('a binary_tree);;

(*tree_height_tail : 'a binary_tree -> int*)
(*This one is NOT tail recursive b/c when we call recursively, it's not the last
thing we do; no continuation*)
let rec tree_height (tree: 'a binary_tree) : int =  match tree with
  | Empty -> 0
  | Node (a, t1, t2) -> 
      let h1 = tree_height t1 in
      let h2 = tree_height t2 in
      if h1 > h2 then h1+1
      else h2 +1 ;;
(*One with helper function with continuation*)       
let rec tree_height_cont (tree: 'a binary_tree) : int =  
  let rec height tree (cont: int -> int) : int = match tree with
    | Empty -> cont 0
    | Node (_, t1, t2) -> 
        height t1 (fun h1 -> 
            height t2 (fun h2 -> if h1 > h2 then cont (h1+1)
                        else cont (h2+1)))
  in height tree (fun x->x);;

let t = Node (1, Empty, Node(2, Node (3, Empty, Empty), Node (4, Empty,
                                                              Empty)));;