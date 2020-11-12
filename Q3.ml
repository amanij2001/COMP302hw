type 'a susp = Susp of (unit -> 'a)
type 'a str = {hd: 'a ; tl : ('a str ) susp }
let force (Susp f) = f ()
let rec take n s = match n with
  | 0 -> []
  | n -> s.hd :: take (n-1) (force s.tl);;
                                          
(*Harmonic*) 
let rec reciprocal (n:int) : float str =
  {hd = 1./.(float) n;
   tl = Susp (fun () -> reciprocal (n+1))};;

let harmonic_number (n:int) (str: float str): float =
  let rec harmonic n str acc =
    if n=0 then acc
    else harmonic (n-1) (force str.tl) (str.hd +.acc)
  in harmonic n str 0.;;
(*End Harmonic*)

let rec addFltStr (s1: float str) (s2: float str) : float str =
  {hd = s1.hd +. s2.hd;
   tl = Susp (fun () -> addFltStr (force s1.tl) (force s2.tl))};;

let rec addIntStr (s1: int str) (s2: int str) : int str =
  {hd = s1.hd + s2.hd;
   tl = Susp (fun () -> addIntStr (force s1.tl) (force s2.tl))};;

let rec ones =
  {hd = 1;
   tl = Susp (fun () -> ones)}

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

type bin_code = int list;;
exception Error;;
type hf_tree = Leaf of float * char | Node of float * hf_tree * hf_tree;;
let dna = Node(1., 
               (Node (0.075, 
                      (Leaf (0.025, 'g')), 
                      (Node (0.05, 
                             (Leaf (0.025, 't')), (Leaf (0.025, 'c')))))), 
               (Leaf (0.925, 'a')));;

let decode (t: hf_tree) (w: bin_code) : char list = 
  let rec dec t' w' = match t', w' with
    | Leaf(_, a), [] -> [a]
    | Leaf(_, a), w -> a::dec t w
    | Node (_, l, r), (0::w) -> dec l w
    | Node (_, l, r), (1::w) -> dec r w
    | _, [] -> raise Error
  in 
  if w = [] then [] else dec t w;;

let rec encode (t: hf_tree) (c: char) : bin_code = match t with 
  | Leaf (_, a) -> if a = c then [] else raise Error
  | Node (_, l, r) -> try 1::(encode r c) with Error -> 0::(encode l c);;

let rec path (c: char) (t: hf_tree) (fc: unit->bin_code) (sc: bin_code->bin_code)
  : bin_code = match t with
  | Leaf (_, v) -> if v = c then sc [] else fc ()
  | Node (_, l, r) -> 
      path c r (fun () -> 
          path c l fc (fun b -> sc (0::b))) 
        (fun b -> sc (1::b));;

let code (c: char) (t: hf_tree) = path c t (fun () -> raise Error) (fun x -> x);;

let rec fold (f: 'a->'b->'c) (s: 'a str) (a: 'b) : 'c str =
  {hd = f (s.hd) a;
   tl = Susp (fun () -> fold f (force s.tl) (f s.hd a))};;

let rec nats_from n =
  {hd = n;
   tl = Susp (fun () -> nats_from (n + 1))}
let nats = nats_from 1;;

let triangular_numbers =
  fold (fun x y -> x+y) nats 0;;

let goebels_series =
  let square n = n * n in
  let rec goebels xs n =
    {hd = xs/n;
     tl = Susp (fun () -> goebels (xs + (square (xs/n))) (n+1))}
  in 
  {hd = 1;
   tl = Susp (fun () -> goebels (1+ (square 1)) 1)} ;; 

let rec psums (str: int str) : int str =
  {hd = str.hd;
   tl = Susp (fun () -> addIntStr (force str.tl) (psums str))};;

let rec map f s =
  {hd = f s.hd;
   tl = Susp (fun () -> map f (force s.tl)) } ;;

let rec pascal : int str str =
  {hd = ones;
   tl = Susp (fun () -> map psums pascal)} ;;

let rec getNth (n: int) (s: int str) : int = match n with
  | 0 -> s.hd
  | n -> getNth (n-1) (force s.tl);;

let rec row (k: int) (s: (int str) str) : int list = 
  let rec aux n k s =
    if k = 0 then 
      [1]
    else 
      (getNth k s.hd)::(aux n (k-1) (force s.tl))
  in aux 0 k s;;

let triangle =
  let rec aux (k:int) (s: (int str) str) : int list str = 
    {hd = row k s;
     tl = Susp (fun () -> aux (k+1) pascal)}
  in aux 0 pascal