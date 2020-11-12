type 'a susp = Susp of (unit -> 'a)
let force = fun (Susp f) -> f ()
type 'a stream =
  { hd: 'a;
    tl: 'a stream susp }
  
let rec nats_from n =
  { hd = n;
    tl = Susp (fun () -> nats_from (n + 1))}
let nats = nats_from 0
    
type 'a tree =
  { data: 'a;
    left: 'a tree susp;
    right: 'a tree susp; }
  
let rec lazy_tree n =
  {data = n;
   left = Susp (fun () -> lazy_tree (n+1));
   right = Susp (fun () -> lazy_tree (n+2))
  }
  
type traversal_choice = Left | Right;;

let rec traverse tree (list: traversal_choice list) : 'a tree = match list with 
  | [] -> tree
  | Left::ds -> traverse (force tree.left) ds
  | Right:: ds -> traverse (force tree.right) ds