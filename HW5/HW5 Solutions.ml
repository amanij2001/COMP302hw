(* TODO: Write some tests for neighbours. Consider creating a graph first,
and then writing your tests based on it *)

(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

let a = { nodes = ["a"; "b"; "c"; "d"; "e"];
          edges = [("a", "d", 50); ("a", "c", 40); ("d", "c", 60); ("b", "a" ,20)]
        }

(* We've added a type annotation here so that the compiler can help
   you write tests of the correct form. *) 
let neighbours_tests: ((string graph * string) * (string * weight) list) list = [
  ((a, "c"), ([]));
  ((a, "e"), ([]));
  ((a, "a"), ([("d", 50); ("c", 40)]));
  ((a, "b"), ([("a", 20)]));
  ((a, "d"), ([("c", 60)])); 
]

(* TODO: Implement neighbours. *)
let neighbours (g: 'a graph) (vertex: 'a) : ('a * weight) list =
  let m = g.edges in
  let rec neighbours' edges (acc: ('a*weight) list) =
    match edges with
    | [] -> acc
    | (v1, v2, w)::xs -> if v1 = vertex then neighbours' (xs) ((v2, w)::acc)
        else neighbours' xs acc
  in neighbours' m []
;;

(* TODO: Implement find_path. *)
let find_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node (node: 'a) visited: 'a list =
    if (List.exists (fun s->s=node) visited) then []
    else visited@[node]
  and aux_list (nodes:'a list) visited =
    raise NotImplemented 
  in
  (*let try_path = ref [] in
   let try_weight = ref 0 in*)
  let m = g.edges in
  let rec find_inner_path (edges: ('a*'a*weight) list) (a': 'a) : ('a list * weight) =
    match edges with
    | []-> raise Fail
    | (v1, v2, w)::[] -> if (v1=a') && (v2=b) then (!try_path@[b], !try_weight+w)
        else raise Fail
    | (v1, v2, w)::xs ->if ((v1=a) && (List.exists (fun s->s=v2) !try_path) = false) then 
          try (try_path := !try_path@[v2]; try_weight := !try_weight+w;
               find_inner_path m v2) with
            Fail-> find_inner_path xs a' 
        else find_inner_path xs a 
  in find_inner_path m a 

(* TODO: Implement find_path'. *)
let find_path' (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node node visited fc sc =
    raise NotImplemented
  and aux_list nodes visited fc sc =
    raise NotImplemented
  in
  raise NotImplemented


(* TODO: Implement find_all_paths *)
let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) list = 
  let rec aux_node node visited =
    raise NotImplemented  
  and aux_list nodes visited = 
    raise NotImplemented  
  in
  raise NotImplemented  


(* TODO: Implement find_shortest_path *)
let find_shortest_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) option = 
  raise NotImplemented    