exception Invalid;; 

let solve (a, b,c) : (float*float) = match a with
  | 0. -> raise Invalid
  | _ -> if (b *. b -. (4.*.a*.c)) < 0. then raise Invalid else
        let r1= (b *. -1. +. ( sqrt(b *. b -. (4.*.a*.c)) ) )/.(2.*.a) in
        let r2 = (b *. -1. -. ( sqrt(b*.b -. (4.*.a*.c)) ) )/.(2.*.a) in
        (r1, r2);;

let find_item_index (a,l) = 
  let rec find n l = match l with
    | [] -> raise Invalid
    | x::xs -> if x=a then n else find (n+1) xs
  in find 0 l ;;

let run_find_item_index (x, l) =
  try 
    let n = find_item_index (x,l) in 
    print_string ("The item " ^ string_of_int x ^ 
                  " is at index " ^ string_of_int n)
  with Invalid -> print_string "This item does not exist in the list";;