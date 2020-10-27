type 'a nulref = Null | Ref of 'a ref ;;

let funNulRef x = Ref (ref x) ;;

exception NullPointerException;;

let deref (x: 'a nulref) : 'a = match x with
  | Null -> raise NullPointerException 
  | Ref a -> !a ;;


let modify (f: 'a->'a) (x: 'a nulref) : unit = match x with
  | Null -> raise NullPointerException
  | Ref a -> a := (f (deref x)) ;;



type element = { parent : element ref option ;
                 name : string
               } ;;

let makeElement (name: string) : element ref = 
  ref {parent = None;
       name = name} ;;

let rec find (e: element ref) : element ref = 
  match (!e.parent) with
  | None -> e
  | Some a -> let root = find a in
      (e := {parent = Some root;
             name = !e.name}; root)
                
let rec union (e1: element ref) (e2: element ref) : unit =
  let root1 = find e1 in 
  let root2 = find e2 in
  e2 := {parent = Some root1;
         name = !root2.name }
