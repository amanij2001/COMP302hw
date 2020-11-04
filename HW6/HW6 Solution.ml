(*------------------ Q1------------------ *)
let rec parseExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  parseSExp
    toklist 
    (fun toklist' exp -> match toklist' with
       | SEMICOLON :: toklist'' -> sc toklist'' exp
       | _ -> raise (Error "Expected a single semicolon"))
    
and parseSExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a = 
  parsePExp 
    toklist 
    (fun toklist' pExp -> match toklist' with
       | PLUS::toklist'' -> parseSExp toklist'' (fun t -> fun s -> sc t (Sum(pExp, s)))
       | SUB::toklist'' -> parseSExp toklist'' (fun t -> fun s-> sc t (Minus(pExp, s)))
       | _ -> sc toklist' pExp)

and parsePExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  parseAtom 
    toklist 
    (fun toklist' aExp -> match toklist' with
       | TIMES::toklist'' -> parsePExp toklist'' (fun t -> fun p -> sc t (Prod(aExp, p)))
       | DIV::toklist'' -> parsePExp toklist'' (fun t -> fun p -> sc t (Div(aExp, p)))
       | _ -> sc toklist' aExp)

and parseAtom (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a = 
  match toklist with
  | INT n :: ts -> sc ts (Int n)
  | LPAREN :: ts -> parseSExp ts (fun toklist' sExp -> match toklist' with
      | RPAREN :: toklist'' -> sc toklist'' sExp
      | _ -> raise (Error "Expected a closing parenthesis at the end"))
  | _ -> raise (Error "Excpeted an integer or an S-expression surrounded by parenthesis")

(* parse : string -> exp *)
let parse string =
  parseExp
    (lex string)
    (fun s e -> match s with
       | [] -> e
       | _ -> raise (Error "Incomplete expression"))

(* eval : string -> int *)
let eval e = eval' (parse e)

    (* ---------- Hamming Numbers ----------- *)

let rec take n s = match n with
  | 0 -> []
  | n-> s.hd :: take (n-1) (force s.tl);; 

let rec merge (s1: 'a str) (s2: 'a str) : 'a str = 
  let n = compare (s1.hd) (s2.hd) in
  if n>0 then
    {hd = s2.hd;
     tl = Susp (fun () -> merge s1 (force s2.tl))}
  else if n<0 then
    {hd = s1.hd;
     tl = Susp (fun () -> merge (force s1.tl) s2)}
  else 
    {hd = s1.hd;
     tl = Susp (fun () -> merge (force s1.tl) (force s2.tl))};; 

let rec hamming_series = 
  let rec two =
    {hd = 1; tl = Susp (fun () -> times 2 two)} in
  let rec three =
    {hd = 1; tl = Susp (fun () -> times 3 three)} in 
  let rec five =
    {hd = 1; tl = Susp (fun () -> times 5 five)} in
  let mergeAllThree = merge five (merge two three) in
  {hd = 1;
   tl = Susp (fun () -> merge (times 2 mergeAllThree) 
                 (merge (times 3 mergeAllThree) (times 5 mergeAllThree)))};;