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
    (fun toklist' e -> match toklist' with
       | PLUS::toklist'' | SUB::toklist'' -> (*parseSExp toklist'' sc*)
           sc toklist'' e
       | _ -> sc toklist' e)

and parsePExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  parseAtom 
    toklist 
    (fun toklist' e -> match toklist' with
       | TIMES::toklist'' | DIV::toklist'' -> parsePExp toklist'' sc
           (*sc toklist'' e*)
       | _ -> sc toklist' e)

and parseAtom (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a = 
  match toklist with
  | INT n :: ts -> sc ts (Int n)
  | LPAREN :: ts -> parseSExp ts (fun toklist' e -> match toklist' with
      | RPAREN :: toklist'' -> sc toklist'' e 
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

    (*(* ---------- Hamming Numbers ----------- *)

      let rec merge (s1: 'a str) (s2: 'a str) : 'a str = 
        raise NotImplemented

      let rec hamming_series = 
        raise NotImplemented*)