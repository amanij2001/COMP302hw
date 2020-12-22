let unused_vars_tests : (exp * name list) list = [
  ((Primop (Plus, [Primop (Times, [Int 10; Int 10]); Int 33])), []); (*2*)
                                                                     
  ((Anno (If (Bool true, Int 3, Int 5), TInt)), []); (*4*)
                                                     
  ((Let ([Val (Int 1, "x")], Primop (Plus, [Var "x"; Int 5]))), []); (*5*)
                                                                     
  ((Let ([Val (Bool true, "x");
          Val (Int 1, "x")], Primop (Plus, [Var "x"; Int 5]))), ["x"]); (*6*)
                                                                        
  ((Let
      ([Valtuple
          (Tuple [Primop (Plus, [Int 2; Int 1]); Primop (Times, [Int 2; Int 50])],
           ["x"; "y"])],
       Primop (Times, [Primop (Times, [Var "x"; Var "x"]); Var "y"]))), []); (*8*)
                                                                             
  ((Let
      ([Val
          (Rec ("repeat",
                TArrow (TInt, TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt))),
                Fn
                  ("n", Some TInt,
                   Fn
                     ("f", Some (TArrow (TInt, TInt)),
                      Fn
                        ("x", Some TInt,
                         If (Primop (Equals, [Var "n"; Int 0]), Var "x",
                             Apply
                               (Apply
                                  (Apply (Var "repeat", Primop (Minus, [Var "n"; Int 1])),
                                   Var "f"),
                                Apply (Var "f", Var "x"))))))),
           "repeat")],
       Apply
         (Apply (Apply (Var "x", Int 4),
                 Fn ("z", Some TInt, Primop (Times, [Var "z"; Int 2]))),
          Int 100))), []); (*9*)
                           
  ((Let
      ([Val
          (Let ([Val (Int 10, "ten")],
                Anno (Fn ("y", None, Var "ten"), TArrow (TInt, TInt))),
           "f")],
       Apply (Var "f", Int 55))), ["y"]); (*10*)
                                          
  ((Let ([ByName (Int 3, "x")         ],
         Primop (Plus, [Primop (Plus, [Int 2; Int 1]); Var "z"]))), ["x"]); (*13*)
                                       
  ((Let ([Val (Var "y", "x");
          Valtuple
            (Tuple
               [Primop (Times, [Int 2; Int 3]); Primop (Times, [Int 6; Var "w"])],
             ["y"; "z"]);
          ByName (Primop (Minus, [Primop (Plus, [Int 8; Int 9]); Var "y"]), "u")], 
         Primop (Plus,
                 [Primop (Plus,
                          [Primop (Plus, [Primop (Plus, [Var "x" ; Int 1]); Var "u"]); Var "e"]);
                  Var "i"]))), ["z"]); (*14*)         
                                       
  ((Let ([Val (Var "y", "x"); 
          ByName (Primop (Minus, [Primop (Plus, [Int 8; Int 9]); Var "y"]), "u");
          Val (Int 7, "o")],
         Primop (Plus,
                 [Primop (Plus, 
                          [Primop (Plus, [Var "x"; Int 1]); 
                           Var "e"]); Var "i"]))), ["u"; "o"]); (*15*)
]

(*returns [] if x was ever used, [x] if unused*)
let rec checkLets x decList = match decList with
  | [] -> [x]
  | d::ds -> (match d with 
      | Val (e, y) | ByName (e, y) -> 
          if x = y then [x]
          else (if member x (free_vars e) then []
                else checkLets x ds)
      | Valtuple (e, ylist) ->
          if member x ylist then [x]
          else (if member x (free_vars e) then []
                else checkLets x ds) )
    
let rec checkE2 unusedLets freeVars trueUnused = match unusedLets with
  | [] -> trueUnused
  | x::xs -> 
      if member x freeVars then checkE2 xs (delete [x] freeVars) trueUnused
      else checkE2 xs freeVars (x::trueUnused)

(* Q2  : Check variables are in use *)
let rec unused_vars (e : exp) : name list = match e with
  | Var y -> []
  | Int n -> []
  | Bool b -> []
  | If (e, e1, e2) -> 
      union (unused_vars e) (union (unused_vars e1) (unused_vars e2))
  | Primop (po, args) -> 
      List.fold_right (fun e1 fv -> union (unused_vars e1) fv) args []
  | Let (d, e2) -> 
      let rec unusedLets dlist accBound accExp = match dlist with
        | [] -> (accBound, accExp)
        | Val (e1, x)::ds | ByName (e1, x)::ds -> 
            (match e1 with
             | Rec (n, t, ex) ->
                 if (checkLets x ds) = []
                 then unusedLets ds accBound (accExp@(unused_vars ex))
                 else if member x (unused_vars e1) 
                 then unusedLets ds (x::accBound) (accExp@(delete [x] (unused_vars e1)))
                 else unusedLets ds accBound (accExp@(delete [x] (unused_vars e1)))
             | _ ->
                 if (checkLets x ds) = [] (*meaning x has been 
                                     used properly*) 
                 then unusedLets ds accBound (accExp@(unused_vars e1))
                 else (*meaning x was not used in lets*)
                   unusedLets ds (x::accBound) (accExp@(unused_vars e1)) )
        | Valtuple (e1, xlist)::ds -> 
            let tupleUnused = List.flatten (List.map (fun xi -> checkLets xi ds) xlist) in 
            if tupleUnused = [] then unusedLets ds accBound (accExp@(unused_vars e1))
            else unusedLets ds (tupleUnused@accBound) (accExp@(unused_vars e1))
      in let (bound, fromExp) = unusedLets d [] [] in
      fromExp@(checkE2 bound (free_vars e2) [])@(unused_vars e2)
  | Apply (e1, e2) -> union (unused_vars e1) (unused_vars e2)
  | Tuple exps -> 
      List.fold_right (fun e1 fv -> union (unused_vars e1) fv) exps []
  | Fn (n, t, e) -> 
      if member n (free_vars e) then (unused_vars e)
      else n::(unused_vars e)
  | Rec (n, t, e) -> 
      if member n (free_vars e) then (unused_vars e)
      else n::(unused_vars e)
  | Anno (e, t) -> unused_vars e
