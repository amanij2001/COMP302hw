(*My own helper functions*) 
let program_11 = 
  "let name x = y in 
     x + 1 + z end; " ;;

let program_12 = (*the y in x=y is free, but other y's are bound*)
  " let val x = y in 
   let val (y, z) = (2*3, 6*w) in 
   let name u = 8+9-w in 
    x + 1 + w - y + z + i
end end end; " ;; 

let program_13 = 
  "let name x = 3 in 
     2 + 1 + z end; " ;;


let program_14 = 
  "let val x = y in 
   let val (y, z) = (2*3, 6*w) in 
   let name u = 8+9-y in 
     x + 1 + u + e + i
end end end; " ;;

let program_15 = 
  "let val x = y in 
   let name u = 8+9-y in 
   let val o = 7 in 
     x + 1 + e + i
end end end; " ;;

let program_16 = 
  "(fn y => y + x * 2) x;" ;;
    
let program_17 =
  "(3=3)&&(4+5=9)&&(3+3);";;

let program_18 =
  "(5=4)||(3+3=7)||(5+5);";;

let program_19 =
  "let in x+y+3 end;";;


(* Q0  : Get familiar with the external syntax of MiniML *)
let parse_tests : (string * (string, exp) either) list = [
    (* Provide your tests for the parser *)
  ("1;", Right (Int 1));
  ("1+2;", Right (Primop (Plus, [(Int 1); (Int 2)]))); 
]


let free_vars_tests : (exp * name list) list = [
  (Int 10, []);
  (Let
     ([Valtuple
         (Tuple [Primop (Plus, [Int 2; Int 1]); Primop (Times, [Int 2; Int 50])],
          ["x"; "y"])],
      Primop (Times, [Primop (Times, [Var "x"; Var "x"]); Var "y"])),[]);
  (Let ([ByName (Var "y", "x")],
        Primop (Plus, [Primop (Plus, [Var "x"; Int 1]); Var "z"])), ["y"; "z"]);
  (Let ([Val (Var "y", "x")],
        Let
          ([Valtuple
              (Tuple
                 [Primop (Times, [Int 2; Int 3]); Primop (Times, [Int 6; Var "w"])],
               ["y"; "z"])],
           Let
             ([ByName (Primop (Minus, [Primop (Plus, [Int 8; Int 9]); Var "e"]), "u")],
              Primop (Plus,
                      [Primop (Plus,
                               [Primop (Minus,
                                        [Primop (Plus, [Primop (Plus, [Var "x"; Int 1]); Var "u"]);
                                         Var "y"]);
                                Var "e"]);
                       Var "i"])))), ["y"; "w"; "e"; "i"]);
  ((Let
      ([Val
          (Rec ("fact", TArrow (TInt, TInt),
                Fn
                  ("x", Some TInt,
                   If (Primop (Equals, [Var "x"; Int 0]), Int 1,
                       Primop (Times,
                               [Var "x"; Apply (Var "fact", Primop (Minus, [Var "x"; Int 1]))])))),
           "fact")],
       Apply (Var "fact", Int 5))), []);
]

(* Q1  : Find the free variables in an expression *)
let rec free_vars (e : exp) : name list = match e with
  | Var y -> [y]
  | Int n -> []
  | Bool b -> []
  | If (e, e1, e2) ->
      union (free_vars e) (union (free_vars e1) (free_vars e2))
  | Primop (po, args) ->
      List.fold_right (fun e1 fv -> union (free_vars e1) fv) args []
  | Let (d, e2) -> (match d with 
      | [] -> []
      | Val (e, x) :: [] | ByName (e, x) :: [] -> 
          union (free_vars e) (delete [x] (free_vars e2))
      | Valtuple (e, xlist) :: [] -> 
          union (free_vars e) (delete xlist (free_vars e2))
      | Val (e, x)::ds | ByName (e, x)::ds -> 
          union (free_vars e) (delete [x] (free_vars (Let (ds, e2))))
      | Valtuple (e, xlist)::ds -> 
          union (free_vars e) (delete xlist (free_vars (Let (ds, e2))) ) )
  | Apply (e1, e2) -> union (free_vars e1) (free_vars e2) 
  | Tuple exps -> List.fold_right (fun e1 fv -> union (free_vars e1) fv) exps []
  | Fn (n, t, e) -> delete [n] (free_vars e)
  | Rec (n, t, e) -> delete [n] (free_vars e)
  | Anno (e, t) -> free_vars e 
                                     

let unused_vars_tests : (exp * name list) list = [
  ((Primop (Plus, [Primop (Times, [Int 10; Int 10]); Int 33])), []); (*2*)
  ((Anno (If (Bool true, Int 3, Int 5), TInt)), []); (*4*)
  ((Let ([ByName (Int 3, "x")         ],
         Primop (Plus, [Primop (Plus, [Int 2; Int 1]); Var "z"]))), ["x"]); (*13*)
  ((Let ([Val (Bool true, "x");
          Val (Int 1, "x")], Primop (Plus, [Var "x"; Int 5]))), ["x"]); (*6*)
  ((Let ([Val (Int 1, "x")], Primop (Plus, [Var "x"; Int 5]))), []); (*5*)
  ((Let
      ([Valtuple
          (Tuple [Primop (Plus, [Int 2; Int 1]); Primop (Times, [Int 2; Int 50])],
           ["x"; "y"])],
       Primop (Times, [Primop (Times, [Var "x"; Var "x"]); Var "y"]))), []); (*8*)
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
  ((Let
      ([Val
          (Let ([Val (Int 10, "ten")],
                Anno (Fn ("y", None, Var "ten"), TArrow (TInt, TInt))),
           "f")],
       Apply (Var "f", Int 55))), []); (*10*)
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
         (Apply (Apply (Var "repeat", Int 4),
                 Fn ("z", Some TInt, Primop (Times, [Var "z"; Int 2]))),
          Int 100))), []); (*9*)
  
]

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
      let rec unused dlist used notused = match dlist with
        | [] -> notused
        | di::ds -> 
            match di with
            | Val (e, x) | ByName (e, x) -> (unused_vars e)@(
                if member x (
                    union (List.flatten (List.map (fun dr -> 
                        let Val (e,_) | ByName (e,_) | Valtuple (e,_) = dr in
                        free_vars e )
                        ds ) ) (free_vars e2) ) 
                then 
                  if member x used then unused ds used (notused@[x])
                  else unused ds (x::used) notused
                else unused ds used (notused@[x]) )
            | Valtuple (e, xlist) -> (unused_vars e)@(
                let unusedtuple =
                  List.flatten (List.map (fun xi ->
                      if member xi (
                          union (List.flatten (List.map (fun dr -> 
                              let Val (e,_) | ByName (e,_) | Valtuple (e,_) = dr in
                              free_vars e
                            ) ds ) ) (free_vars e2) ) 
                      then []
                      else [xi]) xlist) 
                in let usedtuple = delete unusedtuple xlist
                in unused ds (used@usedtuple) (notused@unusedtuple))
      in unused d [] []
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

  
let subst_tests : (((exp * name) * exp) * exp) list = [
  (((Int 1, "x"), (Let
                     ([Val
                         (Rec ("apply", TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt)),
                               Fn
                                 ("f", Some (TArrow (TInt, TInt)),
                                  Fn ("x", Some TInt, Apply (Var "f", Var "x")))),
                          "apply")],
                      Apply
                        (Apply (Var "apply", Fn ("x", None, Primop (Times, [Var "x"; Int 3]))),
                         Int 100)))), 
   Let
     ([Val
         (Rec ("apply", TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt)),
               Fn
                 ("f", Some (TArrow (TInt, TInt)),
                  Fn ("x", Some TInt, Apply (Var "f", Var "x")))),
          "apply")],
      Apply
        (Apply (Var "apply", Fn ("x", None, Primop (Times, [Var "x"; Int 3]))),
         Int 100))); (*1*)
                     
  (((Int 5, "y"), Let ([Val (Int 1, "x")], Primop (Plus, [Var "y"; Int 5]))), 
   Let ([Val (Int 1, "x")], Primop (Plus, [Int 5; Int 5]))); (*5*)
                                                             
  (((Primop (Plus, [Var "y"; Int 3]), "z"), Let
      ([Valtuple
          (Tuple [Primop (Plus, [Int 2; Int 1]); Primop (Times, [Int 2; Int 50])],
           ["x"; "y"])],
       Primop (Times, [Primop (Times, [Var "x"; Var "z"]); Var "y"]))), 
   Let
     ([Valtuple
         (Tuple [Primop (Plus, [Int 2; Int 1]); Primop (Times, [Int 2; Int 50])],
          ["x"; "1y"])],
      Primop (Times, [Primop (Times, [Var "x"; Primop (Plus, [Var "y"; Int 3])]); Var "1y"]))); (*8*)
                                                                                                
  (((Var "z", "w"),  
    Let ([Val (Var "y", "x"); 
          Valtuple (Tuple [Primop (Times, [Int 2; Int 3]); Primop (Times, [Int 6; Var "w"])],["y"; "z"]);
          ByName (Primop (Minus, [Primop (Plus, [Int 8; Int 9]); Var "w"]), "u")],
         Primop (Plus,
                 [Primop (Plus,
                          [Primop (Minus,
                                   [Primop (Plus, [Primop (Plus, [Var "x"; Int 1]); Var "w"]);
                                    Var "y"]);
                           Var "z"]);
                  Var "i"]))), 
   Let ([Val (Var "y", "x"); 
         Valtuple (Tuple [Primop (Times, [Int 2; Int 3]); Primop (Times, [Int 6; Var "z"])],["y"; "1z"]);
         ByName (Primop (Minus, [Primop (Plus, [Int 8; Int 9]); Var "z"]), "u")],
        Primop (Plus,
                [Primop (Plus,
                         [Primop (Minus,
                                  [Primop (Plus, [Primop (Plus, [Var "x"; Int 1]); Var "z"]);
                                   Var "y"]);
                          Var "1z"]);
                 Var "i"]))); (*12*)    
                              
  (((Primop (Plus, [Var "y"; Int 1]), "x"), 
    Apply
      (Fn
         ("y", None, Primop (Plus, [Var "y"; Primop (Times, [Var "x"; Int 2])])),
       Var "x")), 
   Apply
     (Fn
        ("1y", None, Primop (Plus, [Var "1y"; Primop (Times, [Primop (Plus, [Var "y"; Int 1]); Int 2])])),
      Primop (Plus, [Var "y"; Int 1]))); (*16*)
                                         
  (((Int 5, "x"), 
    Let ([], Primop (Plus, [Primop (Plus, [Var "x"; Var "y"]); Int 3])) ), 
   Let ([], Primop (Plus, [Primop (Plus, [Int 5; Var "y"]); Int 3]))); 
]

(* Q3  : Substitute a variable *)
let rec subst ((e', x) : exp * name) (e : exp) : exp = match e with
  | Var y ->
      if x = y then
        e'
      else
        Var y 
  | Int _ | Bool _ -> e
  | Primop (po, es) -> Primop (po, List.map (subst (e', x)) es)
  | If (e1, e2, e3) -> 
      If (subst (e', x) e1, subst (e', x) e2, subst (e', x) e3)
  | Tuple es -> Tuple (List.map (subst (e', x)) es)
  | Anno (e, t) -> Anno (subst (e', x) e, t)

  | Let (d, e2) -> (
      if d = [] then Let ([], subst (e', x) e2) else
        let rec buildValid gList vList ve2 : (dec list * exp) = match gList with
          | [] -> (vList, ve2)
(*If last/only one & y!=x, then we know we can subst x into e2 safely*)
          | Val (e1, y)::[] ->
              if y=x then 
                buildValid [] (vList@[Val (subst (e', x) e1, y)]) ve2
              else if (member y (free_vars e')) then
                let new_var = fresh_var y in
              (*subst into ve2; subst (e', x) ve2*)
              (*let vewVayRemaining = subst (Var new_var, y) Let (ds, ve2) in*)
                let newVarInE2 = subst (Var new_var, y) ve2 in
                buildValid [] (vList@[Val (subst (e', x) e1, new_var)])
                  (subst (e', x) newVarInE2)
              else buildValid [] (vList@[Val (subst (e', x) e1, y)]) (subst (e', x) ve2)
                  
          | ByName (e1, y)::[] ->
              if y=x then 
                buildValid [] (vList@[ByName (subst (e', x) e1, y)]) ve2
              else if (member y (free_vars e')) then
                let new_var = fresh_var y in 
                let newVarInE2 = subst (Var new_var, y) ve2 in
                buildValid [] (vList@[ByName (subst (e', x) e1, new_var)]) 
                  (subst (e', x) newVarInE2)
              else buildValid [] (vList@[ByName (subst (e', x) e1, y)]) (subst (e', x) ve2)
                   
          | Valtuple (e1, ylist)::[] -> 
              let check = List.exists (fun y -> y=x) ylist in
              if check then
                buildValid [] (vList@[Valtuple (subst (e', x) e1, ylist)]) ve2
              else
                let fv = List.filter (fun y -> (member y (free_vars e'))) ylist in 
                let rec replace free (repNList, repE2) : (name list * exp) = match free with
                  | [] -> (repNList, repE2)
                  | x::xs -> 
                      let new_var = fresh_var x in
                      let newNameList = 
                        List.map (fun n -> if n=x then new_var else n) repNList in
                      replace xs (newNameList, (subst (Var new_var, x) repE2))
                in
                let (repNames, repE2) = replace fv (ylist, e2) in
                buildValid [] (vList@[Valtuple (subst (e', x) e1, repNames)]) 
                  (subst (e', x) repE2)
  
          | Val (e1, y)::ds ->
              if y=x then 
                buildValid [] (vList@[Val (subst (e', x) e1, y)]@ds) ve2
              else if (member y (free_vars e')) then
                let new_var = fresh_var y in 
                let (Let (newVarInRem, newVarInE2)) = subst (Var new_var, y) (Let (ds, e2)) in
                buildValid newVarInRem (vList@[Val (subst (e', x) e1, new_var)]) newVarInE2
              else buildValid ds (vList@[Val (subst (e', x) e1, y)]) ve2
              
          | ByName (e1, y)::ds ->
              if y=x then 
                buildValid [] (vList@[ByName (subst (e', x) e1, y)]@ds) ve2
              else if (member y (free_vars e')) then
                let new_var = fresh_var y in 
                let (Let (newVarInRem, newVarInE2)) = subst (Var new_var, y) (Let (ds, e2)) in
                buildValid newVarInRem (vList@[ByName (subst (e', x) e1, new_var)]) newVarInE2
              else buildValid ds (vList@[ByName (subst (e', x) e1, y)]) ve2
                   
          | Valtuple (e1, ylist)::ds ->
              let check = List.exists (fun y -> y=x) ylist in
              if check then
                buildValid [] (vList@[Valtuple (subst (e', x) e1, ylist)]@ds) ve2
              else
                let fv = List.filter (fun y -> (member y (free_vars e'))) ylist in 
                let rec replace free (repNList, repRemaining) : (name list * exp) = match free with
                  | [] -> (repNList, repRemaining)
                  | x::xs -> 
                      let new_var = fresh_var x in
                      let newNameList = 
                        List.map (fun n -> if n=x then new_var else n) repNList in
                      let newRemaining = subst (Var new_var, x) repRemaining in
                      replace xs (newNameList, newRemaining)
                in
                let (repNames, Let (repDecs, repE2)) = replace fv (ylist, Let(ds, e2)) in
                buildValid repDecs (vList@[Valtuple (subst (e', x) e1, repNames)]) repE2
        in 
        let (list, exp2) = buildValid d [] e2 in 
        Let (list, exp2) )
  | Apply (e1, e2) -> Apply (subst (e', x) e1, subst (e', x) e2)
  | Fn (y, t, e1) -> 
      if (y=x) then e
      else if (member y (free_vars e')) then 
        let new_var = fresh_var y in
        let new_exp = subst (Var new_var, y) e1 in
        Fn (new_var, t, subst (e', x) new_exp)
      else Fn (y, t, subst (e', x) e1)
  | Rec (y, t, e1) -> 
      if (y=x) then e
      else if (member y (free_vars e')) then 
        let new_var = fresh_var y in
        let new_exp = subst (Var new_var, y) e1 in
        Rec (new_var, t, subst (e', x) new_exp)
      else Rec (y, t, subst (e', x) e1)


let eval_tests : (exp * exp) list = [
  (Primop (And,
           [Primop (Equals, [Int 3; Int 3]);
            Primop (And,
                    [Primop (Equals, [Primop (Plus, [Int 4; Int 5]); Int 10]);
                     Primop (Equals, [Int 3; Int 3])])]), Bool false); (*17*)
                                                                       
  (Primop (Or,
           [Primop (Or,
                    [Primop (Equals, [Int 5; Int 4]);
                     Primop (Equals, [Primop (Plus, [Int 3; Int 3]); Int 6])]);
            Primop (Plus, [Int 5; Int 5])]), Bool true); (*18*)
                                                         
  (Fn
     ("y", None, Primop (Plus, [Var "y"; Primop (Times, [Var "x"; Int 2])])), 
   Fn
     ("y", None, Primop (Plus, [Var "y"; Primop (Times, [Var "x"; Int 2])]))); (*16 no apply*)
                                                                               
  (Apply
     (Fn
        ("y", None, Primop (Plus, [Var "y"; Primop (Times, [Int 6; Int 2])])),
      Int 3), Int 15); (*16*)
                       
  (Apply (Int 5, Int 6), Int 0); (*Apply error*)
                                 
  (Let
     ([Val
         (Rec ("apply", TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt)),
               Fn
                 ("f", Some (TArrow (TInt, TInt)),
                  Fn ("x", Some TInt, Apply (Var "f", Var "x")))),
          "apply")],
      Apply
        (Apply (Var "apply", Fn ("x", None, Primop (Times, [Var "x"; Int 3]))),
         Int 100)), Int 300); (*1*)
                              
  (Let
     ([Val
         (Rec ("fact", TArrow (TInt, TInt),
               Fn
                 ("x", Some TInt,
                  If (Primop (Equals, [Var "x"; Int 0]), Int 1,
                      Primop (Times,
                              [Var "x"; Apply (Var "fact", Primop (Minus, [Var "x"; Int 1]))])))),
          "fact")],
      Apply (Var "fact", Int 5)), Int 120 ); (*3*)     
                                             
  (Let ([Val (Bool true, "x")],
        Let ([Val (Int 1, "x")], Primop (Plus, [Var "x"; Int 5]))), Int 6 ); (*6*)
                                                                             
  (Let ([ByName (Int 3, "x")], Primop (Plus, [Var "x"; Int 1])), 
   Int 4); (*7*)
                                   
  (Let
     ([Valtuple
         (Tuple [Primop (Plus, [Int 2; Int 1]); Primop (Times, [Int 2; Int 50])],
          ["x"; "y"])],
      Primop (Times, [Primop (Times, [Var "x"; Var "x"]); Var "y"])), Int 900); (*8*)
                                                                                
  (Let
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
        (Apply (Apply (Var "repeat", Int 4),
                Fn ("z", Some TInt, Primop (Times, [Var "z"; Int 2]))),
         Int 100)), Int 1600); (*9*)
                               
  (Let
     ([Val
         (Let ([Val (Int 10, "ten")],
               Anno (Fn ("y", None, Var "ten"), TArrow (TInt, TInt))),
          "f")],
      Apply (Var "f", Int 55)), Int 10); (*10*)
]

(* Q4  : Evaluate an expression in big-step *)
let rec eval : exp -> exp =
  (* do not change the code from here *)
  let bigstep_depth = ref 0 in
  fun e ->
    if !debug >= 1 then
      print_endline
        (String.make (!bigstep_depth) ' '
         ^ "eval (" ^ Print.exp_to_string e ^ ")\n");
    incr bigstep_depth;
  (* to here *)
    let result =
      match e with
      | Int _ | Bool _ -> e
      | Tuple es -> Tuple (List.map eval es)
      | If (e1, e2, e3) ->
          begin match eval e1 with
            | Bool b ->
                if b then
                  eval e2
                else
                  eval e3
            | _ -> stuck "Condition for if expression should be of the type bool"
          end
      | Anno (e, _) -> eval e     (* types are ignored in evaluation *)
      | Var x -> stuck ("Free variable \"" ^ x ^ "\" during evaluation")

      | Fn (x, t, e1) -> e
      | Apply (e1, e2) -> 
          (match eval e1 with 
           | Fn (x, t, fnexp) -> eval (subst (eval e2, x) fnexp)
           | _ -> stuck "First expression should be a function")
      | Rec (f, t, e1) -> eval (subst (e, f) e1)

      | Primop (And, es) ->
          (let vs = List.map eval es in
           if (List.length vs) < 2 then stuck "Need at least 2 arguments for And operations"
           else
             let rec checkAll values = match values with
               | v::[] -> v 
               | Bool true::vs -> checkAll vs
               | Bool false::vs -> Bool false
               | _ -> stuck "Bad arguments for And operations"
             in checkAll vs)
      | Primop (Or, es) ->
          (let vs = List.map eval es in
           if (List.length vs) < 2 then stuck "Need at least 2 arguments for Or operations"
           else
             let rec checkAll values = match values with
               | v::[] -> v 
               | Bool true::vs -> Bool true
               | Bool false::vs -> checkAll vs
               | _ -> stuck "Bad arguments for Or operations"
             in checkAll vs)
      | Primop (op, es) ->
          let vs = List.map eval es in
          begin match eval_op op vs with
            | None -> stuck "Bad arguments to primitive operation"
            | Some v -> v
          end

      | Let (d, e) -> (match d with
          | [] -> eval e
          | Val (e1, x)::ds -> eval (subst (eval e1, x) (Let (ds, e)))
          | ByName (e1, x)::ds -> eval (subst (e1, x) (Let (ds, e)))
          | Valtuple (e1, xlist)::ds -> 
              let Tuple vs = eval e1 in
              if List.length xlist = List.length vs then
                let rec subAll varList valList updatedExp = match (varList, valList) with
                  | ([], []) -> updatedExp
                  | (x::xs, v::vs) -> subAll xs vs (subst (v, x) updatedExp)
                in eval (subAll xlist vs (Let (ds, e)))
              else stuck "Tuple size must be the same"
        )
    in
  (* do not change the code from here *)
    decr bigstep_depth;
    if !debug >= 1 then
      print_endline
        (String.make (!bigstep_depth) ' '
         ^ "result of eval (" ^ Print.exp_to_string e ^ ") = "
         ^ Print.exp_to_string result ^ "\n");
  (* to here *)
    result


let infer_tests : ((context * exp) * typ) list = [
]

(* Q5  : Type an expression *)
(* Q7* : Implement the argument type inference
         For this question, move this function below the "unify". *)
let rec infer (ctx : context) (e : exp) : typ = match e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | Tuple es -> TProduct (List.map (infer ctx) es)
  | If (e1, e2, e3) ->
      (match infer ctx e1 with
       | TBool -> let (t1, t2) = (infer ctx e2, infer ctx e3) in
           if t1 = t2 then
             t1
           else
             type_fail "Both expressions must have same type"
       | t -> type_fail ("Condition for if expression should be of the type bool,
      but type " ^ Print.typ_to_string t ^ " was infered") )
  | Anno (e, t) -> t
  | Var x -> type_fail ("Free variable \"" ^ x ^ "\" during evaluation")

  | Fn (x, t, e1) -> (match t with 
      | Some ty -> TArrow (ty, infer (extend ctx (x, ty)) e1)
      | _ -> type_fail "No type for function input")
    
                      
  | Apply (e1, e2) -> 
      (match infer ctx e1 with 
       | TArrow (tau, tau') -> 
           let typE2 = infer ctx e2 in
           if typE2 = tau then tau'
           else type_fail ("Infered type of second expression was " ^ Print.typ_to_string typE2 ^ 
                           " but was expecting " ^ Print.typ_to_string tau ^ " in order to apply to function")
                                                                                                  
       | t -> type_fail ("First expression should be a function, 
                but type " ^ Print.typ_to_string t ^ " was infered"))
  | Rec (f, t, e1) -> t

  | Primop (Negate, es) -> (
      if es = e::[] then match infer ctx e with 
        | TInt -> TInt
        | t -> type_fail ("Expression type was infered to be " ^ Print.typ_to_string t ^ 
                          " but was expecting an int")    
      else type_fail "Negate only works on one integer at a time") 
  | Primop (And, es) | Primop (Or, es) ->
      (let typList = List.map (infer ctx) es in
       if List.for_all (fun t -> t=TBool) typList then TBool
       else type_fail "All expressions must be booleans" ) 
  | Primop (Plus, es) | Primop (Minus, es) | Primop (Times, es) | Primop (Div, es) ->
      (let typList = List.map (infer ctx) es in
       if List.for_all (fun t -> t=TInt) typList then TInt
       else type_fail "All expressions must be integers" ) 
  | Primop (op, es) -> 
      (let typList = List.map (infer ctx) es in
       if List.for_all (fun t -> t=TInt) typList then TBool
       else type_fail "All expressions must be integers" )

  | Let (d, e) -> (match d with
      | [] -> 
      | Val (e1, x)::ds ->
      | ByName (e1, x)::ds ->
      | Valtuple (e1, xlist)::ds -> 
    ) 


let unify_tests : ((typ * typ) * unit) list = [
]

(* find the next function for Q5 *)
(* Q6  : Unify two types *)
let unify (ty1 : typ) (ty2 : typ) : unit = raise NotImplemented


(* Now you can play with the language that you've implemented! *)
let execute (s: string) : unit =
  match P.parse s with
  | Left s -> print_endline ("parsing failed: " ^ s)
  | Right e ->
      try
       (* first we type check the program *)
        ignore (infer (Ctx []) e);
        let result = eval e in
        print_endline ("program is evaluated to: " ^ Print.exp_to_string result)
      with
      | NotImplemented -> print_endline "code is not fully implemented"
      | Stuck s -> print_endline ("evaluation got stuck: " ^ s)
      | NotFound -> print_endline "variable lookup failed"
      | TypeError s -> print_endline ("type error: " ^ s)
      | e -> print_endline ("unknown failure: " ^ Printexc.to_string e)


(************************************************************
 *             Do not change these functions.               *
 *               They are needed for tests.                 *
 ************************************************************)
let list_to_string el_to_string l : string =
  List.fold_left
    begin fun acc el ->
      if acc = "" then
        el_to_string el
      else
        acc ^ "; " ^ el_to_string el
    end
    ""
    l
  |> fun str -> "[" ^ str ^ "]"

let run_test name f ts stringify : unit =
  List.iteri
    begin fun idx (input, expected_output) ->
      try
        let output = f input in
        if output <> expected_output then
          begin
            print_string (name ^ " test #" ^ string_of_int idx ^ " failed\n");
            print_string (stringify output ^ " <> " ^ stringify expected_output)
          end
      with
      | exn ->
          print_string (name ^ " test #" ^ string_of_int idx ^ " raised an exception:\n");
          print_string (Printexc.to_string exn)
    end
    ts

let run_free_vars_tests () : unit =
  run_test "free_vars" free_vars free_vars_tests (list_to_string (fun x -> x))

let run_unused_vars_tests () : unit =
  run_test "unused_vars" unused_vars unused_vars_tests (list_to_string (fun x -> x))

let run_subst_tests () : unit =
  run_test "subst" (fun (s, e) -> subst s e) subst_tests Print.exp_to_string

let run_eval_tests () : unit =
  run_test "eval" eval eval_tests Print.exp_to_string

let run_infer_tests () : unit =
  run_test "infer" (fun (ctx, e) -> infer ctx e) infer_tests Print.typ_to_string

let run_unify_tests () : unit =
  run_test "unify" (fun (ty1, ty2) -> unify ty1 ty2) unify_tests (fun () -> "()")

let run_all_tests () : unit =
  run_free_vars_tests ();
  run_unused_vars_tests ();
  run_subst_tests ();
  run_eval_tests ();
  run_infer_tests ();
  run_unify_tests ()
