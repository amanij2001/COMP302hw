(*My own helper functions*) 
let program_11 = " 
                  let name x = y
in
  x + 1 + z
end; " ;;

let program_12 = (*the y in x=y is free, but other y's are bound*)
  "
                  let val x = y in 
                  let val (y, z) = (2*3, 6*w) in
                  let name u = 8+9-e in
  x + 1 + u-y+e+i
end end end; " ;; 

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
      | Val (e, x) :: [] | ByName (e, x)::[] -> union (free_vars e) (delete [x] (free_vars e2))
      | Valtuple (e, xlist) :: [] -> union (free_vars e) (delete xlist (free_vars e2))
      | Val (e, x)::ds | ByName (e, x)::ds -> union (free_vars e)
                                                (delete [x] (free_vars (Let (ds, e2))))
      | Valtuple (e, xlist)::ds -> union (free_vars e)
                                     (delete xlist (free_vars (Let (ds, e2))) ) )
  | Apply (e1, e2) -> union (free_vars e1) (free_vars e2) 
  | Tuple exps -> List.fold_right (fun e1 fv -> union (free_vars e1) fv) exps []
  | Fn (n, t, e) -> delete [n] (free_vars e)
  | Rec (n, t, e) -> delete [n] (free_vars e)
  | Anno (e, t) -> free_vars e 
                                     

let unused_vars_tests : (exp * name list) list = [
]

(* Q2  : Check variables are in use *)
let rec unused_vars (e : exp) : name list = raise NotImplemented


let subst_tests : (((exp * name) * exp) * exp) list = [
]

(* Q3  : Substitute a variable *)
let rec subst ((e', x) : exp * name) (e : exp) : exp =
  match e with
  | Var y ->
      if x = y then
        e'
      else
        Var y

  | Int _ | Bool _ -> e
  | Primop (po, es) -> Primop (po, List.map (subst (e', x)) es)
  | If (e1, e2, e3) -> If (subst (e', x) e1, subst (e', x) e2, subst (e', x) e3)
  | Tuple es -> Tuple (List.map (subst (e', x)) es)
  | Anno (e, t) -> Anno (subst (e', x) e, t)

  | Let (ds, e2) -> raise NotImplemented
  | Apply (e1, e2) -> raise NotImplemented
  | Fn (y, t, e) -> raise NotImplemented
  | Rec (y, t, e) -> raise NotImplemented


let eval_tests : (exp * exp) list = [
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

      | Fn (x, t, e) -> raise NotImplemented
      | Apply (e1, e2) -> raise NotImplemented
      | Rec (f, t, e) -> raise NotImplemented

      | Primop (And, es) ->
          raise NotImplemented
      | Primop (Or, es) ->
          raise NotImplemented
      | Primop (op, es) ->
          let vs = List.map eval es in
          begin match eval_op op vs with
            | None -> stuck "Bad arguments to primitive operation"
            | Some v -> v
          end

      | Let (ds, e) -> raise NotImplemented
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
let infer (ctx : context) (e : exp) : typ = raise NotImplemented


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
