open SmallCTypes
open EvalUtils
open TokenTypes


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec search_id env id = 
  match env with 
  | [] -> raise (DeclareError "Variable not declared!")
  | (str, value)::t ->  if str = id then value else (search_id t id)

let rec eval_expr env t =
  match t with 
  | Int x -> Int_Val(x)
  | Bool b -> Bool_Val(b)
  | ID id -> (match (search_id env id) with
              | Int_Val x -> Int_Val(x)
              | Bool_Val b -> Bool_Val(b)
              | _ -> raise (DeclareError "Something went wrong"))
  | Add (expr1, expr2) -> 
    let n1 = eval_expr env expr1 in 
    let n2 = eval_expr env expr2 in 
    (match n1, n2 with
    | (Int_Val x), (Int_Val y) -> Int_Val (x+y)
    | _ -> raise (TypeError "Wrong types!"))
  | Sub (expr1, expr2) -> 
    let n1 = eval_expr env expr1 in 
    let n2 = eval_expr env expr2 in 
    (match n1, n2 with
    | (Int_Val x), (Int_Val y) -> Int_Val (x-y)
    | _ -> raise (TypeError "Wrong types!"))
  | Mult (expr1, expr2) ->  
    let n1 = eval_expr env expr1 in 
    let n2 = eval_expr env expr2 in 
    (match n1, n2 with
    | (Int_Val x), (Int_Val y) -> Int_Val (x*y)
    | _ -> raise (TypeError "Wrong types!"))
  | Div (expr1, expr2) -> 
    let n1 = eval_expr env expr1 in 
    let n2 = eval_expr env expr2 in 
    (match n1, n2 with
    | (Int_Val x), (Int_Val 0) -> raise DivByZeroError
    | (Int_Val x), (Int_Val y) -> Int_Val (x/y)
    | _ -> raise (TypeError "Wrong types!"))
  | Pow (expr1, expr2) -> 
    let n1 = eval_expr env expr1 in 
    let n2 = eval_expr env expr2 in 
    (match n1, n2 with
    | (Int_Val x), (Int_Val 0) -> Int_Val (1)
    | (Int_Val x), (Int_Val y) -> Int_Val (int_of_float((float_of_int x)**(float_of_int y)))
    | _ -> raise (TypeError "Wrong types!"))
  | Greater (expr1, expr2) -> 
    let n1 = eval_expr env expr1 in 
    let n2 = eval_expr env expr2 in 
    (match n1, n2 with
    | (Int_Val x), (Int_Val y) -> if x > y then Bool_Val true else Bool_Val false
    | _ -> raise (TypeError "Wrong types!"))
  | Less (expr1, expr2) ->  
    let n1 = eval_expr env expr1 in 
    let n2 = eval_expr env expr2 in 
    (match n1, n2 with
    | (Int_Val x), (Int_Val y) -> if x < y then Bool_Val true else Bool_Val false
    | _ -> raise (TypeError "Wrong types!"))
  | GreaterEqual (expr1, expr2) ->  
    let n1 = eval_expr env expr1 in 
    let n2 = eval_expr env expr2 in 
    (match n1, n2 with
    | (Int_Val x), (Int_Val y) -> if (x >= y) then Bool_Val true else Bool_Val false
    | _ -> raise (TypeError "Wrong types!"))
  | LessEqual (expr1, expr2) -> 
    let n1 = eval_expr env expr1 in 
    let n2 = eval_expr env expr2 in 
    (match n1, n2 with
    | (Int_Val x), (Int_Val y) -> if (x <= y) then Bool_Val true else Bool_Val false
    | _ -> raise (TypeError "Wrong types!"))
  | Equal (expr1, expr2) -> 
    let n1 = eval_expr env expr1 in 
    let n2 = eval_expr env expr2 in 
    (match n1, n2 with
    | (Int_Val x), (Int_Val y) -> if x = y then Bool_Val true else Bool_Val false
    | (Bool_Val x), (Bool_Val y) -> if x = y then Bool_Val true else Bool_Val false
    | _ -> raise (TypeError "Wrong types!"))
  | NotEqual (expr1, expr2) -> 
    let n1 = eval_expr env expr1 in 
    let n2 = eval_expr env expr2 in 
    (match n1, n2 with
    | (Int_Val x), (Int_Val y) -> if x <> y then Bool_Val true else Bool_Val false
    | (Bool_Val x), (Bool_Val y) -> if x <> y then Bool_Val true else Bool_Val false
    | _ -> raise (TypeError "Wrong types!"))
  | Or (expr1, expr2) ->  
    let n1 = eval_expr env expr1 in 
    let n2 = eval_expr env expr2 in 
    (match n1, n2 with
    | (Bool_Val true), (Bool_Val true) -> Bool_Val true
    | (Bool_Val true), (Bool_Val false) -> Bool_Val true
    | (Bool_Val false), (Bool_Val true) -> Bool_Val true
    | (Bool_Val false), (Bool_Val false) -> Bool_Val false
    | _ -> raise (TypeError "Wrong types!"))
  | And (expr1, expr2) -> 
    let n1 = eval_expr env expr1 in 
    let n2 = eval_expr env expr2 in 
    (match n1, n2 with
    | (Bool_Val true), (Bool_Val true) -> Bool_Val true
    | (Bool_Val true), (Bool_Val false) -> Bool_Val false
    | (Bool_Val false), (Bool_Val true) -> Bool_Val false
    | (Bool_Val false), (Bool_Val false) -> Bool_Val false
    | _ -> raise (TypeError "Wrong types!"))
  | Not expr -> 
    let n1 = eval_expr env expr in 
    (match n1 with
    | (Bool_Val true) -> Bool_Val false
    | (Bool_Val false) -> Bool_Val true
    | _ -> raise (TypeError "Wrong types!"))

let rec find_id env id = 
  match env with 
  | [] -> false
  | (str, _)::t ->  if str = id then true else (find_id t id)

let rec assign_id env id assignment = 
  match env with 
  | [] -> raise (DeclareError "Variable not declared!")
  | (str, value)::t ->  if str = id then 
                          (match value, assignment with
                          | Int_Val x, Int_Val y -> (str, assignment)::t 
                          | Bool_Val x, Bool_Val y -> (str, assignment)::t 
                          | _ -> raise (TypeError "Assignment types don't match"))
                        else (str, value)::(assign_id t id assignment)

let rec eval_stmt env s =
  match s with 
  | NoOp -> env
  | Seq (stmt1, stmt2) -> 
      let env1 = eval_stmt env stmt1 in
        eval_stmt env1 stmt2
  | Declare (data, id) -> 
      if (find_id env id) = true then
        raise (DeclareError "Variable already declared")
      else 
        (match data with
          | Int_Type -> (id, Int_Val 0)::env
          | Bool_Type -> (id, Bool_Val false)::env)
  | Assign (id, expr) ->  
      let value = eval_expr env expr in
      let env' = assign_id env id value in
      env'
  | If (expr, stmt1, stmt2) ->  
      let condition = eval_expr env expr in
      (match condition with
      | Bool_Val true -> eval_stmt env stmt1
      | Bool_Val false -> eval_stmt env stmt2
      | _ -> raise (TypeError "Condition not a boolean value"))
  | While (expr, stmt) -> 
      let condition = eval_expr env expr in 
      (match condition with
      | Bool_Val false -> env
      | Bool_Val true ->  let env' = eval_stmt env stmt in
                          eval_stmt env' (While (expr, stmt))
      | _ -> raise (TypeError "Condition not a boolean value"))
  | For (id, start_expr, stop_expr, stmt) ->  
      let start_val = eval_expr env start_expr in
      let stop_val = eval_expr env stop_expr in
      (match start_val, stop_val with
      | Int_Val x, Int_Val y -> 
        if x > y then 
          assign_id env id (Int_Val x)
        else
          let env1 = assign_id env id (Int_Val x) in
          let env2 = eval_stmt env1 stmt in
          let Int_Val new_val = search_id env2 id in
          let env3 = assign_id env2 id (Int_Val (new_val + 1)) in
          eval_stmt env3 (For (id, (Int (new_val+1)), stop_expr, stmt))
      | _, _ -> raise (TypeError "Condition not a boolean value"))
  | Print expr -> 
      let value = eval_expr env expr in
      (match value with 
      | Int_Val x -> print_output_int x; print_output_newline (); env
      | Bool_Val x -> print_output_bool x; print_output_newline (); env)

