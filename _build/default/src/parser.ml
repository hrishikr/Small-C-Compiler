open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =
  parse_OrExpr toks 

and parse_OrExpr toks = 
  let (t, m) = parse_AndExpr toks in
  match lookahead t with
  | Tok_Or -> let t' = match_token t Tok_Or in 
              let (t'', n) = parse_OrExpr t' in
              (t'', Or (m, n))
  | _ -> (t, m)

and parse_AndExpr toks = 
  let (t, m) = parse_EqualityExpr toks in
  match lookahead t with
  | Tok_And -> let t' = match_token t Tok_And in 
              let (t'', n) = parse_AndExpr t' in
              (t'', And (m,n))
  | _ -> (t, m)

and parse_EqualityExpr toks = 
  let (t, m) = parse_RelationalExpr toks in
  match lookahead t with
  | Tok_Equal -> let t' = match_token t Tok_Equal in 
              let (t'', n) = parse_EqualityExpr t' in
              (t'', Equal(m,n))
  | Tok_NotEqual -> let t' = match_token t Tok_NotEqual in 
              let (t'', n) = parse_EqualityExpr t' in
              (t'', NotEqual(m,n))
  | _ -> (t, m)

and parse_RelationalExpr toks = 
  let (t, m) = parse_AdditiveExpr toks in
  match lookahead t with
  | Tok_Less -> let t' = match_token t Tok_Less in 
              let (t'', n) = parse_RelationalExpr t' in
              (t'', Less (m,n))
  | Tok_Greater -> let t' = match_token t Tok_Greater in 
                  let (t'', n) = parse_RelationalExpr t' in
                  (t'', Greater (m,n))
  | Tok_LessEqual -> let t' = match_token t Tok_LessEqual in 
                    let (t'', n) = parse_RelationalExpr t' in
                    (t'', LessEqual (m,n))
  | Tok_GreaterEqual -> let t' = match_token t Tok_GreaterEqual in 
                        let (t'', n) = parse_RelationalExpr t' in
                        (t'', GreaterEqual (m,n))
  | _ -> (t, m)

and parse_AdditiveExpr toks = 
  let (t, m) = parse_MultiplicativeExpr toks in
  match lookahead t with
  | Tok_Add -> let t' = match_token t Tok_Add in 
              let (t'', n) = parse_AdditiveExpr t' in
              (t'', Add (m,n))
  | Tok_Sub -> let t' = match_token t Tok_Sub in 
              let (t'', n) = parse_AdditiveExpr t' in
              (t'', Sub (m,n))
  | _ -> (t, m)

  and parse_MultiplicativeExpr toks = 
  let (t, m) = parse_PowerExpr toks in
  match lookahead t with
  | Tok_Mult -> let t' = match_token t Tok_Mult in 
              let (t'', n) = parse_MultiplicativeExpr t' in
              (t'', Mult (m, n))
  | Tok_Div -> let t' = match_token t Tok_Div in 
              let (t'', n) = parse_MultiplicativeExpr t' in
              (t'', Div (m, n))
  | _ -> (t, m)

and parse_PowerExpr toks = 
  let (t, m) = parse_UnaryExpr toks in
  match lookahead t with
  | Tok_Pow -> let t' = match_token t Tok_Pow in
              let (t'', n) = parse_PowerExpr t'
              in (t'', Pow (m, n))
  | _ -> (t, m)

and parse_UnaryExpr toks = 
  match lookahead toks with
  | Tok_Not -> let t = match_token toks Tok_Not in
              let (t', m) = parse_PrimaryExpr t in
              (t', Not m)
  | _ -> parse_PrimaryExpr toks
    

and parse_PrimaryExpr toks =  
  match lookahead toks with
  | Tok_Int i -> let t = match_token toks (Tok_Int i) in
                (t, Int i)
  | Tok_Bool b -> let t = match_token toks (Tok_Bool b) in
                (t, Bool b)
  | Tok_ID id -> let t = match_token toks (Tok_ID id) in
                (t, ID id)
  | Tok_LParen -> let t = match_token toks Tok_LParen in
                let (t', s) = parse_expr t in
                let t'' = match_token t' Tok_RParen in
                (t'', s)
  | _ -> raise (InvalidInputException "parse_PrimaryExpr failed")


let rec parse_stmt toks : stmt_result =
  let (t1, stmt1) = parse_StmtOptions toks in
  if stmt1 = NoOp then 
    (t1, NoOp)
  else 
    let (t2, stmt2) = parse_stmt t1 in
    t2, Seq (stmt1, stmt2)

and parse_StmtOptions toks =   
  match lookahead toks with 
  | Tok_Int_Type -> parse_DeclareStmt toks
  | Tok_Bool_Type -> parse_DeclareStmt toks
  | Tok_ID id -> parse_AssignStmt toks
  | Tok_Print -> parse_PrintStmt toks
  | Tok_If -> parse_IfStmt toks
  | Tok_For -> parse_ForStmt toks
  | Tok_While -> parse_WhileStmt toks
  | _ -> toks, NoOp


and parse_DeclareStmt toks = 
  match lookahead toks with
  | Tok_Int_Type -> let t = match_token toks Tok_Int_Type in
                    (match lookahead t with 
                    | Tok_ID id -> 
                    let t1 = match_token t (Tok_ID id) in
                    let t2 = match_token t1 Tok_Semi in
                    (t2, Declare (Int_Type, id))
                    | _ -> raise (InvalidInputException "incorrect declaration"))
  | Tok_Bool_Type -> let t = match_token toks Tok_Bool_Type in
                    (match lookahead t with 
                    | Tok_ID id -> 
                    let t1 = match_token t (Tok_ID id) in
                    let t2 = match_token t1 Tok_Semi in
                    (t2, Declare (Bool_Type, id))
                    | _ -> raise (InvalidInputException  "incorrect declaration"))

and parse_AssignStmt toks = 
  match lookahead toks with
  | Tok_ID id -> let t = match_token toks (Tok_ID id) in
                let t1 = match_token t Tok_Assign in
                let (t2, exp) = parse_expr t1 in
                let t3 = match_token t2 Tok_Semi in
                t3, Assign (id, exp)

and parse_PrintStmt toks = 
  match lookahead toks with
  | Tok_Print -> let t = match_token toks Tok_Print in
                let t1 = match_token t Tok_LParen in
                let (t2, exp) = parse_expr t1 in
                let t3 = match_token t2 Tok_RParen in
                let t4 = match_token t3 Tok_Semi in
                t4, Print (exp)


and parse_IfStmt toks =
  match lookahead toks with 
  | Tok_If -> let t = match_token toks Tok_If in
              let t1 = match_token t Tok_LParen in
              let (t2, exp) = parse_expr t1 in
              let t3 = match_token t2 Tok_RParen in
              let t4 = match_token t3 Tok_LBrace in
              let (t5, stmt) = parse_stmt t4 in
              let t6 = match_token t5 Tok_RBrace in
              let (t7, elseStmt) = parse_ElseBranch t6 in
              t7, If (exp, stmt, elseStmt)

and parse_ElseBranch toks = 
  match lookahead toks with
  | Tok_Else -> let t = match_token toks Tok_Else in
                let t1 = match_token t Tok_LBrace in
                let (t2, stmt) = parse_stmt t1 in
                let t3 = match_token t2 Tok_RBrace in
                (t3, stmt)
  | _ -> (toks, NoOp)

and parse_ForStmt toks =
  match lookahead toks with
  | Tok_For -> let t = match_token toks Tok_For in
                let t1 = match_token t Tok_LParen in
                (match lookahead t1 with 
                | Tok_ID id ->
                let t2 = match_token t1 (Tok_ID id) in
                let t3 = match_token t2 Tok_From in
                let (t4, exp1) = parse_expr t3 in
                let t5 = match_token t4 Tok_To in
                let (t6, exp2) = parse_expr t5 in
                let t7 = match_token t6 Tok_RParen in
                let t8 = match_token t7 Tok_LBrace in
                let (t9, stmt) = parse_stmt t8 in
                let t10 = match_token t9 Tok_RBrace in
                (t10, For (id, exp1, exp2,stmt))
                | _ -> raise (InvalidInputException "incorrect for declaration"))
  | _ -> raise (InvalidInputException  "parse_ForStmt failed" )

and parse_WhileStmt toks =
  match lookahead toks with
  | Tok_While -> let t = match_token toks Tok_While in
                let t' = match_token t Tok_LParen in
                let (t'', exp) = parse_expr t' in
                let new_t = match_token t'' Tok_RParen in
                let new_t' = match_token new_t Tok_LBrace in
                let (new_t'', stmt) = parse_stmt new_t' in
                let newer_t = match_token new_t'' Tok_RBrace in
                (newer_t, While (exp,stmt))
  | _ -> raise (InvalidInputException  "parse_WhileStmt failed") 

let parse_main toks : stmt =
  let t = match_token toks Tok_Int_Type in
  let t1 = match_token t Tok_Main in
  let t2 = match_token t1 Tok_LParen in
  let t3 = match_token t2 Tok_RParen in
  let t4 = match_token t3 Tok_LBrace in
  let (t5, stmt) = parse_stmt t4 in
  let t6 = match_token t5 Tok_RBrace in
  if t6 <> [EOF] then
    raise (InvalidInputException "parse_main failed")
  else
    stmt
  