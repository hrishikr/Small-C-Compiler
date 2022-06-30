open TokenTypes

let tokenize input =
  let length = String.length input in

  let rec tok pos =
    if pos >= length then
      [EOF]
    else if Str.string_match (Str.regexp "-?[0-9]+") input pos then
      let value = Str.matched_string input in
      Tok_Int(int_of_string value)::(tok (pos + String.length value))
    else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos then
      let str = Str.matched_string input in
      let len = String.length str in
      if str = "true" then Tok_Bool(true)::(tok (pos + len))
      else if str = "false" then Tok_Bool(false)::(tok (pos + len))
      else if str = "int" then Tok_Int_Type::(tok (pos + len))
      else if str = "bool" then Tok_Bool_Type::(tok (pos + len))
      else if str = "printf" then Tok_Print::(tok (pos + len))
      else if str = "main" then Tok_Main::(tok (pos + len))
      else if str = "if" then Tok_If::(tok (pos + len))
      else if str = "else" then Tok_Else::(tok (pos + len))
      else if str = "for" then Tok_For::(tok (pos + len))
      else if str = "from" then Tok_From::(tok (pos + len))
      else if str = "to" then Tok_To::(tok (pos + len))
      else if str = "while" then Tok_While::(tok (pos + len))
      else Tok_ID(str)::(tok (pos + len)) 
    else if Str.string_match (Str.regexp "(") input pos then
      Tok_LParen::(tok (pos + 1))
    else if Str.string_match (Str.regexp ")") input pos then
      Tok_RParen::(tok (pos + 1))
    else if Str.string_match (Str.regexp "{") input pos then
      Tok_LBrace::(tok (pos + 1))
    else if Str.string_match (Str.regexp "}") input pos then
      Tok_RBrace::(tok (pos + 1))
    else if Str.string_match (Str.regexp "==") input pos then
      Tok_Equal::(tok (pos + 2))
    else if Str.string_match (Str.regexp "!=") input pos then
      Tok_NotEqual::(tok (pos + 2))
    else if Str.string_match (Str.regexp ">=") input pos then
      Tok_GreaterEqual::(tok (pos + 2))
    else if Str.string_match (Str.regexp "<=") input pos then
      Tok_LessEqual::(tok (pos + 2))
    else if Str.string_match (Str.regexp "=") input pos then
      Tok_Assign::(tok (pos + 1))
    else if Str.string_match (Str.regexp ">") input pos then
      Tok_Greater::(tok (pos + 1))
    else if Str.string_match (Str.regexp "<") input pos then
      Tok_Less::(tok (pos + 1))
    else if Str.string_match (Str.regexp "||") input pos then
      Tok_Or::(tok (pos + 2))
    else if Str.string_match (Str.regexp "&&") input pos then
      Tok_And::(tok (pos + 2))
    else if Str.string_match (Str.regexp "\\!") input pos then
      Tok_Not::(tok (pos + 1))
    else if Str.string_match (Str.regexp ";") input pos then
      Tok_Semi::(tok (pos + 1))
    else if Str.string_match (Str.regexp "\\+") input pos then
      Tok_Add::(tok (pos + 1))
    else if Str.string_match (Str.regexp "-") input pos then
      Tok_Sub::(tok (pos + 1))
    else if Str.string_match (Str.regexp "\\*") input pos then
      Tok_Mult::(tok (pos + 1))
    else if Str.string_match (Str.regexp "/") input pos then
      Tok_Div::(tok (pos + 1))
    else if Str.string_match (Str.regexp "\\^") input pos then
      Tok_Pow::(tok (pos + 1))
    else
      tok (pos + 1)
  in tok 0;;
    
    
