//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a tuple containing 
// 3 values:  
//
//   (result, msg, program)
//
// where result is true or false (legal or not legal), 
// msg is a success or syntax error message, and program
// is a list of instructions if parsing was successful.
//
// <<YOUR NAME>>
// U. of Illinois, Chicago
// CS 341, Spring 2019
// Project #05
//

#light

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // These are debug routines that output the tokens, or
  // program, respectively.  They are written so you can
  // inject these into a pipeline to output the current 
  // state of the tokens or program.
  //
  let private __outputTokens (tokens, program) =
    printfn "Tokens: %A" tokens
    (tokens, program)

  let private __outputProgram (tokens, program) =
    printfn "Program: %A" program
    (tokens, program)


  //
  // matchToken
  //
  let private matchToken expected_token (tokens, program) =
    let (token, _) = List.head tokens
    //
    // if the token matches the expected token, keep parsing by
    // returning the rest of the tokens.  Otherwise throw an
    // exception because there's a syntax error, effectively 
    // stopping compilation:
    //
    if expected_token = token then  
      (List.tail tokens, program)
    else
      failwith ("expecting " + (string expected_token) + ", but found " + (string token))

  let private empty (tokens, program) =
    let (nextToken, _) = List.head tokens
    if nextToken = lexer.Tokens.CloseBrace then
      (tokens, ["$EMPTY"]::program)
    else
      let (T1,P1) = matchToken lexer.Tokens.Semicolon (tokens, program)
      (T1, ["$EMPTY"]::P1)

  let private vardecl (tokens, program) =
    let (T1, P1) = matchToken lexer.Tokens.Int (tokens, program)
    let (_,ID) = List.head T1
    let (T2, P2) = matchToken lexer.Tokens.ID (T1,P1)
    let (T3, P3) = matchToken lexer.Tokens.Semicolon (T2, P2)
    (T3, ["$DECL";ID]::P3)


  let private input (tokens, program) = 
    let (T1, P1) = matchToken lexer.Tokens.Cin (tokens, program)
    let (T2, P2) = matchToken lexer.Tokens.Input (T1, P1)
    let (_, ID) = List.head T2
    let (T3, P3) = matchToken lexer.Tokens.ID (T2, P2)
    let (T4, P4) = matchToken lexer.Tokens.Semicolon (T3, P3)
    (T4, ["$INPUT";ID]::P4)

  let private is_expr_value token =
    token = lexer.Tokens.ID || token = lexer.Tokens.Int_Literal || token = lexer.Tokens.Str_Literal || token = lexer.Tokens.Bool_Literal

  let private expr_value (tokens, program) =
    let (nextToken, value) = List.head tokens
    if nextToken = lexer.Tokens.ID then
      let (T1,P1) = matchToken lexer.Tokens.ID (tokens, program)
      (T1, [(string nextToken); value]::P1)
    else if nextToken = lexer.Tokens.Int_Literal then
      let (T1,P1) = matchToken lexer.Tokens.Int_Literal (tokens, program)
      (T1, [(string nextToken); value]::P1)
    else if nextToken = lexer.Tokens.Str_Literal then
      let (T1,P1) = matchToken lexer.Tokens.Str_Literal (tokens, program)
      (T1, [(string nextToken); value]::P1)
    else if nextToken = lexer.Tokens.Bool_Literal then
      let (T1,P1) = matchToken lexer.Tokens.Bool_Literal (tokens, program)
      (T1, [(string nextToken); value]::P1)
    else
      failwith ("expecting identifier or literal, but found " + (string nextToken))

  let private output_value (tokens, program) =
    let (nextToken, value) = List.head tokens
    if nextToken = lexer.Tokens.Endl then
      let (T1, P1) = matchToken lexer.Tokens.Endl (tokens, program)
      (T1, ["$OUTPUT";(string nextToken);value]::P1)
    else if is_expr_value nextToken then
      let (T1,P1) = expr_value (tokens, program)
      (T1, (["$OUTPUT"]@(List.head P1))::(List.tail P1))
    else
      failwith ("expecting identifier or literal or endl, but found " + (string nextToken))

  let private output (tokens, program) = 
    (tokens, program)
    |> matchToken lexer.Tokens.Cout
    |> matchToken lexer.Tokens.Output
    |> output_value
    |> matchToken lexer.Tokens.Semicolon

  let private expr_op (tokens, program) = 
    let (nextToken, value) = List.head tokens
    if nextToken = lexer.Tokens.Plus then
      let (T1,P1) = matchToken lexer.Tokens.Plus (tokens, program)
      (T1, [value]::P1)
    else if nextToken = lexer.Tokens.Minus then
      let (T1,P1) = matchToken lexer.Tokens.Minus (tokens, program)
      (T1, [value]::P1)
    else if nextToken = lexer.Tokens.Times then
      let (T1,P1) = matchToken lexer.Tokens.Times (tokens, program) 
      (T1, [value]::P1)
    else if nextToken = lexer.Tokens.Divide then
      let (T1,P1) = matchToken lexer.Tokens.Divide (tokens, program)
      (T1, [value]::P1)
    else if nextToken = lexer.Tokens.Power then
      let (T1,P1) = matchToken lexer.Tokens.Power (tokens, program)
      (T1, [value]::P1)
    else if nextToken = lexer.Tokens.LT then
      let (T1,P1) = matchToken lexer.Tokens.LT (tokens, program)
      (T1, [value]::P1)
    else if nextToken = lexer.Tokens.LTE then
      let (T1,P1) = matchToken lexer.Tokens.LTE (tokens, program)
      (T1, [value]::P1)
    else if nextToken = lexer.Tokens.GT then
      let (T1,P1) = matchToken lexer.Tokens.GT (tokens, program)
      (T1, [value]::P1)
    else if nextToken = lexer.Tokens.GTE then
      let (T1,P1) = matchToken lexer.Tokens.GTE (tokens, program)
      (T1, [value]::P1)
    else if nextToken = lexer.Tokens.EQ then
      let (T1,P1) = matchToken lexer.Tokens.EQ (tokens, program)
      (T1, [value]::P1)
    else if nextToken = lexer.Tokens.NE then
      let (T1,P1) =  matchToken lexer.Tokens.NE (tokens, program)
      (T1, [value]::P1)
    else 
      failwith ("expecting operation, but found " + (string nextToken))

  let private expr (tokens, program) = 
    let (T1, P1) = expr_value (tokens, program)
    let (nextToken,_) = List.head T1
    if nextToken = lexer.Tokens.Semicolon || nextToken = lexer.Tokens.CloseParen then
      (T1, P1)
    else
      let (T2,P2) = expr_op (T1, P1)
      let (T3,P3) = (T2, ((List.head (List.tail P2))@(List.head P2))::(List.tail (List.tail P2)))
      let (T4,P4) = expr_value (T3,P3)
      (T4, ((List.head (List.tail P4)@(List.head P4)))::(List.tail (List.tail P4)))

  let private assignment (tokens, program) = 
    let (_, id) = List.head tokens
    let (T1,P1) = matchToken lexer.Tokens.ID (tokens, program)
    let (T2,P2) = matchToken lexer.Tokens.Assign (T1,P1)
    let (T3,P3) = expr (T2,P2)
    let (T4,P4) = (T3, (["$ASSIGN";id]@(List.head P3))::(List.tail P3))
    matchToken lexer.Tokens.Semicolon (T4,P4)
    

  let private condition (tokens, program) = 
    expr (tokens, program)

  let rec private stmt (tokens, program) =
    let (nextToken, _) = List.head tokens
    if nextToken = lexer.Tokens.Int then
      vardecl (tokens, program)
    else if nextToken = lexer.Tokens.Cin then
      input (tokens, program)
    else if nextToken = lexer.Tokens.Cout then
      output (tokens, program)
    else if nextToken = lexer.Tokens.ID then
      assignment (tokens, program)
    else if nextToken = lexer.Tokens.If then
      ifstmt (tokens, program)
    else if nextToken = lexer.Tokens.Semicolon then
      empty (tokens, program)
    else
      failwith ("expecting statement, but found " + (string nextToken))
    
  and private then_part (tokens, program) =
    stmt (tokens, program)
  and private else_part (tokens, program) =
    let (nextToken, _) = List.head tokens
    if nextToken = lexer.Tokens.Else then
      (tokens, program)
      |> matchToken lexer.Tokens.Else 
      |> stmt
    else
      (tokens, ["$EMPTY"] :: program)

  and private ifstmt (tokens, program) = 
    let (T1,P1) = matchToken lexer.Tokens.If (tokens, program)
    let (T2,P2) = matchToken lexer.Tokens.OpenParen (T1, P1)
    let (T3,P3) = condition (T2, P2)
    let (T4,P4) = (T3, (["$IF"]@(List.head P3))::(List.tail P3))
    let (T5,P5) = matchToken lexer.Tokens.CloseParen (T4, P4)
    let (T6,P6) = then_part (T5, P5)
    else_part (T6, P6)



  let rec private morestmts (tokens, program) =
    let (nextToken, _) = List.head tokens
    if List.isEmpty tokens || nextToken = lexer.Tokens.CloseBrace then
      (tokens, program)
    else
      (tokens, program)
      |> stmt
      |> morestmts


  let private stmts (tokens, program) =
    (tokens, program)
    |> stmt
    |> morestmts

  //
  // simpleC
  // 
  let private simpleC (tokens, program) = 
    (tokens, program)
    |> matchToken lexer.Tokens.Void
    |> matchToken lexer.Tokens.Main
    |> matchToken lexer.Tokens.OpenParen
    |> matchToken lexer.Tokens.CloseParen
    |> matchToken lexer.Tokens.OpenBrace
    |> stmts
    |> matchToken lexer.Tokens.CloseBrace
    |> matchToken lexer.Tokens.EOF


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // a tuple containing 3 values:  
  //
  //   (result, msg, program)
  //
  // where result is true or false (legal or not legal), 
  // msg is a success or syntax error message, and program
  // is a list of instructions if parsing was successful.
  //
  let parse tokens = 
    try
      let (_, program) = simpleC (tokens, [])
      (true, "success", List.rev program)
    with 
      | ex -> (false, ex.Message, [])
