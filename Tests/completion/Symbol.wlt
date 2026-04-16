Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "SymbolTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "SymbolTest.wl"}]];

(*
    SystemSymbol in a script file: Autocompletion of Plo
    no user-symbol
*)
VerificationTest[
  completionResponse[uri, 1, 5, 3]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 1, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
  },
  TestID -> "IDE-Test-SystemSymbol",
  SameTest -> MatchQ
]


(*
  SystemSymbol and use-symbol in a script file: Autocompletion of plo
*)
VerificationTest[
  completionResponse[uri, 2, 7, 3]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (*
      should contain plotVar1 but not plotVar2
      <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|>
    *)
  },
  TestID -> "IDE-Test-UserSymbol-and-SystemSymbol",
  SameTest -> MatchQ
]


(*
  Scoped local variable test
*)
VerificationTest[
  completionResponse[uri, 3, 14, 7]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 3, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (* should contain var01, var02 *)
    (* <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> *)
  },
  TestID -> "IDE-Test-scoped-1",
  SameTest -> MatchQ
]


VerificationTest[
  completionResponse[uri, 4, 15, 7]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 4, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (* should contain name01, name02 not name1, name2*)
    (* <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> *)
  },
  TestID -> "IDE-Test-scoped-2",
  SameTest -> MatchQ
]


(* Scoped variable at the outer level *)
VerificationTest[
  completionResponse[uri, 5, 34, 7]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 5, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (* No modBlock*)
    (* <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> *)
  },
  TestID -> "IDE-Test-Scoped-Outer-Block",
  SameTest -> MatchQ
]


(* Scoped variable at the inner level *)
VerificationTest[
  completionResponse[uri, 6, 44, 11]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 6, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (* should contain modBlock and modVar ...*)
    (* <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> *)
  },
  TestID -> "IDE-Test-Scoped-Inner-Block",
  SameTest -> MatchQ
]


(* Scoped variable at the inner level *)
VerificationTest[
  completionResponse[uri, 7, 9, 4]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 7, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (* should contain functionName1 and functionName2; user-defined variable*)
    (* <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> *)
  },
  TestID -> "IDE-Test-Function-Name",
  SameTest -> MatchQ
]


