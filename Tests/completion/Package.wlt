Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "PackageTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "PackageTest.wl"}]];

(* SystemSymbol in a package: Autocompletion of Plo*)
VerificationTest[
  completionResponse[uri, 1, 27, 3]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 1, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
  },
  TestID -> "IDE-Test-SystemSymbol-In-Package",
  SameTest -> MatchQ
]


(*
  Function is defined in the Private context in a package.
  Autocompletion of plo in Context1
*)
VerificationTest[
  completionResponse[uri, 2, 14, 11]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (*
      should contain plotVar1 but not plotVar2
      <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|>
    *)
  },
  TestID -> "IDE-Test-ContextDepth-1-In-Package",
  SameTest -> MatchQ
]


(*
  If a function is defined one level below the Private context level,
  we are not going to detect that.
  Autocompletion of plo in Private Context1
*)
VerificationTest[
  completionResponse[uri, 3, 29, 3]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 3, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (* should contain plotVar1 but not plotVar2 *)
    (* <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> *)
  },
  TestID -> "IDE-Test-ContextDepth-2-In-Package",
  SameTest -> MatchQ
]


(* Function with multiple usage in a package, defined with Set and SetDelayed *)
VerificationTest[
  completionResponse[uri, 4, 31, 3]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 4, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (* should contain any user-defined-variable: no kind 6*)
    (* <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> *)
  },
  TestID -> "IDE-Test-No-UserSymbol-In-Package",
  SameTest -> MatchQ
]


(* Scoped variable at the outer level *)
VerificationTest[
  completionResponse[uri, 5, 37, 6]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 5, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (* should contain modBlock1 ...*)
    (* <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> *)
  },
  TestID -> "IDE-Test-Scoped-Variable-1-In-Package",
  SameTest -> MatchQ
]


(* Scoped variable at the inner level *)
VerificationTest[
  completionResponse[uri, 6, 46, 11]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 6, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (* should contain modBlock1 and modVar1...*)
    (* <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> *)
  },
  TestID -> "IDE-Test-Scoped-Variable-2-In-Package",
  SameTest -> MatchQ
]


(* Scoped variable at the inner level *)
VerificationTest[
  completionResponse[uri, 7, 55, 3]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 7, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (*
      should contain functionName1 and functionName2
    ^^ user defined symbol
    ...*)
    (* <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> *)
  },
  TestID -> "IDE-Test-local-function-autocompletion-In-Package",
  SameTest -> MatchQ
]


