Get[FileNameJoin[{DirectoryName[$TestFileName], "..", "hover", "Init.wl"}]];

fixturePath = FileNameJoin[{DirectoryName[$TestFileName], "BuiltinVariadicTest.wl"}];
initFunction[fixturePath];
uri = LocalObjects`PathToURI[fixturePath];

LSPServer`PacletIndex`UpdateFileIndex[uri, ReadString[fixturePath]];
LSPServer`handleContent[
  <|"method" -> "textDocument/runWorkspaceDiagnostics",
    "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
  |>
];

result = LSPServer`handleContent[
  <|"method" -> "textDocument/publishDiagnostics",
    "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
  |>
];
allDiags = result[[1, "params", "diagnostics"]];

(* Test 1: Plus[x,y,z] with 3 Integer args should NOT trigger DeclaredTypeMismatch *)
VerificationTest[
  NoneTrue[allDiags,
    StringStartsQ[Lookup[#, "code", ""], "DeclaredTypeMismatch"] &&
    StringContainsQ[Lookup[#, "message", ""], "addThreeInts"] &],
  True,
  TestID -> "Builtin-Variadic-Plus-NoMismatch"
]

(* Test 2: StringJoin[s1,s2,s3] with 3 String args should NOT trigger DeclaredTypeMismatch *)
VerificationTest[
  NoneTrue[allDiags,
    StringStartsQ[Lookup[#, "code", ""], "DeclaredTypeMismatch"] &&
    StringContainsQ[Lookup[#, "message", ""], "joinThreeStrings"] &],
  True,
  TestID -> "Builtin-Variadic-StringJoin-NoMismatch"
]

(* Test 3: And[a,b,c] with 3 args should NOT trigger DeclaredTypeMismatch *)
VerificationTest[
  NoneTrue[allDiags,
    StringStartsQ[Lookup[#, "code", ""], "DeclaredTypeMismatch"] &&
    StringContainsQ[Lookup[#, "message", ""], "andThree"] &],
  True,
  TestID -> "Builtin-Variadic-And-NoMismatch"
]

(* Tests 4–8: unit tests for helper functions added in Task 2.
   These FAIL until builtinSpecToPattern and overloadSpecMatchesArgs are
   defined in LSPServer`Diagnostics`Private`. That is expected — TDD. *)

(* Test 4: builtinSpecToPattern converts "Integer..." to BlankSequence[Integer] *)
VerificationTest[
  LSPServer`Diagnostics`Private`builtinSpecToPattern["Integer..."],
  BlankSequence[Integer],
  TestID -> "Builtin-SpecToPattern-BlankSequence"
]

(* Test 5: builtinSpecToPattern converts "String*" to BlankNullSequence[String] *)
VerificationTest[
  LSPServer`Diagnostics`Private`builtinSpecToPattern["String*"],
  BlankNullSequence[String],
  TestID -> "Builtin-SpecToPattern-BlankNullSequence"
]

(* Test 6: overloadSpecMatchesArgs matches variadic overload with 3-arg Integer call *)
VerificationTest[
  LSPServer`Diagnostics`Private`overloadSpecMatchesArgs[
    {"Integer..."},
    {0, 1, 2}
  ],
  True,
  TestID -> "Builtin-VariadicMatch-ThreeInts"
]

(* Test 7: overloadSpecMatchesArgs rejects type mismatch in variadic tail *)
VerificationTest[
  LSPServer`Diagnostics`Private`overloadSpecMatchesArgs[
    {"Integer..."},
    {0, "hello", 2}
  ],
  False,
  TestID -> "Builtin-VariadicMatch-TypeMismatch"
]

(* Test 8: overloadSpecMatchesArgs matches fixed-arity overload *)
VerificationTest[
  LSPServer`Diagnostics`Private`overloadSpecMatchesArgs[
    {"String", "Integer"},
    {"", 0}
  ],
  True,
  TestID -> "Builtin-FixedArity-Match"
]
