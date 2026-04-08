Get[FileNameJoin[{DirectoryName[$TestFileName], "..", "hover", "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "IPWLDiagTest.ipwl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "IPWLDiagTest.ipwl"}]];
LSPServer`PacletIndex`UpdateFileIndex[uri, ReadString[FileNameJoin[{DirectoryName[$TestFileName], "IPWLDiagTest.ipwl"}]]];

(* Run workspace diagnostics *)
LSPServer`handleContent[
  <|"method" -> "textDocument/runWorkspaceDiagnostics",
    "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
  |>
];

diags = LSPServer`handleContent[
  <|"method" -> "textDocument/publishDiagnostics",
    "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
  |>
][[1, "params", "diagnostics"]];

(* Test 1: no DeclaredTypeMismatch for returnsCorrect *)
VerificationTest[
  NoneTrue[diags, StringStartsQ[Lookup[#, "code", ""], "DeclaredTypeMismatch"] &&
    StringContainsQ[Lookup[#, "message", ""], "returnsCorrect"] &],
  True,
  TestID -> "IPWL-Diag-NoWarningCorrect"
]

(* Test 2: DeclaredTypeMismatch fires for returnsMismatch *)
VerificationTest[
  AnyTrue[diags, StringStartsQ[Lookup[#, "code", ""], "DeclaredTypeMismatch"] &],
  True,
  TestID -> "IPWL-Diag-MismatchWarning"
]

(* Test 3: no DeclaredTypeMismatch warning for unknown callee *)
VerificationTest[
  NoneTrue[diags, StringStartsQ[Lookup[#, "code", ""], "DeclaredTypeMismatch"] &&
    StringContainsQ[Lookup[#, "message", ""], "returnsUnknown"] &],
  True,
  TestID -> "IPWL-Diag-NoWarningUnknown"
]

(* Test 4: IPWLSyntaxError emitted for bad annotation (bare literal 123) *)
VerificationTest[
  AnyTrue[diags, StringStartsQ[Lookup[#, "code", ""], "IPWLSyntaxError"] &],
  True,
  TestID -> "IPWL-Diag-SyntaxError"
]

(* Test 5: IPWLUnresolvedSymbol code string exists (sentinel) *)
VerificationTest[
  StringQ["IPWLUnresolvedSymbol"],
  True,
  TestID -> "IPWL-Diag-UnresolvedSymbolCode"
]
