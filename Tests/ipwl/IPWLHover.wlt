Get[FileNameJoin[{DirectoryName[$TestFileName], "..", "hover", "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "IPWLHoverTest.ipwl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "IPWLHoverTest.ipwl"}]];
LSPServer`PacletIndex`UpdateFileIndex[uri, ReadString[FileNameJoin[{DirectoryName[$TestFileName], "IPWLHoverTest.ipwl"}]]];

(* Test 1: variable with DeclaredType -> "Declared type:" label (not "Inferred Pattern:") *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 1,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>,
                    "position" -> <|"line" -> 7, "character" -> 0|>|>
    |>
  ],
  {<|"jsonrpc" -> "2.0", "id" -> 1,
     "result" -> <|"contents" -> <|"kind" -> "markdown",
       "value" -> "**Declared type:** `_String`"
     |>|>
  |>},
  TestID -> "IPWL-Hover-DeclaredTypeVariable"
]

(* Test 2: function with DeclaredType -> hover shows "Declared type:" label for return type *)
VerificationTest[
  Module[{val = LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 2,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>,
                    "position" -> <|"line" -> 4, "character" -> 0|>|>
    |>
  ][[1]]["result"]["contents"]["value"]},
    StringQ[val] && StringContainsQ[val, "Declared type"] && StringContainsQ[val, "_Integer"]
  ],
  True,
  TestID -> "IPWL-Hover-DeclaredTypeFunction"
]
