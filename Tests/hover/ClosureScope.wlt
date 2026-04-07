Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "ClosureScopeTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "ClosureScopeTest.wl"}]];
LSPServer`PacletIndex`UpdateFileIndex[uri, ReadString[FileNameJoin[{DirectoryName[$TestFileName], "ClosureScopeTest.wl"}]]];


(* ── Test 1: Case A – hover after body reassignment: expect _String ── *)
(* Line 9 (1-based) = LSP line 8 (0-based). "  x" -> x at character 2. *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 1,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 8, "character" -> 2|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 1,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_String`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-ClosureScope-FlowAfterReassign"
]


(* ── Test 2: Case B – hover before body reassignment: expect _Integer ── *)
(* Line 16 (1-based) = LSP line 15 (0-based). "  x;" -> x at character 2. *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 2,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 15, "character" -> 2|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 2,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_Integer`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-ClosureScope-FlowBeforeReassign"
]


(* ── Test 3: Case C – uninitialized local: expect _ ── *)
(* Line 23 (1-based) = LSP line 22 (0-based). "  x" -> x at character 2. *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 3,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 22, "character" -> 2|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 3,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-ClosureScope-Uninit"
]


(* ── Test 4: Case D – module-local x is Integer: expect _Integer ── *)
(* Line 33 (1-based) = LSP line 32 (0-based). "  x" -> x at character 2. *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 4,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 32, "character" -> 2|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 4,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_Integer`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-ClosureScope-ModuleLocalInt"
]
