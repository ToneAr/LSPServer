Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "ClosureScopeTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "ClosureScopeTest.wl"}]];
(* Populate the PacletIndex for this file so that cross-function pattern inference works. *)
LSPServer`PacletIndex`UpdateFileIndex[uri, ReadString[FileNameJoin[{DirectoryName[$TestFileName], "ClosureScopeTest.wl"}]]];

LSPServer`handleContent[
  <|"method" -> "textDocument/runWorkspaceDiagnostics",
    "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
  |>
];


(* ── Test 1: needsStr[x] at file scope (line 38 = LSP line 37) should NOT warn ──
   x at file scope has no closure type. needsStr expects _String.
   Without fix: x bleeds Integer from Module above → mismatch fires (WRONG).
   With fix: x filtered out by closureRange → no type → no warning. *)
VerificationTest[
  Module[{result, diags},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "DocCommentInputMismatch"] &&
        #["range"]["start"]["line"] === 37 &]
  ]
  ,
  {},   (* no mismatch on line 37 *)
  TestID -> "IDE-Test-ClosureScope-NoBleedAtFileScope"
]


(* ── Test 2: needsStr[x] inside Module[{x=5},...] (line 41 = LSP line 40) SHOULD warn ──
   x is Integer inside the module. needsStr expects _String → mismatch.
   "  needsStr[x]" → 'x' is at character 11 (0-based).
   This verifies the fix doesn't suppress legitimate warnings inside closures. *)
VerificationTest[
  Module[{result, diags},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "DocCommentInputMismatch"] &&
        #["range"]["start"]["line"] === 40 &]
  ]
  ,
  {
    <|
      "code" -> "DocCommentInputMismatch\[VeryThinSpace]\:25bb\[VeryThinSpace]needsStr",
      "message" -> "Argument 1 of \"needsStr\" does not match any declared input pattern. Expected _String, got a Integer.",
      "severity" -> 1,
      "range" -> <|
        "start" -> <|"line" -> 40, "character" -> 11|>,
        "end"   -> <|"line" -> 40, "character" -> 12|>
      |>,
      "source" -> "wolfram lint"
    |>
  },
  TestID -> "IDE-Test-ClosureScope-WarnInsideClosure"
]
