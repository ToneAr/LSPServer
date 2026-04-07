Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "OptionsArityTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "OptionsArityTest.wl"}]];
(* Populate the PacletIndex for this file so that cross-function pattern inference works. *)
LSPServer`PacletIndex`UpdateFileIndex[uri, ReadString[FileNameJoin[{DirectoryName[$TestFileName], "OptionsArityTest.wl"}]]];

LSPServer`handleContent[
  <|"method" -> "textDocument/runWorkspaceDiagnostics",
    "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
  |>
];


(* ── Test 1: Case A – Rule option arg should NOT produce arity warning (line 9 = LSP 8) ── *)
VerificationTest[
  Module[{result, diags},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "DocCommentArityMismatch"] &&
        #["range"]["start"]["line"] === 8 &]
  ]
  ,
  {},
  TestID -> "IDE-Test-OptionsArity-NoArityWarnRuleArg"
]


(* ── Test 2: Case C – RuleDelayed option arg should NOT produce arity warning (line 17 = LSP 16) ── *)
VerificationTest[
  Module[{result, diags},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "DocCommentArityMismatch"] &&
        #["range"]["start"]["line"] === 16 &]
  ]
  ,
  {},
  TestID -> "IDE-Test-OptionsArity-NoArityWarnRuleDelayedArg"
]


(* ── Test 3: Case B – Integer arg with option still warns about type mismatch (line 13 = LSP 12) ──
   The Rule option is stripped, leaving fixedArityFn[42] — which IS a type mismatch for _String. *)
VerificationTest[
  Module[{result, diags},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    Length[Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "DocCommentInputMismatch"] &&
        #["range"]["start"]["line"] === 12 &]] > 0
  ]
  ,
  True,
  TestID -> "IDE-Test-OptionsArity-TypeMismatchStillFires"
]
