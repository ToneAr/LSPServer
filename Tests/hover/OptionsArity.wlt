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


(* ── Test 4: Case D – forwarded OptionsPattern variable should NOT count as arity ── *)
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
        StringContainsQ[Lookup[#, "message", ""], "optionForwardTarget"] &]
  ]
  ,
  {},
  TestID -> "IDE-Test-OptionsArity-NoArityWarnForwardedOptionsPattern"
]


(* Test 5: assigned/aliased OptionsPattern variable should NOT count as arity *)
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
        StringContainsQ[Lookup[#, "message", ""], "optionForwardTarget"] &&
        #["range"]["start"]["line"] >= 25 &]
  ]
  ,
  {},
  TestID -> "IDE-Test-OptionsArity-NoArityWarnAliasedOptionsPattern"
]


(* Test 6: Case E - conditioned overload definition should NOT be treated as a call *)
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
        StringContainsQ[Lookup[#, "message", ""], "conditionedOverloadFn"] &]
  ]
  ,
  {},
  TestID -> "IDE-Test-OptionsArity-NoArityWarnConditionedOverloadDefinition"
]


(* Test 7: a named OptionsPattern[] parameter should infer OptionsPattern[owningHead] *)
VerificationTest[
  Module[{result},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/hoverFencepost",
        "id" -> 7,
        "params" -> <|
          "textDocument" -> <|"uri" -> uri|>,
          "position" -> <|"line" -> 43, "character" -> 5|>
        |>
      |>
    ];
    result[[1]]["result"]["contents"]["value"]
  ],
  "**Inferred Pattern:** `OptionsPattern[discoverDomainRoots]`",
  TestID -> "IDE-Test-OptionsArity-HoverNamedDefaultOptionsPattern"
]


(* Test 8: OptionsPattern[] and ___Rules/{___Rules} captures should forward equivalently. *)
VerificationTest[
  Module[{result, diags, names},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    names = {
      "optionsTargetForRules",
      "rulesSequenceTarget",
      "rulesListTarget",
      "sequenceFilterRulesTarget"
    };
    Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "DocCommentArityMismatch"] &&
        AnyTrue[names, StringContainsQ[Lookup[#, "message", ""], #] &] &]
  ],
  {},
  TestID -> "IDE-Test-OptionsArity-RulesAndOptionsPatternForwardingEquivalent"
]
