Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "BranchNarrowingTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "BranchNarrowingTest.wl"}]];

(* Populate the PacletIndex so local definitions are findable across files. *)
LSPServer`PacletIndex`UpdateFileIndex[uri, ReadString[FileNameJoin[{DirectoryName[$TestFileName], "BranchNarrowingTest.wl"}]]];

LSPServer`handleContent[
  <|"method" -> "textDocument/runWorkspaceDiagnostics",
    "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
  |>
];


(* ── Test B1: If[x1 == 5, bNarrowSq[x1], ...] ─────────────────────────────────
   True branch: x1 inferred as integer 5 -> matches _Integer -> no warn (line 6, 0-based). *)
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
        #["range"]["start"]["line"] === 6 &]
  ]
  ,
  {},
  TestID -> "BranchNarrowing-IfEqLiteral-TrueBranch-NoWarn"
]


(* ── Test B2: If[x1 == 5, ..., bNarrowSq[x1]] ──────────────────────────────────
   False branch: x1 inferred as Except[5] -> conservative, no warn (line 7, 0-based). *)
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
        #["range"]["start"]["line"] === 7 &]
  ]
  ,
  {},
  TestID -> "BranchNarrowing-IfEqLiteral-FalseBranch-NoWarn"
]


(* ── Test B3: If[StringQ[x2], bNarrowSq[x2], ...] ─────────────────────────────
   True branch: x2 inferred as String "" -> does not match _Integer -> WARN (line 13, 0-based). *)
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
        #["range"]["start"]["line"] === 13 &]
  ]
  ,
  {
    <|
      "code"     -> "DocCommentInputMismatch\[VeryThinSpace]\:25bb\[VeryThinSpace]bNarrowSq",
      "message"  -> "Argument 1 of \"bNarrowSq\" does not match any declared input pattern. Expected _Integer, got a String.",
      "severity" -> 1,
      "range"    -> <|
        "start" -> <|"line" -> 13, "character" -> 13|>,
        "end"   -> <|"line" -> 13, "character" -> 15|>
      |>,
      "source"   -> "wolfram lint"
    |>
  },
  TestID -> "BranchNarrowing-StringQ-TrueBranch-Warn"
]


(* ── Test B4: If[StringQ[x2], ..., bNarrowSq[x2]] ─────────────────────────────
   False branch: no narrowing for x2 -> unknown, no warn (line 14, 0-based). *)
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
        #["range"]["start"]["line"] === 14 &]
  ]
  ,
  {},
  TestID -> "BranchNarrowing-StringQ-FalseBranch-NoWarn"
]


(* ── Test B5: If[!StringQ[x3], bNarrowSq[x3], ...] ────────────────────────────
   True branch: condition is Not[StringQ], no positive constraint -> no warn (line 20, 0-based). *)
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
        #["range"]["start"]["line"] === 20 &]
  ]
  ,
  {},
  TestID -> "BranchNarrowing-NotStringQ-TrueBranch-NoWarn"
]


(* ── Test B6: If[!StringQ[x3], ..., bNarrowSq[x3]] ────────────────────────────
   False branch: condition Not[StringQ] -> x3 IS String -> WARN (line 21, 0-based). *)
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
        #["range"]["start"]["line"] === 21 &]
  ]
  ,
  {
    <|
      "code"     -> "DocCommentInputMismatch\[VeryThinSpace]\:25bb\[VeryThinSpace]bNarrowSq",
      "message"  -> "Argument 1 of \"bNarrowSq\" does not match any declared input pattern. Expected _Integer, got a String.",
      "severity" -> 1,
      "range"    -> <|
        "start" -> <|"line" -> 21, "character" -> 13|>,
        "end"   -> <|"line" -> 21, "character" -> 15|>
      |>,
      "source"   -> "wolfram lint"
    |>
  },
  TestID -> "BranchNarrowing-NotStringQ-FalseBranch-Warn"
]


(* ── Test B7: Switch[x4, _Integer, bNarrowSq[x4], ...] ────────────────────────
   _Integer branch: x4 inferred as 0 (Integer) -> no warn (line 26, 0-based). *)
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
        #["range"]["start"]["line"] === 26 &]
  ]
  ,
  {},
  TestID -> "BranchNarrowing-Switch-IntegerBranch-NoWarn"
]


(* ── Test B8: Switch[x4, ..., _String, bNarrowSq[x4]] ─────────────────────────
   _String branch: x4 inferred as "" (String) -> WARN (line 27, 0-based). *)
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
        #["range"]["start"]["line"] === 27 &]
  ]
  ,
  {
    <|
      "code"     -> "DocCommentInputMismatch\[VeryThinSpace]\:25bb\[VeryThinSpace]bNarrowSq",
      "message"  -> "Argument 1 of \"bNarrowSq\" does not match any declared input pattern. Expected _Integer, got a String.",
      "severity" -> 1,
      "range"    -> <|
        "start" -> <|"line" -> 27, "character" -> 23|>,
        "end"   -> <|"line" -> 27, "character" -> 25|>
      |>,
      "source"   -> "wolfram lint"
    |>
  },
  TestID -> "BranchNarrowing-Switch-StringBranch-Warn"
]
