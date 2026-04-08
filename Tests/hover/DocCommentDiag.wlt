Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "DocCommentTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "DocCommentTest.wl"}]];

(* Populate the PacletIndex so that InputPatterns are available for diagnostics. *)
LSPServer`PacletIndex`UpdateFileIndex[uri, ReadString[FileNameJoin[{DirectoryName[$TestFileName], "DocCommentTest.wl"}]]];

(*
Run workspace diagnostics.  The call-site input-mismatch check is file-local
and does not need $WorkspaceRootPath; it runs as long as the PacletIndex has
been populated by UpdateFileIndex above.
*)
LSPServer`handleContent[
  <|"method" -> "textDocument/runWorkspaceDiagnostics",
    "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
  |>
];

(* ── Test 8: computeSquare[5.] triggers a DocCommentInputMismatch warning ── *)
(*
computeSquare[ 5. ] is on line 62 (1-based) = line 61 (0-based).
5. starts at column 16 (1-based) = character 15 (0-based), ends at character 17.
The param pattern _Integer is inferred from the definition LHS x_Integer.
No doc comment is required - the warning fires based on the typed definition alone.
*)
VerificationTest[
  Module[{result, diags},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    (* Filter for the DocCommentInputMismatch on line 75 (0-based) *)
    Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "DocCommentInputMismatch"] &&
        #["range"]["start"]["line"] === 75 &]
  ]
  ,
  {
    <|
      "code" -> "DocCommentInputMismatch\[VeryThinSpace]\:25bb\[VeryThinSpace]computeSquare",
      "message" -> "Argument 1 of \"computeSquare\" does not match any declared input pattern. Expected _Integer, got a Real.",
      "severity" -> 2,
      "range" -> <|
        "start" -> <|"line" -> 75, "character" -> 15|>,
        "end"   -> <|"line" -> 75, "character" -> 17|>
      |>,
      "source" -> "wolfram lint"
    |>
  },
  TestID -> "IDE-Test-DocComment-InputMismatch-RealForInteger"
]


(* ── Test 9: resultVar = computeSquare[5] does NOT trigger a mismatch ── *)
(*
computeSquare[5] on line 32 (1-based) = line 31 (0-based).
5 is an Integer, matching _Integer from the definition - no warning expected.
*)
VerificationTest[
  Module[{result, diags},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    (* Check no mismatch diagnostic points to line 31 (0-based for resultVar line) *)
    Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "DocCommentInputMismatch"] &&
        #["range"]["start"]["line"] === 31 &]
  ]
  ,
  {},
  TestID -> "IDE-Test-DocComment-InputMatch-NoWarning"
]


(* ── Test 10: StringLength[3] triggers a builtin-pattern mismatch warning ── *)
(*
builtinStrLen = StringLength[ 3 ] is on line 66 (1-based) = line 65 (0-based).
3 starts at character 30 (0-based), ends at character 31.
StringLength expects a String; passing an Integer should warn even though
StringLength is a System` builtin (defined via $BuiltinPatterns fallback).
*)
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
        #["range"]["start"]["line"] === 79 &]
  ]
  ,
  {
    <|
      "code" -> "DocCommentInputMismatch\[VeryThinSpace]\:25bb\[VeryThinSpace]StringLength",
      "message" -> "Argument 1 of \"StringLength\" does not match any declared input pattern. Expected _String, got a Integer.",
      "severity" -> 2,
      "range" -> <|
        "start" -> <|"line" -> 79, "character" -> 30|>,
        "end"   -> <|"line" -> 79, "character" -> 31|>
      |>,
      "source" -> "wolfram lint"
    |>
  },
  TestID -> "IDE-Test-DocComment-BuiltinInputMismatch-StringLength"
]


(* ── Test S1: seqVar = 42; computeSquare[seqVar] -> no warn (Integer before reassignment) ── *)
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
        #["range"]["start"]["line"] === 85 &]
  ]
  ,
  {},
  TestID -> "IDE-Test-SeqAssign-IntegerBeforeReassign-NoWarn"
]


(* ── Test S2: seqVar = "reassigned"; computeSquare[seqVar] -> WARN (String after reassignment) ── *)
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
        #["range"]["start"]["line"] === 87 &]
  ]
  ,
  {
    <|
      "code"     -> "DocCommentInputMismatch\[VeryThinSpace]\:25bb\[VeryThinSpace]computeSquare",
      "message"  -> "Argument 1 of \"computeSquare\" does not match any declared input pattern. Expected _Integer, got a String.",
      "severity" -> 2,
      "range"    -> <|
        "start" -> <|"line" -> 87, "character" -> 15|>,
        "end"   -> <|"line" -> 87, "character" -> 21|>
      |>,
      "source"   -> "wolfram lint"
    |>
  },
  TestID -> "IDE-Test-SeqAssign-StringAfterReassign-Warn"
]


(* ── Test R1: returnsMismatch - literal String body declared Return: _Integer -> WARN ── *)
(*
returnsMismatch[x_] := "wrong type"  is on line 92 (1-based) = line 91 (0-based).
The RHS "wrong type" is a String literal; declared Return: _Integer -> mismatch.
*)
VerificationTest[
  Module[{result, diags},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    Length[Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "DocCommentReturnMismatch"] &&
        #["range"]["start"]["line"] === 91 &]] > 0
  ]
  ,
  True,
  TestID -> "IDE-Test-ReturnMismatch-LiteralStringForInteger"
]


(* ── Test R2: returnsCorrect - literal String body declared Return: _String -> no warn ── *)
(*
returnsCorrect[x_] := "ok"  is on line 96 (1-based) = line 95 (0-based).
*)
VerificationTest[
  Module[{result, diags},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    Length[Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "DocCommentReturnMismatch"] &&
        #["range"]["start"]["line"] === 95 &]] === 0
  ]
  ,
  True,
  TestID -> "IDE-Test-ReturnMismatch-LiteralStringForStringOK"
]


(* ── Test R3: returnsStrFromCall - greet[x] returns _String, declared Return: _Integer -> WARN ── *)
(*
returnsStrFromCall[x_] := greet[x]  is on line 100 (1-based) = line 99 (0-based).
greet's ReturnPattern is _String; declared Return: _Integer -> mismatch.
*)
VerificationTest[
  Module[{result, diags},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    Length[Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "DocCommentReturnMismatch"] &&
        #["range"]["start"]["line"] === 99 &]] > 0
  ]
  ,
  True,
  TestID -> "IDE-Test-ReturnMismatch-CalleeStringForInteger"
]


(* ── Test R4: returnsStrFromCallOK - greet[x] returns _String, declared Return: _String -> no warn ── *)
(*
returnsStrFromCallOK[x_] := greet[x]  is on line 104 (1-based) = line 103 (0-based).
*)
VerificationTest[
  Module[{result, diags},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    Length[Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "DocCommentReturnMismatch"] &&
        #["range"]["start"]["line"] === 103 &]] === 0
  ]
  ,
  True,
  TestID -> "IDE-Test-ReturnMismatch-CalleeStringForStringOK"
]


(* ── Test R5: returnsUnknown - unknown callee, Missing["Unknown"] -> no warn ── *)
(*
returnsUnknown[x_] := someUnknownFunction[x]  is on line 108 (1-based) = line 107 (0-based).
*)
VerificationTest[
  Module[{result, diags},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    Length[Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "DocCommentReturnMismatch"] &&
        #["range"]["start"]["line"] === 107 &]] === 0
  ]
  ,
  True,
  TestID -> "IDE-Test-ReturnMismatch-UnknownCalleeNoWarn"
]
