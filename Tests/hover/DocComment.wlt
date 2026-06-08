Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "DocCommentTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "DocCommentTest.wl"}]];

(* Populate the PacletIndex for this file so that cross-function pattern inference works. *)
LSPServer`PacletIndex`UpdateFileIndex[uri, ReadString[FileNameJoin[{DirectoryName[$TestFileName], "DocCommentTest.wl"}]]];


(* ── Test 1: single overload with full doc-comment (Description + inferred Params + Return) ── *)
(* computeSquare is on line 8 (1-based), so LSP line 7 (0-based). *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 1,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 7, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 1,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Delayed Definitions**\n\n```wolfram\ncomputeSquare[x_Integer]\n```\n\n---\n**Doc Comments**\n\nCompute the square of an integer.  \n**Parameters:** `_Integer`  \n**Returns:** `_Integer`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-FullDocComment"
]


(* ── Test 2: first overload of greet - has full doc-comment ── *)
(* greet first overload is on line 16 (1-based), LSP line 15 (0-based). *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 2,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 15, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 2,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Delayed Definitions**\n\n```wolfram\ngreet[name_String]\ngreet[x_]\n```\n\n---\n**Doc Comments**\n\nGreet a person.  \n**Parameters:** `_String`  \n**Returns:** `_String`\n\nGreet a person.  \n**Returns:** `_String`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-DescriptionInheritance-FirstOverload"
]


(* ── Test 3: second overload of greet - Return-only, inherits Description ── *)
(* greet second overload is on line 18 (1-based), LSP line 17 (0-based). *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 3,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 17, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 3,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Delayed Definitions**\n\n```wolfram\ngreet[name_String]\ngreet[x_]\n```\n\n---\n**Doc Comments**\n\nGreet a person.  \n**Parameters:** `_String`  \n**Returns:** `_String`\n\nGreet a person.  \n**Returns:** `_String`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-DescriptionInheritance-SecondOverload"
]


(* ── Test 4: variable with literal integer RHS - inferred type _Integer ── *)
(* myIntVar is on line 21 (1-based), LSP line 20 (0-based). *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 4,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 20, "character" -> 0|>|>
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
  TestID -> "IDE-Test-DocComment-InferredInteger"
]


(* ── Test 5: variable with literal string RHS - inferred type _String ── *)
(* myStrVar is on line 24 (1-based), LSP line 23 (0-based). *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 5,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 23, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 5,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_String`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-InferredString"
]


(* ── Test 6: variable with List literal RHS - inferred type {___Integer} ── *)
(* myListVar = {1, 2, 3} is on line 27 (1-based), LSP line 26 (0-based). *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 6,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 26, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 6,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `{___Integer}`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-InferredList"
]


(* ── Test 7: variable assigned from a doc-commented function call ── *)
(* resultVar = computeSquare[5] is on line 32 (1-based), LSP line 31 (0-based). *)
(* After UpdateFileIndex above, computeSquare has a DocComment with ReturnPattern -> _Integer
   in the PacletIndex, so resultVar's inferred type is _Integer. *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 7,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 31, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 7,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_Integer`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-InferredFromDocCommentedFunction"
]


(* ── Test 8: variable assigned from Echo @ f[{1., 1.}] - transitively _Real ── *)
(* var = Echo @ f[{1., 1.}] is on line 43 (1-based), LSP line 42 (0-based).
   f[{x_Real, y_Real}] returns _Real (doc-commented).
   Echo has _arg1 in $BuiltinPatterns, so resolveCallReturnPattern recurses
   through Echo into f to resolve _Real transitively. *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 8,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 42, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 8,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_Real`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-InferredThroughEcho"
]


(* ── Test 9a: builtinStrLen = StringLength["hello"] -> _Integer ── *)
(* Line 101 (1-based), LSP line 100 (0-based). *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 9,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 100, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 9,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_Integer`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-BuiltinStrLen"
]


(* ── Test 9b: builtinSort = Accumulate[{3,1,2}] -> _List ── *)
(* Line 102 (1-based), LSP line 101 (0-based). *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 10,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 101, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 10,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_List`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-BuiltinSort"
]


(* ── Test 9c: builtinEcho = Echo["world"] -> _String (passthrough of "world") ── *)
(* Line 104 (1-based), LSP line 103 (0-based). *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 11,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 103, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 11,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_String`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-BuiltinEcho"
]


(* ── Test 14: a = Enclose[1+2, Function[e, e; 2]] -> _Integer ──
   DocCommentTest.wl line 88 (1-based) = LSP line 87 (0-based).
   Body type: 1+2 -> _Integer. Callback body is CompoundExpression[e, 2];
   last element 2 -> _Integer, so cbPat = _Integer. Combined: _Integer | _Integer = _Integer. *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 14,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 87, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 14,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_Integer`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-EncloseBodyType"
]


(* ── Test 13: e parameter of Enclose failure callback -> _Failure ──
   DocCommentTest.wl line 90 (1-based) = LSP line 89 (0-based).
   Tab (char 0) + "Function[" (chars 1-9) + "e" at char 10. *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 13,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 89, "character" -> 10|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 13,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_Failure`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-EncloseCallbackParam"
]


(* ── Test 12: a after If[b==5,...] – position-aware fallback to comparison-If ──
   a is assigned twice: first If[b>5, b; b, b; 2.] (line 71), then If[b==5, b; b, b; 2.] (line 78).
   The b==5 assignment returns None (equality If stays opaque), so position-aware hover
   falls back to the b>5 block which gives _Real | _Integer?(#1 > 5 & ).
   Cursor: DocCommentTest.wl line 85 (1-based) = LSP line 84 (0-based). *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 12,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 84, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 12,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_Real | _Integer?(#1 > 5 & )`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-IfComparisonFallback"
]


(* ── Test 13: TagSetDelayed with Return: doc-comment ── *)
(*
j /: Plot[j] := "plotting f" is a TagSetDelayed on line 46 (1-based) with Return: _String.
a = Plot[j] is on line 47 (1-based) = line 46 (0-based).
InferredPattern of a should resolve to _String via the TagSetDelayed entry.
*)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 13,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 46, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 13,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_String`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-TagSetDelayedReturn"
]


(* ── Test 15: modReturn = Module[{}, "hello"] -> _String (closure return type) ──
   DocCommentTest.wl line 157 (1-based) = LSP line 156 (0-based). *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 15,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 156, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 15,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_String`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-Module-ClosureReturnString"
]


(* ── Test 16: t in Module[{t = 42}, t] body -> _Integer ──
   DocCommentTest.wl line 160 (1-based) = LSP line 159 (0-based).
   t is at character 17 (0-based). *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 16,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 159, "character" -> 17|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 16,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_Integer`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-Module-LocalVarInteger"
]


(* ── Test 17: s in With[{s = "abc"}, s] body -> _String ──
   DocCommentTest.wl line 163 (1-based) = LSP line 162 (0-based).
   s is at character 18 (0-based). *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 17,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 162, "character" -> 18|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 17,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_String`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-With-LocalVarString"
]


(* ── Test 18: blockReturn = Block[{}, 99] -> _Integer (closure return type) ──
   DocCommentTest.wl line 166 (1-based) = LSP line 165 (0-based). *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 18,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 165, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 18,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_Integer`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-Block-ClosureReturnInteger"
]


docCommentTestLines = StringSplit[
  ReadString[FileNameJoin[{DirectoryName[$TestFileName], "DocCommentTest.wl"}]],
  {"\r\n", "\n", "\r"},
  All
];
lineContaining[needle_String] := First[FirstPosition[
  docCommentTestLines,
  line_String /; StringContainsQ[line, needle]
]] - 1;

(* ___[All] homogeneous call arguments -> ___Integer *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 19,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>,
        "position" -> <|"line" -> lineContaining["seqAllHomogeneous ="], "character" -> 0|>|>
    |>
  ],
  {
    <|"jsonrpc" -> "2.0", "id" -> 19,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `___Integer`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-SequenceAllHomogeneous"
]

(* ___[All] mixed call arguments -> exact PatternSequence of inferred heads *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 20,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>,
        "position" -> <|"line" -> lineContaining["seqAllMixed ="], "character" -> 0|>|>
    |>
  ],
  {
    <|"jsonrpc" -> "2.0", "id" -> 20,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `PatternSequence[_Integer, _Integer, _Real, _Complex]`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-SequenceAllMixed"
]

(* ___[All] empty call arguments -> ___ *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 21,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>,
        "position" -> <|"line" -> lineContaining["seqAllEmpty ="], "character" -> 0|>|>
    |>
  ],
  {
    <|"jsonrpc" -> "2.0", "id" -> 21,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `___`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-SequenceAllEmpty"
]

(* __[All] homogeneous call arguments -> __Integer *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 22,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>,
        "position" -> <|"line" -> lineContaining["seqAllRequiredHomogeneous ="], "character" -> 0|>|>
    |>
  ],
  {
    <|"jsonrpc" -> "2.0", "id" -> 22,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `__Integer`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-SequenceRequiredAllHomogeneous"
]

(* ___[1, All] homogeneous list elements -> ___Integer *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 23,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>,
        "position" -> <|"line" -> lineContaining["seqFromListHomogeneous ="], "character" -> 0|>|>
    |>
  ],
  {
    <|"jsonrpc" -> "2.0", "id" -> 23,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `___Integer`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-SequenceNestedListHomogeneous"
]

(* Apply[applyTarget, {1, 2}] replaces List with applyTarget -> _String *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 24,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>,
        "position" -> <|"line" -> lineContaining["applyHeadFromList ="], "character" -> 0|>|>
    |>
  ],
  {
    <|"jsonrpc" -> "2.0", "id" -> 24,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_String`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-ApplyHeadFromList"
]

(* Apply[applyTarget, wrapper[1, 2]] uses wrapper's arguments as applyTarget arguments. *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 25,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>,
        "position" -> <|"line" -> lineContaining["applyHeadFromExpression ="], "character" -> 0|>|>
    |>
  ],
  {
    <|"jsonrpc" -> "2.0", "id" -> 25,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `_String`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-ApplyHeadFromExpression"
]

(* Apply[List, wrapper[1, 2, 3]] reconstructs a list from wrapper's arguments. *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 26,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>,
        "position" -> <|"line" -> lineContaining["applyListHeadFromExpression ="], "character" -> 0|>|>
    |>
  ],
  {
    <|"jsonrpc" -> "2.0", "id" -> 26,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `{___Integer}`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-ApplyListHeadFromExpression"
]

(* Apply[seqAll, {...}] composes with ___[All] sequence-return inference. *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 27,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>,
        "position" -> <|"line" -> lineContaining["applySequenceHeadFromList ="], "character" -> 0|>|>
    |>
  ],
  {
    <|"jsonrpc" -> "2.0", "id" -> 27,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "**Inferred Pattern:** `PatternSequence[_Integer, _Integer, _Real, _Complex]`"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DocComment-ApplySequenceHeadFromList"
]
