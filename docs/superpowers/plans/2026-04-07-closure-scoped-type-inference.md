# Closure-Scoped Type Inference Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix two bugs: (1) Module/Block/With local variables bleed across closure boundaries in `localVarMap` (Diagnostics.wl); (2) Hover only shows the var-list initializer type, ignoring body re-assignments (Hover.wl Case F).

**Architecture:** Add a 5th `closureRange` field to all `localVarMap` entries and filter by it in `inferArgSampleValue`. Extend Hover.wl Case F to scan the closure body for `Set` nodes and pick the last assignment at or before the cursor.

**Tech Stack:** Wolfram Language, MUnit test framework, LSPServer paclet

---

## File Map

| File | Changes |
|---|---|
| `Tests/hover/ClosureScopeTest.wl` | **New**: WL source fixture for hover + diag tests |
| `Tests/hover/ClosureScope.wlt` | **New**: 4 hover tests |
| `Tests/hover/ClosureScopeDiag.wlt` | **New**: 2 diagnostic tests |
| `LSPServer/Kernel/Hover.wl` | Modify Case F (~line 1050): replace single-value throw with flow-sensitive body scan |
| `LSPServer/Kernel/Diagnostics.wl` | Add pre-scan + 5th field to all entry types + filter update in `inferArgSampleValue` |
| `build/paclet/LSPServer/Kernel/Hover.wl` | Synced copy (never edit directly) |
| `build/paclet/LSPServer/Kernel/Diagnostics.wl` | Synced copy (never edit directly) |
| Installed paclet (see Task 2 Step 5 for path) | Synced copy (never edit directly) |

---

## Background

Tests load from `build/paclet/` via `PacletDirectoryLoad` in `Tests/hover/Init.wl`.
Always sync source → build/ → installed after editing.

Test runner (run from repo root):
```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/hover/Name.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```

WL sync commands (find installed path first):
```bash
INST=$(ls -d ~/.Wolfram/Paclets/Repository/LSPServer--*/Kernel | head -1)
echo $INST
```

---

## Task 1: Create test source file and write failing hover tests

**Files:**
- Create: `Tests/hover/ClosureScopeTest.wl`
- Create: `Tests/hover/ClosureScope.wlt`

### Step 1: Write the test source file

Create `Tests/hover/ClosureScopeTest.wl` with exact content below.
Line numbers are critical — all test line references depend on them.

```wolfram
(* Test fixture for closure-scope type inference.
   Line numbers are 1-based; LSP positions are 0-based (line - 1). *)

(*  Case A: flow-sensitive hover after body reassignment.
    x = 5 in var-list (Integer), then x = "hello" in body.
    Hovering on x after the reassignment should give _String. *)
Module[{x = 5},
  x = "hello";
  x
]

(*  Case B: flow-sensitive hover before body reassignment.
    x = 5 in var-list (Integer), x = "hello" appears later.
    Hovering on x; (before reassignment) should give _Integer. *)
Module[{x = 5},
  x;
  x = "hello"
]

(*  Case C: uninitialized local variable.
    Hovering on x inside Module[{x}, ...] should give _ (Blank[]). *)
Module[{x},
  x
]

(*  Case D: scope bleed check.
    x inside the module is Integer. x at file scope should have no type.
    needsStr expects _String; called with file-scope x should NOT warn;
    called with module-local Integer x it SHOULD warn. *)
needsStr[x_String] := x

Module[{x = 5},
  x
];

x

needsStr[x]

Module[{x = 5},
  needsStr[x]
]
```

Verify final line numbers (count carefully — every blank line counts):
- Line 7: `Module[{x = 5},`  (Case A open)
- Line 8: `  x = "hello";`
- Line 9: `  x`               ← hover target A (after reassign)
- Line 10: `]`
- Line 15: `Module[{x = 5},`  (Case B open)
- Line 16: `  x;`             ← hover target B (before reassign)
- Line 17: `  x = "hello"`
- Line 18: `]`
- Line 22: `Module[{x},`      (Case C open)
- Line 23: `  x`              ← hover target C (uninit)
- Line 24: `]`
- Line 30: `needsStr[x_String] := x`   (function def for diag)
- Line 32: `Module[{x = 5},`  (Case D inner module)
- Line 33: `  x`              ← hover target D (module-local Integer)
- Line 34: `];`
- Line 36: `x`                ← file-scope x (no closure type)
- Line 38: `needsStr[x]`      ← diag: should NOT warn
- Line 40: `Module[{x = 5},`
- Line 41: `  needsStr[x]`    ← diag: SHOULD warn (Integer vs _String)
- Line 42: `]`

- [ ] **Step 2: Write the failing hover tests**

Create `Tests/hover/ClosureScope.wlt`:

```wolfram
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
```

- [ ] **Step 3: Run tests and confirm they FAIL**

```bash
cd /home/tonya/Working/forked-projects/forked-project-WOLFRAM-LSP && wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/hover/ClosureScope.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```

Expected: at least Tests 1 and 2 FAIL (flow sensitivity is the bug). Tests 3 and 4 may already pass.

- [ ] **Step 4: Commit the failing tests**

```bash
git add Tests/hover/ClosureScopeTest.wl Tests/hover/ClosureScope.wlt
git commit -m "test: add failing closure-scope hover tests (TDD)"
```

---

## Task 2: Implement Hover.wl Case F body scan

**Files:**
- Modify: `LSPServer/Kernel/Hover.wl:1050` (single line replacement)
- Sync: `build/paclet/LSPServer/Kernel/Hover.wl`
- Sync: installed paclet

- [ ] **Step 1: Replace line 1050 in Hover.wl**

Find the exact text to replace (line 1050 in `LSPServer/Kernel/Hover.wl`):
```wolfram
          If[varRhsPattern =!= None && varRhsPattern =!= Null, Throw[varRhsPattern]]
```

Replace with:

```wolfram
          (* Flow-sensitive: scan body for re-assignments, pick last before cursor.
             Skip entirely if tokenSymbol not found in var-list (varRhsPattern === Null). *)
          If[varRhsPattern =!= Null,
            Module[{initPat, allPairs, validPairs},
              initPat = If[varRhsPattern === None, None, varRhsPattern];
              allPairs = If[initPat =!= None, {{outerSrc[[1, 1]], initPat}}, {}];
              allPairs = Join[allPairs,
                Cases[Rest[scopeArgs],
                  CallNode[LeafNode[Symbol, "Set", _],
                    {LeafNode[Symbol, tokenSymbol, _], rhs_},
                    KeyValuePattern[Source -> {{setLine_, _}, _}]] :>
                    Module[{p = Which[
                      argNodePattern[rhs] =!= None,
                        argNodePattern[rhs],
                      MatchQ[rhs, CallNode[LeafNode[Symbol, "List", _], _List, _]],
                        With[{ep = inferListElementPattern[rhs]},
                          If[ep =!= None, ep, Blank[List]]],
                      MatchQ[rhs, CallNode[LeafNode[Symbol, "Association", _], _, _]],
                        Blank[Association],
                      True, None
                    ]},
                    If[p =!= None, {setLine, p}, Nothing]
                    ],
                  Infinity]
              ];
              validPairs = Select[allPairs, #[[1]] <= cursorLine &];
              If[Length[validPairs] > 0,
                Throw[Last[SortBy[validPairs, First]][[2]]]
              ]
            ]
          ]
```

`scopeArgs` is already defined in the enclosing `Module` (line 1017: `scopeArgs = mbwNode[[2]]`).
`Rest[scopeArgs]` skips the var-list (index 1) and gives the body args.
`outerSrc` is defined at line 1023.

**Known limitation (non-goal per spec):** `Cases[..., Infinity]` will find `Set[x, rhs]` nodes inside nested Module/Block/With var-lists if those inner closures also declare `x`. This means hovering on the outer `x` at a cursor position after the inner closure would show the inner var-list's type instead of the outer init type. This edge case is explicitly excluded from this spec. Do not attempt to fix it here.

- [ ] **Step 2: Sync to build/ and installed paclet**

```bash
# Find installed paclet path
INST=$(ls -d ~/.Wolfram/Paclets/Repository/LSPServer--*/Kernel | head -1)
echo "Installing to: $INST"

cp LSPServer/Kernel/Hover.wl build/paclet/LSPServer/Kernel/Hover.wl
cp LSPServer/Kernel/Hover.wl "$INST/Hover.wl"
```

- [ ] **Step 3: Run hover tests and confirm they PASS**

```bash
cd /home/tonya/Working/forked-projects/forked-project-WOLFRAM-LSP && wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/hover/ClosureScope.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```

Expected: Passed: 4, Failed: 0

- [ ] **Step 4: Commit**

```bash
git add LSPServer/Kernel/Hover.wl build/paclet/LSPServer/Kernel/Hover.wl
git commit -m "feat: flow-sensitive type inference for Module/Block/With locals in Hover Case F"
```

---

## Task 3: Write failing diagnostic tests

**Files:**
- Create: `Tests/hover/ClosureScopeDiag.wlt`

- [ ] **Step 1: Write the failing diagnostic tests**

Create `Tests/hover/ClosureScopeDiag.wlt`:

```wolfram
Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "ClosureScopeTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "ClosureScopeTest.wl"}]];
LSPServer`PacletIndex`UpdateFileIndex[uri, ReadString[FileNameJoin[{DirectoryName[$TestFileName], "ClosureScopeTest.wl"}]]];

LSPServer`handleContent[
  <|"method" -> "textDocument/runWorkspaceDiagnostics",
    "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
  |>
];


(* ── Test 1: needsStr[x] at file scope (line 38, ≡ LSP line 37) should NOT warn ──
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


(* ── Test 2: needsStr[x] inside Module[{x=5},...] (line 41, ≡ LSP line 40) SHOULD warn ──
   x is Integer inside the module. needsStr expects _String → mismatch.
   "  needsStr[x]" → 'x' is at character 11 (0-based).
   This verifies the fix didn't suppress legitimate warnings inside closures. *)
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
      "message" -> "Argument 1 of \"needsStr\" does not match any declared input pattern. Expected _String, got an Integer.",
      "severity" -> 2,
      "range" -> <|
        "start" -> <|"line" -> 40, "character" -> 11|>,
        "end"   -> <|"line" -> 40, "character" -> 12|>
      |>,
      "source" -> "wolfram lint"
    |>
  },
  TestID -> "IDE-Test-ClosureScope-WarnInsideClosure"
]
```

- [ ] **Step 2: Run tests and confirm expected failure**

```bash
cd /home/tonya/Working/forked-projects/forked-project-WOLFRAM-LSP && wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/hover/ClosureScopeDiag.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```

Expected: Test 1 FAILS (gets a spurious mismatch warning due to scope bleed). Test 2 may pass or fail.

If the actual message text for Test 2 doesn't match exactly, update the `"message"` string to match the actual output before moving on. Do NOT change the `TestID` line. Commit the adjustment if needed.

- [ ] **Step 3: Commit the failing tests**

```bash
git add Tests/hover/ClosureScopeDiag.wlt
git commit -m "test: add failing closure-scope diagnostic tests (TDD)"
```

---

## Task 4: Implement Diagnostics.wl closureRange changes

**Files:**
- Modify: `LSPServer/Kernel/Diagnostics.wl` (6 separate edits)
- Sync: `build/paclet/LSPServer/Kernel/Diagnostics.wl`
- Sync: installed paclet

### Step 1: Add closureVarRanges pre-scan before rawEntries

Find this text in `LSPServer/Kernel/Diagnostics.wl` (around line 1117):
```wolfram
        localVarMap =
        Module[{rawEntries, convergenceEntries, condEntries, paramEntries,
                iterEntries, mapParamEntries, allEntries},
          rawEntries = Cases[ast,
```

Replace with:

```wolfram
        localVarMap =
        Module[{rawEntries, convergenceEntries, condEntries, paramEntries,
                iterEntries, mapParamEntries, allEntries,
                closureVarRanges, findEnclosingClosureForVar},
          (* Pre-scan: build closureVarRanges — maps each declared var name to the
             list of source ranges of closures (Module/Block/With) that declare it.
             Used to scope rawEntries and convergenceEntries to their closure. *)
          closureVarRanges = Association[];
          Scan[
            Function[{mbwNode},
              Catch[
                Module[{varListNode, closureSrc, varList},
                  If[Length[mbwNode[[2]]] < 1, Throw[Null, "next"]];
                  varListNode = mbwNode[[2, 1]];
                  If[!MatchQ[varListNode, CallNode[LeafNode[Symbol, "List", _], _List, _]],
                    Throw[Null, "next"]
                  ];
                  closureSrc = Quiet[mbwNode[[3, Key[Source]]]];
                  If[!MatchQ[closureSrc, {{_Integer, _Integer}, {_Integer, _Integer}}],
                    Throw[Null, "next"]
                  ];
                  varList = varListNode[[2]];
                  Scan[Function[vn,
                    Catch[
                      Module[{vName},
                        Which[
                          MatchQ[vn, LeafNode[Symbol, _, _]],
                            vName = vn[[2]],
                          MatchQ[vn, CallNode[LeafNode[Symbol, "Set", _],
                                              {LeafNode[Symbol, _, _], _}, _]],
                            vName = vn[[2, 1, 2]],
                          True, Throw[Null, "next"]
                        ];
                        closureVarRanges[vName] = Append[
                          Lookup[closureVarRanges, vName, {}], closureSrc
                        ]
                      ]
                    , "next"]
                  ], varList]
                ]
              , "next"]
            ],
            Cases[ast, CallNode[LeafNode[Symbol, "Module" | "Block" | "With", _], _List, _],
                  Infinity]
          ];
          (* Returns the innermost closure range that declares varName and contains line.
             Returns None if line is not inside any closure that declares varName. *)
          findEnclosingClosureForVar = Function[{varName, line},
            Module[{ranges, candidates},
              ranges = Lookup[closureVarRanges, varName, {}];
              candidates = Select[ranges,
                Function[r, r[[1, 1]] <= line <= r[[2, 1]]]
              ];
              If[Length[candidates] === 0, None,
                Last[SortBy[candidates, #[[1, 1]] &]]
              ]
            ]
          ];
          rawEntries = Cases[ast,
```

- [ ] **Step 2: Add 5th field to rawEntries**

Find (around line 1136, inside the rawEntries Cases block):
```wolfram
                {varName, assignLine, sampleVal, assignBranch}
```

Replace with:
```wolfram
                {varName, assignLine, sampleVal, assignBranch,
                 findEnclosingClosureForVar[varName, assignLine]}
```

- [ ] **Step 3: Add 5th field to convergenceEntries**

Find (around line 1181, inside convergenceEntries):
```wolfram
                              {v, afterLine, branchVals[[1]], None},
```

Replace with:
```wolfram
                              {v, afterLine, branchVals[[1]], None,
                               findEnclosingClosureForVar[v, afterLine]},
```

- [ ] **Step 4: Add None as 5th field to condEntries, paramEntries, iterEntries, mapParamEntries**

Find (around line 1373):
```wolfram
          allEntries = Join[condEntries, rawEntries, convergenceEntries,
                            paramEntries, iterEntries, mapParamEntries];
```

Replace with:
```wolfram
          (* Append closureRange = None to entry types that are already scope-guarded
             by branchRange or defSrc (condEntries, paramEntries, iterEntries, mapParamEntries). *)
          condEntries      = Map[Append[#, None] &, condEntries];
          paramEntries     = Map[Append[#, None] &, paramEntries];
          iterEntries      = Map[Append[#, None] &, iterEntries];
          mapParamEntries  = Map[Append[#, None] &, mapParamEntries];
          allEntries = Join[condEntries, rawEntries, convergenceEntries,
                            paramEntries, iterEntries, mapParamEntries];
```

- [ ] **Step 5: Update GroupBy to store 4 fields (add closureRange as e[[5]])**

Find (around line 1375):
```wolfram
          GroupBy[
            allEntries,
            First,
            Function[entries, SortBy[Map[Function[e, {e[[2]], e[[3]], e[[4]]}], entries], First]]
          ]
```

Replace with:
```wolfram
          GroupBy[
            allEntries,
            First,
            Function[entries, SortBy[Map[Function[e, {e[[2]], e[[3]], e[[4]], e[[5]]}], entries], First]]
          ]
```

- [ ] **Step 6: Update inferArgSampleValue to filter by closureRange**

Find (around line 1477, inside `inferArgSampleValue`):
```wolfram
              valid = Select[entries, Function[e,
                e[[1]] <= argLine &&
                (e[[3]] === None || (
                  MatchQ[e[[3]], {{_Integer, _Integer}, {_Integer, _Integer}}] &&
                  e[[3, 1, 1]] <= argLine <= e[[3, 2, 1]]
                ))
              ]];
```

Replace with:
```wolfram
              valid = Select[entries, Function[e,
                e[[1]] <= argLine &&
                (e[[3]] === None || (
                  MatchQ[e[[3]], {{_Integer, _Integer}, {_Integer, _Integer}}] &&
                  e[[3, 1, 1]] <= argLine <= e[[3, 2, 1]]
                )) &&
                (e[[4]] === None || (
                  MatchQ[e[[4]], {{_Integer, _Integer}, {_Integer, _Integer}}] &&
                  e[[4, 1, 1]] <= argLine <= e[[4, 2, 1]]
                ))
              ]];
```

Note: after adding the 5th source field in Step 1-4, the stored tuples inside `localVarMap` (after GroupBy) are `{line, sample, branchRange, closureRange}` (4 elements), so:
- `e[[1]]` = line
- `e[[2]]` = sample
- `e[[3]]` = branchRange
- `e[[4]]` = closureRange ← new check

- [ ] **Step 7: Sync to build/ and installed paclet**

```bash
INST=$(ls -d ~/.Wolfram/Paclets/Repository/LSPServer--*/Kernel | head -1)
cp LSPServer/Kernel/Diagnostics.wl build/paclet/LSPServer/Kernel/Diagnostics.wl
cp LSPServer/Kernel/Diagnostics.wl "$INST/Diagnostics.wl"
```

- [ ] **Step 8: Run diagnostic tests and confirm they PASS**

```bash
cd /home/tonya/Working/forked-projects/forked-project-WOLFRAM-LSP && wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/hover/ClosureScopeDiag.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```

Expected: Passed: 2, Failed: 0

- [ ] **Step 9: Commit**

```bash
git add LSPServer/Kernel/Diagnostics.wl build/paclet/LSPServer/Kernel/Diagnostics.wl
git commit -m "feat: scope-limit localVarMap entries to declaring closure via closureRange 5th field"
```

---

## Task 5: Regression check

**Files:** None changed — read-only verification.

- [ ] **Step 1: Run existing DocComment hover tests**

```bash
cd /home/tonya/Working/forked-projects/forked-project-WOLFRAM-LSP && wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/hover/DocComment.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```

Expected: Passed: 19, Failed: 0 (all pre-existing tests still pass)

- [ ] **Step 2: Run existing BranchNarrowing tests**

```bash
cd /home/tonya/Working/forked-projects/forked-project-WOLFRAM-LSP && wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/hover/BranchNarrowing.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```

Expected: all pass (branch narrowing uses branchRange check which is unchanged)

- [ ] **Step 3: Run DocCommentDiag (pre-existing 5/10 failures are acceptable)**

```bash
cd /home/tonya/Working/forked-projects/forked-project-WOLFRAM-LSP && wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/hover/DocCommentDiag.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```

Expected: same pass/fail counts as before (5/10 pre-existing failures are unrelated to this change). If the count changes, investigate before proceeding.

- [ ] **Step 4: Commit (if any sync files were left untracked)**

```bash
git status
# Only commit if there are unstaged sync changes
git add -p  # review each hunk
git commit -m "chore: sync build/ copies after closure-scope implementation"
```
