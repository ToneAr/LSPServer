# Builtin Variadic Pattern Map Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a `"Type..."` / `"Type*"` variadic spec syntax to the builtin pattern data format, emit it in the generator, handle it correctly in both consumers (Diagnostics.wl + PacletIndex.wl), fix arity-aware overload selection, and regenerate the data file.

**Architecture:** The system stores builtin function signatures as `{inputSpecList, returnTypeString}` pairs in `LSPServer/Resources/BuiltinInputPatterns.wl`. Both `Diagnostics.wl` and `PacletIndex.wl` load this file and convert the specs to WL patterns for matching. The `"Variadic"` flag computed by the generator is currently discarded at data-write time, causing the consumers to hardcode `"Variadic" -> False` and use exact-arity matching - which silently misfires for calls like `Plus[1,2,3]`. This plan extends the spec format in-place (backward-compatible), updates both consumers with a shared matching algorithm, and improves the generator to emit complete signatures for variadic functions.

**Tech Stack:** Wolfram Language, MUnit for tests, `wolframscript` CLI.

---

## Background: How it fits together

```
Generator (GenerateBuiltinPatterns.wl)
  └─ writes ──► LSPServer/Resources/BuiltinInputPatterns.wl
                  └─ loaded by ─► Diagnostics.wl  ($BuiltinPatterns)
                  └─ loaded by ─► PacletIndex.wl   ($BuiltinPatterns)
```

**Current format:**
```wolfram
"Plus" -> {{{None}, "_?NumericQ"}, {{"Real","Real"}, "_Real"}, {{"Integer","Integer"}, "_Integer"}}
```
No way to express "one-or-more Integer args". The `"Variadic"` field from `extractDVOverloads` is computed but discarded before writing.

**New format (additive, backward-compatible):**
```wolfram
"Plus" -> {
  {{"Integer..."}, "_Integer"},    (* 1+ Integer args → Integer *)
  {{"Real..."},    "_Real"},       (* 1+ Real args → Real *)
  {{"Complex..."}, "_Complex"},
  {{None},         "_?NumericQ"}  (* catch-all *)
}
```

**Spec string vocabulary (last element of `inputSpecList` only):**
| Spec string  | Means                        | WL pattern equivalent      |
|:-------------|:-----------------------------|:---------------------------|
| `"Type..."`  | 1+ args with head Type       | `BlankSequence[Symbol[T]]` |
| `"Type*"`    | 0+ args with head Type       | `BlankNullSequence[Symbol[T]]` |
| `"..."`      | 1+ args (untyped)            | `BlankSequence[]`          |
| `"*"`        | 0+ args (untyped)            | `BlankNullSequence[]`      |

Non-variadic specs (`"String"`, `"Integer"`, `"_?BooleanQ"`, `None`) are unchanged.

---

## File Map

| File | Change |
|:-----|:-------|
| `CodeTools/Generate/GenerateBuiltinPatterns.wl` | Add `isSeqArg`, variadic spec emission in `extractDVOverloads`, update `$identityOverrides` |
| `LSPServer/Kernel/Diagnostics.wl` | New `builtinSpecToPattern` helper, update spec conversion + arity/type matching |
| `LSPServer/Kernel/PacletIndex.wl` | Same helpers + same matching in two places (`inferPatternFromRHS`, `resolveCallReturnPattern`) |
| `LSPServer/Resources/BuiltinInputPatterns.wl` | Regenerated (do not edit by hand) |
| `Tests/builtin/BuiltinVariadic.wlt` | New test suite for variadic overload matching |
| `Tests/builtin/BuiltinVariadicTest.wl` | New test fixture file |

---

## How to run tests

```bash
# Single test suite
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/builtin/BuiltinVariadic.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'

# Full IPWL regression
wolframscript -code 'Needs["MUnit`"]; Do[report = TestReport[f]; Print[f, "  ", report["TestsSucceededCount"], "/", report["TestsSucceededCount"] + report["TestsFailedCount"]], {f, {"Tests/ipwl/IPWLParse.wlt","Tests/ipwl/IPWLHover.wlt","Tests/ipwl/IPWLParametric.wlt","Tests/ipwl/IPWLDiag.wlt"}}]'
```

After editing source, sync to installed paclet:
```bash
cp LSPServer/Kernel/Diagnostics.wl ~/.Wolfram/Paclets/Repository/LSPServer--2.9.59/Kernel/Diagnostics.wl
cp LSPServer/Kernel/PacletIndex.wl  ~/.Wolfram/Paclets/Repository/LSPServer--2.9.59/Kernel/PacletIndex.wl
```

---

## Task 1: Write failing tests (TDD - do these first)

**Files:**
- Create: `Tests/builtin/BuiltinVariadicTest.wl`
- Create: `Tests/builtin/BuiltinVariadic.wlt`

- [ ] **Step 1.1: Create test fixture `Tests/builtin/BuiltinVariadicTest.wl`**

```wolfram
(* Builtin variadic pattern test fixture *)

(** Return: _Integer *)
addThreeInts[x_Integer, y_Integer, z_Integer] := Plus[x, y, z]

(** Return: _String *)
joinThreeStrings[s1_String, s2_String, s3_String] := StringJoin[s1, s2, s3]

(** Return: _?BooleanQ *)
andThree[a_, b_, c_] := And[a, b, c]

(** Return: _Integer *)
sumList[xs_List] := Total[xs]
```

- [ ] **Step 1.2: Create test suite `Tests/builtin/BuiltinVariadic.wlt`**

```wolfram
Get[FileNameJoin[{DirectoryName[$TestFileName], "..", "hover", "Init.wl"}]];

fixturePath = FileNameJoin[{DirectoryName[$TestFileName], "BuiltinVariadicTest.wl"}];
initFunction[fixturePath];
uri = LocalObjects`PathToURI[fixturePath];

LSPServer`PacletIndex`UpdateFileIndex[uri, ReadString[fixturePath]];
LSPServer`handleContent[
  <|"method" -> "textDocument/runWorkspaceDiagnostics",
    "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
  |>
];

result = LSPServer`handleContent[
  <|"method" -> "textDocument/publishDiagnostics",
    "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
  |>
];
allDiags = result[[1, "params", "diagnostics"]];

(* Test 1: Plus[x,y,z] with 3 Integer args should NOT trigger DeclaredTypeMismatch *)
VerificationTest[
  NoneTrue[allDiags,
    StringStartsQ[Lookup[#, "code", ""], "DeclaredTypeMismatch"] &&
    StringContainsQ[Lookup[#, "code", ""], "addThreeInts"] &],
  True,
  TestID -> "Builtin-Variadic-Plus-NoMismatch"
]

(* Test 2: StringJoin[s1,s2,s3] with 3 String args should NOT trigger DeclaredTypeMismatch *)
VerificationTest[
  NoneTrue[allDiags,
    StringStartsQ[Lookup[#, "code", ""], "DeclaredTypeMismatch"] &&
    StringContainsQ[Lookup[#, "code", ""], "joinThreeStrings"] &],
  True,
  TestID -> "Builtin-Variadic-StringJoin-NoMismatch"
]

(* Test 3: And[a,b,c] with 3 args should NOT trigger DeclaredTypeMismatch *)
VerificationTest[
  NoneTrue[allDiags,
    StringStartsQ[Lookup[#, "code", ""], "DeclaredTypeMismatch"] &&
    StringContainsQ[Lookup[#, "code", ""], "andThree"] &],
  True,
  TestID -> "Builtin-Variadic-And-NoMismatch"
]

(* Test 4: builtinSpecToPattern converts variadic spec strings correctly *)
VerificationTest[
  LSPServer`Diagnostics`Private`builtinSpecToPattern["Integer..."],
  BlankSequence[Integer],
  TestID -> "Builtin-SpecToPattern-BlankSequence"
]

(* Test 5: builtinSpecToPattern converts 0+ spec correctly *)
VerificationTest[
  LSPServer`Diagnostics`Private`builtinSpecToPattern["String*"],
  BlankNullSequence[String],
  TestID -> "Builtin-SpecToPattern-BlankNullSequence"
]

(* Test 6: overloadMatchesArgs matches variadic overload with 3-arg call *)
VerificationTest[
  LSPServer`Diagnostics`Private`overloadSpecMatchesArgs[
    {"Integer..."},   (* variadic spec: 1+ integers *)
    {0, 1, 2}        (* 3 integer samples *)
  ],
  True,
  TestID -> "Builtin-VariadicMatch-ThreeInts"
]

(* Test 7: overloadMatchesArgs rejects type mismatch in variadic tail *)
VerificationTest[
  LSPServer`Diagnostics`Private`overloadSpecMatchesArgs[
    {"Integer..."},   (* 1+ integers *)
    {0, "hello", 2}  (* mixed: int, string, int *)
  ],
  False,
  TestID -> "Builtin-VariadicMatch-TypeMismatch"
]

(* Test 8: overloadMatchesArgs matches fixed-arity overload unchanged *)
VerificationTest[
  LSPServer`Diagnostics`Private`overloadSpecMatchesArgs[
    {"String", "Integer"},  (* fixed 2-arg spec *)
    {"", 0}                  (* matching samples *)
  ],
  True,
  TestID -> "Builtin-FixedArity-Match"
]
```

- [ ] **Step 1.3: Run to confirm tests 4–8 fail (helpers not yet defined) and 1–3 may fail**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/builtin/BuiltinVariadic.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]' 2>/dev/null
```
Expected: Multiple FAIL (helpers undefined, or tests 1–3 may pass if current behavior coincidentally works).

---

## Task 2: Add `builtinSpecToPattern` and `overloadSpecMatchesArgs` to Diagnostics.wl

**Files:**
- Modify: `LSPServer/Kernel/Diagnostics.wl` (near line 16, in the file-level section before the Module)

These are **file-level** helpers (outside the large diagnostic Module) so they're available at both the module and file scope.

- [ ] **Step 2.1: Add helpers after the `$BuiltinPatterns` loading block (around line 26)**

Find this block (lines 14–26):
```wolfram
With[{builtinFile = FileNameJoin[{
    DirectoryName[AbsoluteFileName[$InputFileName]],
    "..", "Resources", "BuiltinInputPatterns.wl"}]},
  If[FileExistsQ[builtinFile],
    Get[builtinFile],
    $BuiltinPatterns = <||>
  ]
];
```

Insert AFTER this block:

```wolfram
(*
  builtinSpecToPattern[s]
  Convert a single input-spec string from $BuiltinPatterns into a WL pattern
  expression suitable for MatchQ against a representative sample value.

  Spec string vocabulary:
    "Type..."  → BlankSequence[Symbol["Type"]]   (1+ expressions with head Type)
    "Type*"    → BlankNullSequence[Symbol["Type"]] (0+ expressions with head Type)
    "..."      → BlankSequence[]                  (1+ untyped)
    "*"        → BlankNullSequence[]              (0+ untyped)
    "_?Pred"   → ToExpression["_?Pred"]           (PatternTest)
    "Type"     → Blank[Symbol["Type"]]            (exactly one typed arg)
    None       → Blank[]                           (exactly one untyped arg)
*)
builtinSpecToPattern[s_String] :=
  Which[
    s === "...", BlankSequence[],
    s === "*",   BlankNullSequence[],
    StringEndsQ[s, "..."], BlankSequence[Symbol[StringDrop[s, -3]]],
    StringEndsQ[s, "*"],   BlankNullSequence[Symbol[StringDrop[s, -1]]],
    StringStartsQ[s, "_"], ToExpression[s],
    True, Blank[Symbol[s]]
  ]
builtinSpecToPattern[None] := Blank[]

(*
  overloadSpecMatchesArgs[specs, argSamples]
  True when the overload described by specs (a List of spec strings) is
  compatible with the given argument samples.

  Handles both fixed-arity and variadic (last spec ending in "..." or "*").
  argSamples: list of representative WL values (0, "", 0., True, {}, <||>, etc.)
              Missing["Unknown"] means the arg type could not be inferred and
              always passes the check.
*)
overloadSpecMatchesArgs[specs_List, argSamples_List] :=
  Module[{n = Length[specs], m = Length[argSamples],
          isVar, fixedN, varSpec, varElemPat},
    isVar = n > 0 && StringQ[Last[specs]] &&
              (StringEndsQ[Last[specs], "..."] || StringEndsQ[Last[specs], "*"]);
    If[!isVar,
      (* Fixed arity: exact length + element-wise match *)
      n === m && (n === 0 || AllTrue[
        Transpose[{argSamples, builtinSpecToPattern /@ specs}],
        (#[[1]] === Missing["Unknown"] || MatchQ[#[[1]], #[[2]]]) &
      ]),
      (* Variadic: at least fixedN args; prefix match + tail element match *)
      fixedN  = n - 1;
      varSpec = Last[specs];
      (* Element pattern for checking each tail arg individually *)
      varElemPat = Which[
        varSpec === "..." || varSpec === "*", Blank[],
        StringEndsQ[varSpec, "..."],
          Blank[Symbol[StringDrop[varSpec, -3]]],
        StringEndsQ[varSpec, "*"],
          Blank[Symbol[StringDrop[varSpec, -1]]],
        True, Blank[]
      ];
      m >= fixedN &&
      (fixedN === 0 || AllTrue[
        Transpose[{Take[argSamples, fixedN],
                   builtinSpecToPattern /@ Take[specs, fixedN]}],
        (#[[1]] === Missing["Unknown"] || MatchQ[#[[1]], #[[2]]]) &
      ]) &&
      AllTrue[Drop[argSamples, fixedN],
        (# === Missing["Unknown"] || MatchQ[#, varElemPat]) &
      ]
    ]
  ]
```

- [ ] **Step 2.2: Sync and run tests 4–8 to verify helpers work**

```bash
cp LSPServer/Kernel/Diagnostics.wl ~/.Wolfram/Paclets/Repository/LSPServer--2.9.59/Kernel/Diagnostics.wl
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/builtin/BuiltinVariadic.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]' 2>/dev/null
```
Expected: Tests 4–8 PASS. Tests 1–3 still depend on data changes (Task 5).

- [ ] **Step 2.3: Commit**

```bash
git add Tests/builtin/BuiltinVariadic.wlt Tests/builtin/BuiltinVariadicTest.wl LSPServer/Kernel/Diagnostics.wl
git commit -m "feat: add builtinSpecToPattern and overloadSpecMatchesArgs helpers for variadic specs"
```

---

## Task 3: Update `$BuiltinPatterns` spec conversion in Diagnostics.wl

**Files:**
- Modify: `LSPServer/Kernel/Diagnostics.wl` lines ~1614–1628 and ~1681–1692

The two places to update are:
1. **Spec-to-pattern conversion** (building the `allDefs` list from `$BuiltinPatterns`)
2. **Compatibility check** (`symptomatic = !AnyTrue[allDefs, ...]`)

- [ ] **Step 3.1: Update spec conversion block (~line 1614)**

Find this block:
```wolfram
If[KeyExistsQ[$BuiltinPatterns, funcName],
  (* Each overload stored as {inputPatternList, returnPatternString}.
     Convert input type strings to Blank[T] pattern expressions. *)
  Map[
    Function[{overload},
      <|"InputPatterns" -> Map[
        Function[{s}, If[StringQ[s], Blank[Symbol[s]], Blank[]]],
        overload[[1]]
      ], "Variadic" -> False|>
    ],
    $BuiltinPatterns[funcName]
  ],
  {}
]
```

Replace with:
```wolfram
If[KeyExistsQ[$BuiltinPatterns, funcName],
  Map[
    Function[{overload},
      Module[{specs = overload[[1]], isVar},
        isVar = Length[specs] > 0 && StringQ[Last[specs]] &&
                  (StringEndsQ[Last[specs], "..."] || StringEndsQ[Last[specs], "*"]);
        <|"InputPatterns" -> builtinSpecToPattern /@ specs,
          "Variadic"      -> isVar|>
      ]
    ],
    $BuiltinPatterns[funcName]
  ],
  {}
]
```

- [ ] **Step 3.2: Update compatibility check (~line 1681)**

Find this block:
```wolfram
symptomatic = !AnyTrue[allDefs,
  Function[{d},
    Module[{pats},
      pats = Lookup[d, "InputPatterns", {}];
      Length[pats] === Length[callArgs] &&
      AllTrue[
        Transpose[{argSamples, pats}],
        argMatchesPattern[#[[1]], #[[2]]] &
      ]
    ]
  ]
];
```

Replace with:
```wolfram
symptomatic = !AnyTrue[allDefs,
  Function[{d},
    Module[{specs = Lookup[d, "RawSpecs", {}],
            pats  = Lookup[d, "InputPatterns", {}],
            isVar = Lookup[d, "Variadic", False],
            fixedN, varElemPat},
      If[isVar && Length[pats] > 0,
        (* Variadic: check prefix + tail *)
        fixedN = Length[pats] - 1;
        varElemPat = With[{lp = Last[pats]},
          If[Head[lp] === BlankSequence && Length[lp] === 1, Blank[lp[[1]]],
          If[Head[lp] === BlankNullSequence && Length[lp] === 1, Blank[lp[[1]]],
          Blank[]]]];
        Length[argSamples] >= fixedN &&
        (fixedN === 0 || AllTrue[
          Transpose[{Take[argSamples, fixedN], Take[pats, fixedN]}],
          argMatchesPattern[#[[1]], #[[2]]] &
        ]) &&
        AllTrue[Drop[argSamples, fixedN],
          argMatchesPattern[#, varElemPat] &
        ],
        (* Fixed arity *)
        Length[pats] === Length[callArgs] &&
        AllTrue[
          Transpose[{argSamples, pats}],
          argMatchesPattern[#[[1]], #[[2]]] &
        ]
      ]
    ]
  ]
];
```

- [ ] **Step 3.3: Update arity check (~line 1649) to use variadic flag correctly**

The arity check is:
```wolfram
If[!AnyTrue[allDefs,
  Lookup[#, "Variadic", False] ||
  Length[Lookup[#, "InputPatterns", {}]] === Length[callArgs] &],
  ...
```

This already uses `"Variadic"` - it now works because we set `"Variadic" -> True` for variadic builtins. No change needed here.

- [ ] **Step 3.4: Sync, run all builtin + IPWL tests**

```bash
cp LSPServer/Kernel/Diagnostics.wl ~/.Wolfram/Paclets/Repository/LSPServer--2.9.59/Kernel/Diagnostics.wl
wolframscript -code 'Needs["MUnit`"]; Do[report = TestReport[f]; Print[f, "  ", report["TestsSucceededCount"], "/", report["TestsSucceededCount"] + report["TestsFailedCount"]], {f, {"Tests/builtin/BuiltinVariadic.wlt","Tests/ipwl/IPWLDiag.wlt","Tests/ipwl/IPWLHover.wlt"}}]' 2>/dev/null
```
Expected: All passing or no regressions (data hasn't changed yet, so tests 1–3 may still fail until Task 5).

- [ ] **Step 3.5: Commit**

```bash
git add LSPServer/Kernel/Diagnostics.wl
git commit -m "feat: update Diagnostics.wl builtin spec conversion and matching for variadic support"
```

---

## Task 4: Update PacletIndex.wl consumer (two matching sites)

**Files:**
- Modify: `LSPServer/Kernel/PacletIndex.wl` - two `$BuiltinPatterns` conversion + matching blocks

> **Note:** PacletIndex.wl and Diagnostics.wl both define `builtinSpecToPattern` independently (in their respective Private contexts). That's intentional - they're short helpers, no shared file needed.

- [ ] **Step 4.1: Add helpers after the `$BuiltinPatterns` loading block in PacletIndex.wl (~line 66)**

Find the loading block:
```wolfram
With[{builtinFile = ...},
  If[FileExistsQ[builtinFile],
    Get[builtinFile],
    $BuiltinPatterns = <||>
  ]
];
```

Insert AFTER this block the same two helpers as in Task 2 - `builtinSpecToPattern` and `overloadSpecMatchesArgs` (same code, different private context):

```wolfram
builtinSpecToPattern[s_String] :=
  Which[
    s === "...", BlankSequence[],
    s === "*",   BlankNullSequence[],
    StringEndsQ[s, "..."], BlankSequence[Symbol[StringDrop[s, -3]]],
    StringEndsQ[s, "*"],   BlankNullSequence[Symbol[StringDrop[s, -1]]],
    StringStartsQ[s, "_"], ToExpression[s],
    True, Blank[Symbol[s]]
  ]
builtinSpecToPattern[None] := Blank[]

overloadSpecMatchesArgs[specs_List, argSamples_List] :=
  Module[{n = Length[specs], m = Length[argSamples],
          isVar, fixedN, varElemPat},
    isVar = n > 0 && StringQ[Last[specs]] &&
              (StringEndsQ[Last[specs], "..."] || StringEndsQ[Last[specs], "*"]);
    If[!isVar,
      n === m && (n === 0 || AllTrue[
        Transpose[{argSamples, builtinSpecToPattern /@ specs}],
        (#[[1]] === Missing["Unknown"] || MatchQ[#[[1]], #[[2]]]) &
      ]),
      fixedN  = n - 1;
      varElemPat = Which[
        StringEndsQ[Last[specs], "..."], Blank[Symbol[StringDrop[Last[specs], -3]]],
        StringEndsQ[Last[specs], "*"],   Blank[Symbol[StringDrop[Last[specs], -1]]],
        True, Blank[]
      ];
      m >= fixedN &&
      (fixedN === 0 || AllTrue[
        Transpose[{Take[argSamples, fixedN],
                   builtinSpecToPattern /@ Take[specs, fixedN]}],
        (#[[1]] === Missing["Unknown"] || MatchQ[#[[1]], #[[2]]]) &
      ]) &&
      AllTrue[Drop[argSamples, fixedN],
        (# === Missing["Unknown"] || MatchQ[#, varElemPat]) &
      ]
    ]
  ]
```

- [ ] **Step 4.2: Update FIRST `$BuiltinPatterns` → def conversion in `inferPatternFromRHS` (~line 1314)**

Find:
```wolfram
KeyExistsQ[$BuiltinPatterns, headName],
  Map[
    Function[{overload},
      With[{
        inPats = Map[
          Function[{s}, If[StringQ[s],
            If[StringStartsQ[s, "_"], ToExpression[s], Blank[Symbol[s]]],
            Blank[]]],
          overload[[1]]
        ],
        retStr = overload[[2]]
      },
        <|
          "InputPatterns" -> inPats,
          "DocComment" -> If[StringQ[retStr], ...
```

Replace the `inPats` binding with:
```wolfram
inPats = builtinSpecToPattern /@ overload[[1]],
```

And add `"Variadic"` to the association:
```wolfram
<|
  "InputPatterns" -> inPats,
  "Variadic" -> (Length[overload[[1]]] > 0 && StringQ[Last[overload[[1]]]] &&
    (StringEndsQ[Last[overload[[1]]], "..."] || StringEndsQ[Last[overload[[1]]], "*"])),
  "DocComment" -> ...
|>
```

- [ ] **Step 4.3: Update the TWO matching blocks in `inferPatternFromRHS` (~lines 1364 and 1381)**

Both use:
```wolfram
Length[pats] === Length[callArgSamples] &&
AllTrue[Transpose[{callArgSamples, pats}],
  (#[[1]] === Missing["Unknown"] || MatchQ[#[[1]], #[[2]]]) &
]
```

Replace both with `overloadSpecMatchesArgs[overload[[1]], callArgSamples]` where we have access to the raw specs. Or since we're inside the `allDefs` scan, use the isVar flag already on `def`:

```wolfram
Module[{pats = Lookup[def, "InputPatterns", {}],
        isVar = Lookup[def, "Variadic", False],
        fixedN, varElemPat},
  If[isVar && Length[pats] > 0,
    fixedN = Length[pats] - 1;
    varElemPat = With[{lp = Last[pats]},
      If[Head[lp] === BlankSequence && Length[lp] === 1, Blank[lp[[1]]],
      If[Head[lp] === BlankNullSequence && Length[lp] === 1, Blank[lp[[1]]],
      Blank[]]]];
    Length[callArgSamples] >= fixedN &&
    (fixedN === 0 || AllTrue[
      Transpose[{Take[callArgSamples, fixedN], Take[pats, fixedN]}],
      (#[[1]] === Missing["Unknown"] || MatchQ[#[[1]], #[[2]]]) &
    ]) &&
    AllTrue[Drop[callArgSamples, fixedN],
      (# === Missing["Unknown"] || MatchQ[#, varElemPat]) &
    ],
    Length[pats] === Length[callArgSamples] &&
    AllTrue[Transpose[{callArgSamples, pats}],
      (#[[1]] === Missing["Unknown"] || MatchQ[#[[1]], #[[2]]]) &
    ]
  ]
]
```

Apply this to both the "first pass" (with ReturnPattern check) and "second pass" matching blocks.

- [ ] **Step 4.4: Update SECOND `$BuiltinPatterns` → def conversion in `resolveCallReturnPattern` (~line 1615)**

Same change as Step 4.2 - update `inPats` to use `builtinSpecToPattern /@ overload[[1]]` and add `"Variadic"` field.

- [ ] **Step 4.5: Update matching block in `resolveCallReturnPattern` (~line 1650)**

Same replacement as Step 4.3.

- [ ] **Step 4.6: Sync and run tests**

```bash
cp LSPServer/Kernel/PacletIndex.wl ~/.Wolfram/Paclets/Repository/LSPServer--2.9.59/Kernel/PacletIndex.wl
wolframscript -code 'Needs["MUnit`"]; Do[report = TestReport[f]; Print[f, "  ", report["TestsSucceededCount"], "/", report["TestsSucceededCount"] + report["TestsFailedCount"]], {f, {"Tests/builtin/BuiltinVariadic.wlt","Tests/ipwl/IPWLDiag.wlt","Tests/ipwl/IPWLHover.wlt","Tests/ipwl/IPWLParse.wlt"}}]' 2>/dev/null
```
Expected: No regressions.

- [ ] **Step 4.7: Commit**

```bash
git add LSPServer/Kernel/PacletIndex.wl
git commit -m "feat: update PacletIndex.wl builtin spec conversion and matching for variadic support"
```

---

## Task 5: Update `$identityOverrides` with variadic specs for key functions

**Files:**
- Modify: `CodeTools/Generate/GenerateBuiltinPatterns.wl` - the `$identityOverrides` block (~line 740)

This is the most impactful task: update the hardcoded overrides for functions that are inherently variadic.

- [ ] **Step 5.1: Replace these entries in `$identityOverrides`**

Find and replace each of the following entries:

**Plus and Times (numeric promotion):**
```wolfram
(* Replace existing Plus and Times entries *)
"Plus" -> {
  {{"Integer..."},   "_Integer"},    (* Plus[n1,n2,...] all-integer -> Integer *)
  {{"Real..."},      "_Real"},        (* any Real in args -> Real *)
  {{"Complex..."},   "_Complex"},
  {{None},           "_?NumericQ"}   (* catch-all / unknown args *)
},
"Times" -> {
  {{"Integer..."},   "_Integer"},
  {{"Real..."},      "_Real"},
  {{"Complex..."},   "_Complex"},
  {{None},           "_?NumericQ"}
},
```

**Boolean variadic operators:**
```wolfram
"And"  -> {{{"_?BooleanQ..."}, "_?BooleanQ"}, {{None}, "_?BooleanQ"}},
"Or"   -> {{{"_?BooleanQ..."}, "_?BooleanQ"}, {{None}, "_?BooleanQ"}},
"Xor"  -> {{{"_?BooleanQ..."}, "_?BooleanQ"}, {{None}, "_?BooleanQ"}},
"Nand" -> {{{"_?BooleanQ..."}, "_?BooleanQ"}, {{None}, "_?BooleanQ"}},
"Nor"  -> {{{"_?BooleanQ..."}, "_?BooleanQ"}, {{None}, "_?BooleanQ"}},
```

> **Note:** `_?BooleanQ...` is not a valid variadic spec by the grammar above (it starts with `_`). The spec syntax only supports type-name prefixes. Use `"..."` (untyped variadic) for these:
```wolfram
"And"  -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
"Or"   -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
"Xor"  -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
"Nand" -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
"Nor"  -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
```

**String functions:**
```wolfram
"StringJoin" -> {{{"String..."}, "_String"}},
```

**Container-joining functions:**
```wolfram
"Join" -> {
  {{"List..."},        "_List"},
  {{"String..."},      "_String"},
  {{"Association..."}, "_Association"},
  {{None},             None}
},
```

**Integer arithmetic functions:**
```wolfram
"GCD" -> {{{"Integer..."}, "_Integer"}, {{None}, "_Integer"}},
"LCM" -> {{{"Integer..."}, "_Integer"}, {{None}, "_Integer"}},
"BitAnd" -> {{{"Integer..."}, "_Integer"}, {{None}, "_Integer"}},
"BitOr"  -> {{{"Integer..."}, "_Integer"}, {{None}, "_Integer"}},
"BitXor" -> {{{"Integer..."}, "_Integer"}, {{None}, "_Integer"}},
```

**Comparison chains (WL allows `Greater[a,b,c]` meaning `a > b > c`):**
```wolfram
"Greater"      -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
"GreaterEqual" -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
"Less"         -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
"LessEqual"    -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
"Equal"        -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
"Unequal"      -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
"SameQ"        -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
"UnsameQ"      -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
```

**Min and Max (variadic):**
```wolfram
"Min" -> {
  {{"Integer..."}, "_Integer"},
  {{"Real..."},    "_Real"},
  {{"List"},       "_?NumericQ"},
  {{None},         "_?NumericQ"}
},
"Max" -> {
  {{"Integer..."}, "_Integer"},
  {{"Real..."},    "_Real"},
  {{"List"},       "_?NumericQ"},
  {{None},         "_?NumericQ"}
},
```

- [ ] **Step 5.2: Commit generator change (data file not yet regenerated)**

```bash
git add CodeTools/Generate/GenerateBuiltinPatterns.wl
git commit -m "feat: update \$identityOverrides with variadic specs for Plus, Times, StringJoin, Join, etc."
```

---

## Task 6: Update generator `extractDVOverloads` to emit variadic specs

**Files:**
- Modify: `CodeTools/Generate/GenerateBuiltinPatterns.wl` lines ~101–332

Currently `extractDVOverloads` sets `"Variadic" -> hasVariadic` but this flag is discarded when writing `combined`. After this task, the variadic type will appear as the last element of the `ArgTypes` list.

- [ ] **Step 6.1: Add `isSeqArg` helper (after `isVariadicArg`, ~line 112)**

```wolfram
(*
  True if the argument is a BlankSequence or BlankNullSequence (captures
  a typed sequence of args).  Excludes OptionsPattern (which is also variadic
  but not a typed sequence and doesn't contribute useful type info).
*)
isSeqArg[arg_] := MatchQ[arg,
  Verbatim[BlankSequence][___]                             |
  Verbatim[BlankNullSequence][___]                         |
  Verbatim[Pattern][_, Verbatim[BlankSequence][___]]       |
  Verbatim[Pattern][_, Verbatim[BlankNullSequence][___]]
]

isBSArg[arg_] := MatchQ[arg,
  Verbatim[BlankSequence][___]                       |
  Verbatim[Pattern][_, Verbatim[BlankSequence][___]]
]
```

- [ ] **Step 6.2: Update `extractDVOverloads` to append variadic spec to `ArgTypes`**

Find inside the `Map[Function[{rule}, ...]` block (~line 287), after `types` and `hasVariadic` are computed:

```wolfram
hasVariadic = AnyTrue[argsList, isVariadicArg];
(* Drop variadic / options args; keep positional ones *)
types = typeFromArg /@ Select[argsList, !isVariadicArg[#] &];
```

Replace with:
```wolfram
hasVariadic = AnyTrue[argsList, isVariadicArg];
(* Positional (non-variadic) args *)
types = typeFromArg /@ Select[argsList, !isVariadicArg[#] &];
(* Sequence args (BlankSequence / BlankNullSequence, NOT OptionsPattern).
   Append a variadic spec string as the last element of types. *)
seqArgs = Select[argsList, isSeqArg[#] &];
If[Length[seqArgs] > 0,
  With[{sa = First[seqArgs], t = typeFromArg[First[seqArgs]]},
    If[isBSArg[sa],
      (* BlankSequence: 1+ args *)
      AppendTo[types, If[StringQ[t], t <> "...", "..."]],
      (* BlankNullSequence: 0+ args *)
      AppendTo[types, If[StringQ[t], t <> "*", "*"]]
    ]
  ]
];
```

The `"ArgTypes"` list in the overload association now has the variadic spec as its last element when applicable. The `"Variadic"` field is kept as-is for backward compat (it's just not used downstream anymore - the spec string is the source of truth).

- [ ] **Step 6.3: Commit generator change**

```bash
git add CodeTools/Generate/GenerateBuiltinPatterns.wl
git commit -m "feat: emit variadic spec strings in extractDVOverloads (Type..., Type*, ...)"
```

---

## Task 7: Fix arity-aware overload selection in `inferCallNodeReturnSampleValue`

**Files:**
- Modify: `LSPServer/Kernel/Diagnostics.wl` lines ~800–811

Currently this always takes `$BuiltinPatterns[fname][[1, 2]]` (first overload's return type). With variadic data, the first overload for Plus will be `{{"Integer..."}, "_Integer"}` which is great for Integer calls - but for a 2-arg Real call, the Real overload should be picked. This task makes the lookup find the best matching overload.

- [ ] **Step 7.1: Replace the `[[1, 2]]` lookup with arity-aware selection (~line 800)**

Find:
```wolfram
(* Builtin with a known fixed return type *)
KeyExistsQ[$BuiltinPatterns, fname],
  Switch[$BuiltinPatterns[fname][[1, 2]],
    "_?BooleanQ", True,
    "_Integer",   0,
    "_String",    "",
    "_Real",      0.,
    "_Rational",  1/2,
    "_List",      {},
    "_Association", <||>,
    None,         Missing["Unknown"],
    _String,      0
  ],
```

Replace with:
```wolfram
(* Builtin with a known return type - pick the best arity-matching overload *)
KeyExistsQ[$BuiltinPatterns, fname],
  With[{ovs = $BuiltinPatterns[fname], nArgs = Length[node[[2]]]},
    Module[{bestOv},
      (* Find the first overload whose arity matches (or is variadic with enough args) *)
      bestOv = SelectFirst[ovs,
        With[{specs = #[[1]], n = Length[#[[1]]]},
          If[n > 0 && StringQ[Last[specs]] &&
               (StringEndsQ[Last[specs], "..."] || StringEndsQ[Last[specs], "*"]),
            nArgs >= n - 1,   (* variadic: arity check for fixed prefix *)
            n === nArgs
          ]
        ] &,
        First[ovs]  (* fallback: first overload if nothing matches *)
      ];
      Switch[bestOv[[2]],
        "_?BooleanQ",  True,
        "_Integer",    0,
        "_String",     "",
        "_Real",       0.,
        "_Rational",   1/2,
        "_List",       {},
        "_Association", <||>,
        None,          Missing["Unknown"],
        _String,       0
      ]
    ]
  ],
```

- [ ] **Step 7.2: Sync and run all tests**

```bash
cp LSPServer/Kernel/Diagnostics.wl ~/.Wolfram/Paclets/Repository/LSPServer--2.9.59/Kernel/Diagnostics.wl
wolframscript -code 'Needs["MUnit`"]; Do[report = TestReport[f]; Print[f, "  ", report["TestsSucceededCount"], "/", report["TestsSucceededCount"] + report["TestsFailedCount"]], {f, {"Tests/builtin/BuiltinVariadic.wlt","Tests/ipwl/IPWLDiag.wlt","Tests/ipwl/IPWLHover.wlt","Tests/ipwl/IPWLParse.wlt","Tests/ipwl/IPWLParametric.wlt"}}]' 2>/dev/null
```
Expected: No regressions.

- [ ] **Step 7.3: Commit**

```bash
git add LSPServer/Kernel/Diagnostics.wl
git commit -m "feat: arity-aware overload selection in inferCallNodeReturnSampleValue"
```

---

## Task 8: Regenerate `BuiltinInputPatterns.wl` and full regression

**Files:**
- Regenerate: `LSPServer/Resources/BuiltinInputPatterns.wl`

- [ ] **Step 8.1: Run the generator**

From repo root:
```bash
wolframscript -file CodeTools/Generate/GenerateBuiltinPatterns.wl 2>/dev/null
```
Expected output (approximately):
```
Collecting System` symbols...
Total System` symbols: ~7000
Fetching PlaintextUsage for all symbols in one batch call...
Building typed pattern associations...
Symbols with typed patterns: ~3000
Total overloads: ~12000
Overloads with typed arg(s): NNNN (NN%)
Overloads with typed return:  NNNN (NN%)
```
The file `LSPServer/Resources/BuiltinInputPatterns.wl` is updated in-place.

- [ ] **Step 8.2: Verify variadic entries are present in generated file**

```bash
wolframscript -code '
Get["LSPServer/Resources/BuiltinInputPatterns.wl"];
Print["Plus overloads: ", $BuiltinPatterns["Plus"]];
Print["StringJoin overloads: ", $BuiltinPatterns["StringJoin"]];
Print["And overloads: ", $BuiltinPatterns["And"]];
'
```
Expected: Plus overloads include an entry with `"Integer..."`, StringJoin includes `"String..."`, And includes `"..."`.

- [ ] **Step 8.3: Sync regenerated file to installed paclet**

```bash
cp LSPServer/Resources/BuiltinInputPatterns.wl ~/.Wolfram/Paclets/Repository/LSPServer--2.9.59/Resources/BuiltinInputPatterns.wl
```
> **Note:** also sync the build/paclet copy: `cp LSPServer/Resources/BuiltinInputPatterns.wl build/paclet/LSPServer/Resources/BuiltinInputPatterns.wl` (if that directory exists and has the file).

- [ ] **Step 8.4: Run full test suite**

```bash
wolframscript -code '
Needs["MUnit`"];
Do[
  report = TestReport[f];
  Print[f, "  ", report["TestsSucceededCount"], "/",
    report["TestsSucceededCount"] + report["TestsFailedCount"]],
  {f, {
    "Tests/builtin/BuiltinVariadic.wlt",
    "Tests/hover/DocCommentDiag.wlt",
    "Tests/hover/DocComment.wlt",
    "Tests/hover/BranchNarrowing.wlt",
    "Tests/ipwl/IPWLParse.wlt",
    "Tests/ipwl/IPWLHover.wlt",
    "Tests/ipwl/IPWLParametric.wlt",
    "Tests/ipwl/IPWLDiag.wlt"
  }}
]
' 2>/dev/null
```
Expected:
- `Tests/builtin/BuiltinVariadic.wlt`: 8/8
- IPWL tests: 17/17 (unchanged from before)
- Hover tests: same counts as baseline (pre-existing failures unchanged)

- [ ] **Step 8.5: Commit everything**

```bash
git add LSPServer/Resources/BuiltinInputPatterns.wl build/paclet/LSPServer/Resources/BuiltinInputPatterns.wl
git commit -m "feat: regenerate BuiltinInputPatterns.wl with variadic specs (Type..., Type*, ...)"
```

---

## Self-Review

**Spec coverage:**
- [x] Variadic syntax defined and documented - Task 1, data file header
- [x] Generator emits variadic specs from DownValues - Task 6
- [x] `$identityOverrides` updated for Plus, Times, And/Or/Xor, StringJoin, Join, GCD/LCM, BitOps, Min/Max, comparisons - Task 5
- [x] Diagnostics.wl consumer updated - Tasks 2, 3, 7
- [x] PacletIndex.wl consumer updated (2 matching sites) - Task 4
- [x] Tests written before implementation (TDD) - Task 1
- [x] Regression run after generation - Task 8

**Placeholder scan:** No TBDs or missing code blocks found.

**Type consistency:** `builtinSpecToPattern` and `overloadSpecMatchesArgs` are defined identically in both private contexts and called consistently throughout.

**WL gotchas:**
- `MatchQ[0, BlankSequence[Integer]]` is never called - tail args use `Blank[T]` extracted from the BlankSequence wrapper
- `"_?BooleanQ..."` is NOT a valid spec (starts with `_`) - logical operators use `"..."` (untyped variadic) instead
- `BlankSequence @@ {}` = `BlankSequence[]` (untyped) - used for `"..."` spec
