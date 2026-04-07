# Closure-Scoped Type Inference for Module/Block/With

**Date:** 2026-04-07
**Branch:** tonya/wl-doc-experiments

---

## Problem

Type inference for local variables declared inside `Module`, `Block`, or `With` closures
currently has two bugs:

1. **Scope bleed (Diagnostics.wl):** Entries for closure-local variables in `localVarMap`
   are tagged with `branchRange = None`, making them visible to any code that comes
   after the closure — including code at file scope or in unrelated closures. Two
   `Module`s both binding `x` contaminate each other.

2. **No flow-sensitivity (Hover.wl Case F):** The hover type for a Module/Block/With
   local variable is fixed at the var-list initializer. Re-assignments inside the closure
   body are invisible, so hovering after `x = "hello"` still shows `Blank[Integer]` if
   the initializer was `x = 5`.

---

## Goals

- Entries for a closure-local variable are only visible to usages **inside** that closure.
- Within a closure, the inferred type **updates** as assignments accumulate — last
  assignment before the cursor wins (same semantics as the existing position-aware
  PacletIndex lookup for global symbols).
- The same symbol name used **outside** the closure is unaffected.
- `With` vars (immutable) are handled correctly with no extra work since no `Set` nodes
  can appear in a `With` body.

---

## Approach: closureRange as a 5th entry field (Approach A)

Add an orthogonal `closureRange` field alongside the existing `branchRange` field.
Both must pass independently, which correctly handles closure-local vars that also
appear inside an `If` branch.

---

## Diagnostics.wl Changes

### 1. Pre-scan: build `closureVarRanges`

Before `rawEntries`, scan the AST for all `Module | Block | With` nodes.
For each node:
- Extract declared variable names from the var-list (`List[...]` first arg).
  Names come from bare `LeafNode[Symbol, name, _]` entries (uninitialised) and
  `Set[LeafNode[Symbol, name, _], _]` entries (initialised).
- Record the closure's source range `closureSrc = {{startLine, startCol}, {endLine, endCol}}`.

Build a map:
```wolfram
closureVarRanges : <| "x" -> {closureSrc1, closureSrc2, ...}, ... |>
```

One symbol name can appear in multiple closures; all ranges are collected.

### 2. Helper: `findEnclosingClosureForVar[varName, line]`

```
candidates = closureVarRanges[varName] filtered to ranges that contain line
result     = candidate with the largest start line (innermost)
           = None if no candidates
```

### 3. Entry format: 4-field → 5-field

| Entry type | 5th field |
|---|---|
| `rawEntries` | `findEnclosingClosureForVar[varName, assignLine]` |
| `convergenceEntries` | `findEnclosingClosureForVar[varName, afterLine]` |
| `condEntries` | `None` (already scoped to branch body range via 4th field) |
| `paramEntries` | `None` (scoped via `defSrc` in 4th field) |
| `iterEntries` | `None` (scoped via `iterSrc` in 4th field) |
| `mapParamEntries` | `None` (scoped via `funcSrc` in 4th field) |

### 4. GroupBy update

Currently strips `varName` and keeps `{line, sample, branchRange}`.
After: `{line, sample, branchRange, closureRange}`.

### 5. `inferArgSampleValue` lookup update

Add a second independent range check. An entry is valid only if:
```wolfram
e[[1]] <= argLine                         (* before or at usage *)
&& branchRangeCheck[e[[3]], argLine]       (* satisfies branch scope *)
&& closureRangeCheck[e[[4]], argLine]      (* satisfies closure scope *)
```

where `closureRangeCheck[r, line]` is:
```wolfram
r === None || (MatchQ[r, {{_,_},{_,_}}] && r[[1,1]] <= line <= r[[2,1]])
```

---

## Hover.wl Changes (Case F)

### Current behaviour

Case F scans the var-list of each `Module | Block | With` node for an entry matching
`tokenSymbol`. Returns:
- `Blank[]` for a bare (uninitialised) symbol
- A pattern inferred from the initializer RHS for `x = rhs`

Body re-assignments are ignored.

### New behaviour

After producing the initializer entry, also scan the **body** (args after index 1) for
`Set[tokenSymbol, rhs]` nodes. Build a list of `{line, pattern}` pairs:

1. `{closureStartLine, initPattern}` — the initializer (or `Blank[]` if uninitialised).
   Using `closureStartLine` ensures it sorts before any body assignment.
2. For each `Set[tokenSymbol, rhs]` in the body: `{assignLine, bodyPattern}` using the
   same inference logic (`argNodePattern`, `inferListElementPattern`, `Blank[Association]`,
   else `None`).

Filter to entries where `line <= cursorLine`, sort by line, take `Last`.
If no entries survive the filter, fall back to `Blank[]`.

Pattern inference for body assignments uses the identical helper chain as the
initializer case — no new inference logic.

**With:** Vars are immutable in practice — users rarely (and shouldn't) assign to a
`With`-bound name inside the body, since WL would substitute the var away before
evaluation. If such a `Set` node does appear in the CST, the body scan picks it up,
which is acceptable (it's an unusual pattern and the inferred type reflects the written
code). In normal usage the body scan adds nothing and the initializer entry is the
only one.

---

## Files Changed

- `LSPServer/Kernel/Diagnostics.wl` (source)
- `LSPServer/Kernel/Hover.wl` (source)
- `build/paclet/LSPServer/Kernel/Diagnostics.wl` (synced copy)
- `build/paclet/LSPServer/Kernel/Hover.wl` (synced copy)
- Installed paclet copy at `~/.Wolfram/Paclets/Repository/LSPServer--*/Kernel/`

No PacletIndex.wl changes required (PacletIndex already records one entry per
definition at indexing time; position-aware lookup in handleUserSymbols handles
global symbols correctly and is unaffected by this change).

---

## Test Cases

New tests to add (or verify) in `Tests/hover/`:

### Hover (Hover.wl)

```wolfram
Module[{x = 5},
  x = "hello";
  x           (* hover here: Blank[String], not Blank[Integer] *)
]

Module[{x = 5},
  x           (* hover here: Blank[Integer] *)
  x = "hello"
]

Module[{x},
  x           (* hover here: Blank[] *)
]

(* Two modules with same var name should not interfere *)
Module[{x = 5}, x];      (* x: Integer *)
Module[{x = "hi"}, x];   (* x: String *)
x                          (* hover here: no inferred pattern from closure-local scope;
                              falls through to PacletIndex / global assignment *)
```

### Diagnostics (Diagnostics.wl)

Same-name vars in different closures should not produce false mismatch warnings from
the other closure's type. Existing `DocCommentDiag.wlt` tests remain passing.

---

## Non-Goals

- No changes to PacletIndex.wl.
- No changes to `condEntries` closureRange (condEntries are already branchRange-scoped
  to inside the closure body; bleed is not possible in practice).
- No handling of `Module` variables shared across nested closures (that is a distinct
  feature not requested here).
