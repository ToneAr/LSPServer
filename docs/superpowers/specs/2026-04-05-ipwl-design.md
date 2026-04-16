# IPWL (Inferred Pattern Wolfram Language) Design Spec

**Date:** 2026-04-05
**Status:** Approved

---

## Overview

IPWL is a new `.ipwl` file type that extends Wolfram Language with TypeScript-like type annotations. It provides:

- Explicit pattern declarations for symbols and function return types
- Static analysis via LSPServer (hover labels, diagnostics)
- Runtime enforcement via a separate `IPWL` paclet using PatternMatcher
- Parametric return types (`_[in]`, `_[1]`)
- Companion declaration files for existing `.wl` paclets

---

## Section 1: Syntax

IPWL syntax is a superset of valid WL. The `: pattern` annotation is inserted between the symbol/args and the `=`/`:=`.

### Variable annotation
```wolfram
var: _Integer = 42
var: {___Integer} = {1, 2, 3}
```
This is already valid WL (`Set[Pattern[var, _Integer], 42]`) - no pre-processing needed.

### Function return annotation
```wolfram
f[x_String]: _Integer := StringLength[x]
```
This is **not** valid WL. The pre-processor transforms it to:
```wolfram
(* IPWLReturn: _Integer *)
f[x_String] := StringLength[x]
```

### Declaration-only (companion file)
```wolfram
symbol: _Integer
f[x_String]: _Integer
```
No body - declares a type for a symbol defined in a `.wl` file.

### TagSet / UpValue annotation
```wolfram
f /: Plot[f[args_]]: _String := "plotting f"
```
Pre-processed to:
```wolfram
(* IPWLReturn: _String *)
f /: Plot[f[args_]] := "plotting f"
```

### Parametric return types
```wolfram
g[{in__}]: _[in] := First[{in}]   (* return head = head of in__ elements *)
g[{in__}]: _[1]  := First[{in}]   (* return head = head of first arg's elements *)
```

---

## Section 2: Pre-Processor

A new `LSPServer/Kernel/TypeWL.wl` module handles `.ipwl` files. It runs **before** the normal CodeParser indexer pass.

### Responsibilities
1. Detect `: pattern` annotations between args and `:=`/`=`
2. Transform `f[args]: ret :=` → `(* IPWLReturn: ret *)\nf[args] :=`
3. Transform `f[args]: ret` (no body) → `(* IPWLDeclare: f[args] / ret *)`
4. Emit `IPWLSyntaxError` diagnostic for malformed annotations
5. Return the transformed WL source string for normal indexing

### Pre-processor is LSPServer-internal
The IPWL paclet does its own transformation at runtime. The LSPServer pre-processor is only for static analysis - it never executes the code.

---

## Section 3: Two-Paclet Split

### LSPServer (static analysis)
- Recognises `.ipwl` file extension
- Runs pre-processor on `.ipwl` files
- Extracts `"DeclaredType"` annotations into `$PacletIndex`
- Hover: `"**Declared type:** \`pattern\`"`
- Diagnostics: `DeclaredTypeMismatch`, `DeclaredInputMismatch`, `IPWLSyntaxError`, `IPWLUnresolvedSymbol`
- Static-only for `.wl` files (doc-comment `Return:` annotations unchanged)

### IPWL Paclet (runtime enforcement)
- Separate paclet, own repo
- `Needs["IPWL`"]` + `Get["file.ipwl"]` installs runtime type guards
- Uses **PatternMatcher** (DanielS, Wolfram Compiler team) as first-class dependency
  - `CompilePatternToBytecode[pattern]` at load time
  - `PatternMatcherMatchQ[result, compiledPattern]` at call time
  - Falls back to native `MatchQ` with a one-time warning if PatternMatcher unavailable
- Throws `TypeError` on return mismatch, `ArgumentError` on input mismatch
- Public API:
  ```wolfram
  Get["myfile.ipwl"]
  IPWLStatus[]
  DisableIPWLGuards[]
  GenerateIPWLDeclarations[pacletDir]
  ValidateIPWLDeclarations[pacletDir]
  ```

---

## Section 4: PacletIndex Integration

### New field: `"DeclaredType"`
Each symbol definition entry in `$PacletIndex["Symbols"][sym]["Definitions"]` gains an optional `"DeclaredType"` field:

```wolfram
<|
  "uri"          -> "file:///...",
  "source"       -> {{line, col}, {line, col}},
  "kind"         -> "declaration",
  "DeclaredType" -> Blank[Integer],     (* explicit - takes precedence *)
  "InferredPattern" -> None
|>
```

### Precedence
`"DeclaredType"` overrides `"InferredPattern"` everywhere - hover, diagnostics, return-type checks.

### Companion file merging
When an `.ipwl` companion declares `symbol: pattern_` with no body, it patches the existing symbol entry. If the symbol doesn't exist yet (companion loaded before the `.wl` file), a stub entry with `kind -> "declaration"` is created and filled in when the `.wl` file is indexed.

### File tracking
`$PacletIndex["Files"][uri]` gains an `"IsIPWL" -> True/False` flag.

---

## Section 5: Runtime Enforcement Architecture

### Guard installation
`Get["file.ipwl"]` evaluates the pre-processed source, then for each annotated function installs a wrapper:

```wolfram
Unprotect[f];
With[{$compiled = CompilePatternToBytecode[ret]},
  f[args] := Module[{$result = body},
    If[!PatternMatcherMatchQ[$result, $compiled],
      Throw[TypeError["f", ret, $result]]
    ];
    $result
  ]
];
Protect[f];
```

### Input guards
For annotated parameters (`x: _Integer`), an `ArgumentError` is thrown if the input doesn't match. Same compile-once model.

### PatternMatcher dependency
- First-class: treated as required, not optional
- Fallback to native `MatchQ` exists but emits a one-time `$IPWLPatternMatcherUnavailable` warning
- `GenerateIPWLDeclarations[dir]` scans a `.wl` paclet and emits a companion `.ipwl` file
- `ValidateIPWLDeclarations[dir]` checks `.ipwl` declarations against live definitions

---

## Section 6: Hover & Diagnostics

### Hover labels
| Source | Label |
|--------|-------|
| `"DeclaredType"` | `**Declared type:** \`pattern\`` |
| `"InferredPattern"` (structured) | `**Inferred Pattern:** \`pattern\`` |
| `"InferredPattern"` (atom) | `**Inferred type:** \`pattern\`` |
| `"DocComment"` ReturnPattern | shown in Doc Comments section |

If both `"DeclaredType"` and `"DocComment"` exist, declared type wins for the type line; description still shows.

Companion-declared symbols show: `*(declared in myfile.ipwl)*`

### New diagnostics
| Code | Severity | Description |
|------|----------|-------------|
| `DeclaredTypeMismatch` | 1 (error) | Body return type doesn't match `"DeclaredType"` |
| `DeclaredInputMismatch` | 1 (error) | Call-site arg doesn't match declared parameter pattern |
| `IPWLSyntaxError` | 1 (error) | Malformed annotation in `.ipwl` file |
| `IPWLUnresolvedSymbol` | 2 (warning) | Declared symbol not found in any indexed `.wl` file |

---

## Section 7: Parametric Return Types

Only supported in `.ipwl` files.

### Syntax
```wolfram
g[{in__}]: _[in] := First[{in}]   (* named: return head = element head of in__ *)
g[{in__}]: _[1]  := First[{in}]   (* positional: return head = element head of arg 1 *)
```

### Storage
Parametric return types are stored as symbolic expressions in `"DeclaredType"`:
- `_[in]` → `Blank[ParametricRef["in"]]`
- `_[1]` → `Blank[ParametricRef[1]]`

### Resolution
Resolved lazily at call sites. When hover or diagnostics needs the concrete return type:
1. Look up the inferred type of the actual argument at the call site
2. Extract the element head from that type
3. Substitute into `Blank[...]`

**Fallback**: If the argument type can't be inferred, resolves to `None` - no hover label, no diagnostic fired.

### Example resolution
```wolfram
g[{1, 2, 3}]   (* arg 1 inferred as {___Integer} → element head = Integer → return _Integer *)
g[{"a", "b"}]  (* arg 1 inferred as {___String}  → element head = String  → return _String *)
g[unknownList] (* arg 1 unknown → parametric resolves to None, no warning *)
```

---

## Files Touched

### New files
- `LSPServer/Kernel/TypeWL.wl` - IPWL pre-processor and annotation extractor
- `IPWL/` - separate paclet (own repo), runtime enforcement

### Modified files (LSPServer)
- `LSPServer/Kernel/PacletIndex.wl` - `"DeclaredType"` field, `"IsIPWL"` flag, companion merging
- `LSPServer/Kernel/Hover.wl` - `"Declared type:"` label, parametric resolution
- `LSPServer/Kernel/Diagnostics.wl` - `DeclaredTypeMismatch`, `DeclaredInputMismatch`
- `LSPServer/Kernel/Workspace.wl` - add `*.ipwl` to watched file globs
- `LSPServer/PacletInfo.wl.in` - version bump

### Test files
- `Tests/hover/DocComment.wlt` / `DocCommentTest.wl` - existing (no changes needed)
- `Tests/ipwl/IPWLParse.wlt` + `IPWLParseTest.ipwl` - pre-processor tests
- `Tests/ipwl/IPWLHover.wlt` + `IPWLHoverTest.ipwl` - hover label tests
- `Tests/ipwl/IPWLDiag.wlt` + `IPWLDiagTest.ipwl` - diagnostic tests
- `Tests/ipwl/IPWLParametric.wlt` + `IPWLParametricTest.ipwl` - parametric type tests
