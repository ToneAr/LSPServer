# Ignore Patterns

LSPServer supports ESLint-style ignore patterns for suppressing diagnostics at multiple levels. This allows fine-grained control over which warnings and errors are shown.

## Table of Contents

- [Comment-Based Ignores](#comment-based-ignores)
  - [Line-Level Ignore](#line-level-ignore)
  - [Next-Line Ignore](#next-line-ignore)
  - [Block-Level Ignore](#block-level-ignore)
  - [File-Level Ignore](#file-level-ignore)
- [Project Configuration](#project-configuration)
  - [Configuration File](#configuration-file)
  - [Rule Settings](#rule-settings)
  - [File Patterns](#file-patterns)
- [Rule Matching Syntax](#rule-matching-syntax)
- [Common Rule Names](#common-rule-names)
- [Examples](#examples)

## Comment-Based Ignores

All comment-based ignores use Wolfram Language comment syntax `(* ... *)`.

### Line-Level Ignore

Disable rules for the current line only.

**Syntax:**
```wolfram
code  (* wl-disable-line *)
code  (* wl-disable-line RuleName *)
code  (* wl-disable-line Rule1, Rule2 *)
```

**Examples:**
```wolfram
(* Disable all rules for this line *)
x = undefinedVar  (* wl-disable-line *)

(* Disable specific rule *)
y = anotherUndefined  (* wl-disable-line UndefinedSymbol *)

(* Disable multiple rules *)
z = foo + bar  (* wl-disable-line UndefinedSymbol, UnusedVariable *)
```

### Next-Line Ignore

Disable rules for the next line only.

**Syntax:**
```wolfram
(* wl-disable-next-line *)
code

(* wl-disable-next-line RuleName *)
code
```

**Examples:**
```wolfram
(* Disable all rules for next line *)
(* wl-disable-next-line *)
x = undefinedVar

(* Disable specific rule for next line *)
(* wl-disable-next-line UndefinedSymbol *)
y = anotherUndefined
```

### Block-Level Ignore

Disable rules for a block of code.

**Syntax:**
```wolfram
(* wl-disable *)
... code ...
(* wl-enable *)

(* wl-disable RuleName *)
... code ...
(* wl-enable RuleName *)
```

**Examples:**
```wolfram
(* Disable all rules for this block *)
(* wl-disable *)
x = foo
y = bar
z = baz
(* wl-enable *)

(* Disable specific rule *)
(* wl-disable UndefinedSymbol *)
a = undefined1
b = undefined2
(* wl-enable UndefinedSymbol *)

(* This line will show diagnostics again *)
c = undefined3
```

**Note:** If `wl-enable` is not found, the disable extends to the end of the file.

### File-Level Ignore

Disable rules for the entire file. Place at the top of the file.

**Syntax:**
```wolfram
(* wl-disable-file *)
(* wl-disable-file RuleName *)
(* wl-disable-file Rule1, Rule2 *)
```

**Examples:**
```wolfram
(* Disable all diagnostics for this file *)
(* wl-disable-file *)

(* Or disable specific rules *)
(* wl-disable-file UndefinedSymbol, UnusedVariable *)

(* Rest of file... *)
```

## Project Configuration

### Configuration File

Create a `.wllintrc` or `.wllintrc.json` file in your project root directory.

**JSON Format (`.wllintrc.json` or `.wllintrc`):**
```json
{
  "rules": {
    "UndefinedSymbol": "off",
    "UnusedVariable": "warn",
    "Deprecated*": "off"
  },
  "ignorePatterns": [
    "**/test/**",
    "**/*.test.wl",
    "**/examples/**"
  ]
}
```

**Wolfram Language Format (`.wllintrc`):**
```wolfram
<|
  "rules" -> <|
    "UndefinedSymbol" -> "off",
    "UnusedVariable" -> "warn",
    "Deprecated*" -> "off"
  |>,
  "ignorePatterns" -> {
    "**/test/**",
    "**/*.test.wl"
  }
|>
```

### Rule Settings

Each rule can have one of these settings:

| Setting | Description |
|---------|-------------|
| `"off"` or `false` | Disable the rule entirely |
| `"warn"` | Show as warning (default behavior) |
| `"error"` | Show as error |

### File Patterns

The `ignorePatterns` array accepts glob-style patterns to exclude entire files or directories from diagnostics:

- `**/test/**` - Ignore all files in any `test` directory
- `**/*.test.wl` - Ignore all files ending in `.test.wl`
- `examples/**` - Ignore files in the `examples` directory

## Rule Matching Syntax

All ignore patterns support these matching modes:

| Pattern | Description | Example |
|---------|-------------|---------|
| `*` | Matches all rules | `(* wl-disable-line *)` |
| `RuleName` | Exact match | `(* wl-disable-line UndefinedSymbol *)` |
| `Prefix*` | Prefix match | `(* wl-disable-line Undefined* *)` matches `UndefinedSymbol`, `UndefinedFunction`, etc. |

## Common Rule Names

Here are some common diagnostic rule names you might want to suppress:

| Rule Name | Description |
|-----------|-------------|
| `UndefinedSymbol` | Symbol is not defined in paclet or System context |
| `UnusedVariable` | Variable is defined but never used |
| `UnloadedContext` | Using symbol from a context that isn't loaded |
| `ShadowedVariable` | Variable shadows another variable |
| `DuplicateClauses` | Multiple clauses with same pattern |
| `UnexpectedCharacter` | Unexpected character in code |
| `SyntaxError` | Syntax error in code |

**Note:** Rule names correspond to the `Tag` field of `InspectionObject` from CodeInspector.

## Examples

### Example 1: Suppressing External Package Warnings

```wolfram
(* wl-disable-file UnloadedContext *)
(* This file uses symbols from packages without explicit Needs[] *)

Developer`ToPackedArray[{1, 2, 3}]
Internal`Bag[]
```

### Example 2: Test File with Known Issues

```wolfram
(* wl-disable-file UndefinedSymbol, UnusedVariable *)
(* Test file - intentionally uses undefined symbols for testing *)

BeginPackage["MyTests`"]

testUndefinedHandling[] := Module[{unused},
  undefinedFunction[1, 2, 3]
]

EndPackage[]
```

### Example 3: Legacy Code Block

```wolfram
(* Modern code with full diagnostics *)
processData[data_] := Module[{result},
  result = Transform[data];
  result
]

(* wl-disable *)
(* Legacy code - suppress all warnings *)
oldFunction[x_] := (
  tmp = x;
  foo[tmp]
)
(* wl-enable *)

(* Back to normal diagnostics *)
newFunction[x_] := x^2
```

### Example 4: Project Configuration

**`.wllintrc.json`:**
```json
{
  "rules": {
    "UndefinedSymbol": "off",
    "UnusedVariable": "warn",
    "ShadowedVariable": "off",
    "Experimental*": "off"
  },
  "ignorePatterns": [
    "**/Tests/**",
    "**/Documentation/**",
    "**/*.nb"
  ]
}
```

This configuration:
- Disables `UndefinedSymbol` warnings project-wide
- Keeps `UnusedVariable` as warnings
- Disables `ShadowedVariable` warnings
- Disables all rules starting with `Experimental`
- Ignores all files in `Tests` and `Documentation` directories
- Ignores all notebook files

## API Reference

The ignore patterns module exposes these functions:

```wolfram
(* Parse ignore comments from a CST *)
ParseIgnoreComments[cst]
(* Returns: <|"LineDisables" -> ..., "NextLineDisables" -> ..., 
              "BlockDisables" -> ..., "FileDisables" -> ...|> *)

(* Load project configuration *)
LoadProjectIgnoreConfig[workspacePath]
(* Returns: <|"rules" -> <|...|>, "ignorePatterns" -> {...}|> *)

(* Check if a diagnostic should be ignored *)
ShouldIgnoreDiagnostic[diagnostic, ignoreData]
(* Returns: True | False *)

(* Get ignore data for a file *)
GetIgnoreData[uri]

(* Update ignore data for a file (called automatically) *)
UpdateIgnoreData[uri, cst]

(* Clear ignore data for a file *)
ClearIgnoreData[uri]
```

## Compatibility

- Ignore patterns work alongside CodeInspector's existing suppression system (`::CodeAnalysis::` comments)
- Both systems are checked when filtering diagnostics
- Project configuration is loaded when the workspace is initialized
