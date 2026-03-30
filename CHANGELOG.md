
## 2.9.41 - 30 Mar 2026

### Improvements

#### Context Alias Support

- Hover and external-symbol resolution now understand alias-qualified package contexts introduced with `Needs["Package`" -> "Alias`"]`.
- Hover shows both the alias context used in source and the resolved full context when applicable.
- Alias-qualified contexts are treated consistently by dependency-aware symbol resolution.

#### Semantic Tokens for Math Scoping Forms

- Semantic token scoping now recognizes bound variables in common math constructs including `Integrate`, `NIntegrate`, `D`, `Solve`, `NSolve`, `Reduce`, `FindInstance`, `FindRoot`, `DSolve`, `NDSolve`, `Series`, `Residue`, and `Limit`.
- Iterator-style variables in these forms are highlighted as scoped parameters more consistently.

#### Tests

- Expanded completion coverage for alias-qualified package loading.
- Added test cases covering `Needs["DatabaseLink`" -> "DBL`"]` and alias-based symbol usage.

## 2.9.28 - 29 Mar 2026

### New Features

#### Smart Association Key Autocomplete

Intelligent autocompletion for association keys that understands variable types and nested structures.

- **Quote trigger**: Type `data["` to see all string keys for `data`
- **Bracket trigger**: Type `data[` or `data[[` to see keys without typing a quote
- **Chained access**: `data["users"]["` navigates nested associations and suggests keys at the correct level
- **Comma Part syntax**: `data["users", 1, "` navigates through multi-key Part access
- **Key[] wrapper**: `data[Key["` works like `data["`
- **List-of-associations**: `list[[` on `{<|...|>, <|...|>}` suggests numeric indices `{1, 2, ...}`; `list[[1]]["` suggests keys from element 1 specifically
- **Per-element keys**: Different elements can have different keys; `list[[1]]["` and `list[[2]]["` may suggest different sets
- **`All` and `Span`**: `list[[All]]["` and `list[[;;, "` suggest merged keys from all elements
- **Mixed key types**: Integer keys like `<|1 -> "one", "x" -> 3|>` are included in suggestions
- **Prefix filtering**: Typing partial keys (e.g., `"ho`) filters to matching keys (`hostname`, `hostport`)
- **Text fallback**: Provides completions even when the CST is unavailable (e.g., during typing)
- **Correct textEdit ranges**: Handles existing closing quotes and generates proper insertion ranges

#### Multi-Source Completion Engine

Completely rewritten completion system with six prioritized sources:

1. **Paclet/workspace symbols** (highest priority) -- symbols defined in your project
2. **External dependency symbols** -- symbols from packages loaded via `Needs[]`/`Get[]`
3. **Kernel context symbols** -- symbols from any kernel-known context (e.g., `Internal`Bag`, `Developer`ToPackedArray`) using `Names[]`
4. **System built-in symbols** -- all built-in functions and constants
5. **Options** -- known Wolfram Language options with ` -> ` auto-insert
6. **Context paths** -- all kernel-known contexts discovered via `Contexts[]`

Trigger characters: `$` (system variables), `` ` `` (context paths), `[` (function args / association keys), `"` (association string keys). Results deduplicated and limited to 100 items. Supports lazy documentation loading via `completionItem/resolve`.

#### Kernel Context Awareness

Contexts like `Internal``, `Developer``, `Compile``, and all other kernel-resident contexts are now recognized as always available:

- **No false warnings**: Using `Internal`Bag` or `Developer`ToPackedArray` no longer produces "context not loaded" warnings. The LSP uses `Contexts[]` to discover all kernel-known contexts dynamically.
- **Context completions**: Typing `` Internal` `` shows all `Internal`` sub-contexts and symbols. All ~1400+ kernel contexts are available for completion.
- **Symbol completions**: Typing `Internal`Ba` suggests `Internal`Bag`, `Internal`BagPart`, `Internal`BagLength` via `Names[]`.

#### ESLint-Style Diagnostic Suppression

Comprehensive system for suppressing diagnostics at multiple levels:

- **Line-level**: `(* wl-disable-line RuleName *)` -- suppress on current line
- **Next-line**: `(* wl-disable-next-line RuleName *)` -- suppress on next line
- **Block-level**: `(* wl-disable RuleName *)` ... `(* wl-enable RuleName *)` -- suppress within a block
- **File-level**: `(* wl-disable-file RuleName *)` -- suppress for entire file
- **Project-level**: `.wllintrc` or `.wllintrc.json` configuration file

Supports wildcards (`*` for all rules), prefix matching (`Undefined*`), and multiple rules per directive. See [docs/ignore-patterns.md](docs/ignore-patterns.md) for full documentation.

#### Quick Fix Code Actions for Suppression

Two new code actions available on any diagnostic (lightbulb menu):

- **"Disable RuleName for this line"** -- appends `(* wl-disable-line RuleName *)` to the end of the line
- **"Disable RuleName for next line"** -- inserts `(* wl-disable-next-line RuleName *)` above the line

Already-suppressed diagnostics are automatically skipped.

#### Workspace Symbol Index

Full workspace-wide symbol tracking via the new PacletIndex system:

- **Cross-file go-to-definition**: Jump to where a symbol is defined in any workspace file
- **Workspace symbol search**: Search symbols across the entire project (Ctrl+T / Cmd+T)
- **Symbol visibility tracking**: Distinguishes public vs private symbols based on package structure
- **Definition kinds**: Tracks functions, constants, options, attributes, and public declarations
- **Usage extraction**: Extracts `::usage` messages from workspace files for hover and completion
- **Dependency loading**: Automatically loads external packages via `Needs[]` for completions
- **Incremental updates**: Re-indexes individual files on save/change without full workspace scan

#### Enhanced Hover with Context Display

Hover information now shows the full context path for all symbol types:

- **System symbols**: Shows `` `System` `` followed by usage and documentation link
- **User-defined symbols**: Shows the context from PacletIndex (e.g., `` `MyPackage`Private` ``) followed by usage and definition patterns
- **External package symbols**: Shows the package context (e.g., `` `Developer` ``) followed by usage, definition patterns extracted from `DownValues`/`UpValues`/`SubValues`, and documentation link

#### Workspace Diagnostics

New diagnostic categories integrated into the publishing pipeline:

- **Undefined symbol detection**: Symbols not recognized in system, paclet, or local scope are flagged
- **Unloaded context warnings**: Using `Developer`ToPackedArray` without `Needs["Developer`"]` produces a warning (except for kernel-resident contexts which are always available)
- **Ignore pattern filtering**: All diagnostics pass through the `wl-disable` comment system before publishing

#### Inlay Hints

Context hints shown inline for symbols from non-System, non-Global contexts. Helps identify where a symbol is defined without hovering.

Infrastructure also includes (currently disabled) parameter name hints for known function signatures and return type hints based on naming conventions.

#### Enhanced Folding Support

Improved code folding to support all multi-line constructs:

- All function calls: `f[a, b, c]`
- Lists: `{1, 2, 3}`
- Associations: `<|k -> v|>`
- Part expressions: `data[[i, j]]`
- Parenthesized groups: `(a + b)`
- Nested structures with individual folding regions

Added `startCharacter` and `endCharacter` to folding ranges for better editor support.

#### Workspace Folder Support

- Handles `workspace/didChangeWorkspaceFolders` notifications
- Adding or removing workspace folders triggers re-indexing of affected files
- Registered with `changeNotifications: true`

### Build & CI

GitHub Actions workflow for automated cross-platform builds and releases:

- **Automated builds**: CI workflow triggers on push to `master` and `build` branches and on pull requests
- **Cross-platform**: Builds native libraries for Linux x86-64, Linux ARM64, macOS x86-64, macOS ARM64, Windows x86-64, and Windows ARM64
- **Release process**: Automatically creates GitHub releases with the built `.paclet` archive as a downloadable artifact when pushing a version tag
- **WolframScript detection**: Enhanced CMake logic to locate `wolframscript` and `WolframKernel` across default install paths on all platforms
- **Paclet assembly**: Build pipeline assembles the final paclet archive including entitlement signing in a `dist/` output directory

### Files Added
- `LSPServer/Kernel/Completion.wl` -- Multi-source completion engine with association key autocomplete
- `LSPServer/Kernel/IgnorePatterns.wl` -- ESLint-style diagnostic suppression
- `LSPServer/Kernel/PacletIndex.wl` -- Workspace symbol index, context tracking, dependency management
- `LSPServer/Kernel/InlayHints.wl` -- Inlay hints provider
- `docs/ignore-patterns.md` -- Full documentation for diagnostic suppression
- `.github/workflows/build.yml` -- GitHub Actions CI/CD workflow

### Files Modified
- `LSPServer/Kernel/Hover.wl` -- Context display, external symbol hover, three-tier resolution chain
- `LSPServer/Kernel/CodeAction.wl` -- Quick fix code actions for diagnostic suppression
- `LSPServer/Kernel/Diagnostics.wl` -- Workspace diagnostics, ignore pattern integration
- `LSPServer/Kernel/Workspace.wl` -- Workspace symbol search, folder change handling
- `LSPServer/Kernel/FoldingRange.wl` -- Enhanced folding support
- `LSPServer/Kernel/LSPServer.wl` -- Module loading, trigger characters, capability registration, initialization
- `CMakeLists.txt` -- Added new source files to build and cross-platform CI support
- `cmake/WolframKernel.cmake` -- Enhanced WolframScript and WolframKernel detection
- `Tests/hover/*.wlt` -- Updated test expectations for context display

## 1.8 - 10 Oct, 2022

Get rid of build date checking.

Do not kill the kernel if URINotFound is thrown.

### Fixes

Fix bad formatting of kernel version.


## 1.7 - 4 July, 2022

Fix handling non-BMP PUA characters

Address issues here: https://github.com/WolframResearch/vscode-wolfram/issues/10

Only compare major.minor when doing version checks and only do build
date check if versions are identical

Add foldingRange

13.1 syntax updates


## 1.6 - 12 May, 2022

Add function usage and function definition patterns to hover.


### Fixes

Fix handling stale content for textDocument/documentSymbol and textDocument/references


## 1.5 - 7 Mar, 2022

Add ProcessDirectory option to RunServerDiagnostic

Add Hierarchical Document Symbol support (outlines)

Add a mini server to diagnostics to test for various bugs before doing actual diagnostics

Work-around serious bug 419428 for now

Internal\`WithLocalSettings is broken, so manually insert UnlockQueue[] calls

Use 0.4 seconds, same as default value of spelling squiggly in FE

Handle ScheduledTask in earlier versions before it held its expr

FromDateString was introduced in 12.3, so use a version check

https://github.com/WolframResearch/vscode-wolfram/issues/8

Create a special message for the error and put in queue as regular traffic

This guarantees the proper order of things being read

initializationOptions may be Null

Handle workspace/didChangeConfiguration notification

https://github.com/WolframResearch/LSPServer/issues/1

13.0.1 syntax updates

Only try reporting stdout / stderr for up to 1 second


### Fixes

Fix race condition with stdio error being checked before all previous traffic has been processed

Fix issues found by running with Jupyter Lab LSP


## 1.4 - 25 Oct, 2021

Add Startup Message handling

There may be internal errors in LSPServer that emit messages during ``Needs["LSPServer`"] ``

These messages are exceptionally hard to handle because any code for handling has not yet been loaded

The messages may cause unexplained hangs in clients

So manually set $Messages to a tmp file and then handle the messages later


Do not allow PacletManager to participate in finding \`Generate\` files


Add more features to RunServerDiagnostic:

Print given kernel path and kernel path to-be-started, and check they are the same.

Add a 30 second timeout for the while diagnostic.

Keep track of how long initialize takes, and error if greater than 10 seconds


Use Internal\`WithLocalSettings to protect against aborts when doing LockQueue / UnlockQueue


RunServerDiagnostic: reduce "must be run with same kernel" to warning


If there were messages when loading LSPServer\`, then report in the diagnostic


use FromDateString with `"Language" -> "en"` for more robust date parsing


### Fixes

Various fixes for RunServerDiagnostic:

BinaryWrite may fail, so check return value and quiet `BinaryWrite::errfile`

If `arr == {}` returns unevaluated, then whole Which returns unevaluated


## 1.3 - 30 Aug, 2021

Notes on compatibility have been added to docs/compatibility.md

work around bug 410895, all quotes are stripped from StartProcess on Windows

Experimental support for sockets

Experimental support for multiple clients


## 1.2 - 25 Mar, 2021

Allow `textDocument/definition` to lookup symbols with or without contexts

e.g., allow foo\`bar to look up definition for bar and vice versa

Allow `textDocument/hover` to work with symbols and display their usage messages.

The usage messages are parsed directly from the linear syntax.

Add a build step to generate a file ReplacePUA.wl that provides a map for converting PUA characters to ASCII approximations.

Use LongNames.wl to provide a text replacement for PUA characters that cannot render properly in other editors

Add a background thread for reading from stdin. This thread will write to a queue that the server will read on its main thread.

The server will look at the queue and determine if any of the messages can be discarded.

For example, a long sequence of `textDocument/didChange` requests do not need to be processed. Only the final one needs to be processed.

Similarly, other requests that may be in the queue before a `textDocument/didChange` may also be discarded.

Do a little work on only reparsing if needed.

Handle `textDocument/documentSymbol`

Handle more color literals and also handle `textDocument/colorPresentation`

LSP clients will have a 10 second timeout for starting the kernel. After that, a dialog is presented explaining that there is a problem and diagnostic code is presented to run in a notebook.

Introduce delays for running various methods.

Implementation of `textDocument/selectionRange`

Initial implementation of semantic tokens


## 1.1 - 16 Sep, 2020

Initial work with formatting

Wire up tabSize and insertSpaces options

Add range formatting

Introduce new BracketMatcher UI

Handle workspace/didChangeWachedFiles that comes from IntelliJ plugin

Add implicit tokens for ExpectedOperands


## 1.0 - 2 Apr, 2020

Add preliminary implementation of hover

Add a native library for handling stdio, and remove the Python proxy script

Keep all content as ByteArrays and introduce publishing of implicit Times

Add versions notification

Add a definitions provider

Display other implicit tokens


Enable sending implicit 1, implicit All, and implicit Null to a client.
A little language has been invented for representing implicit tokens,
and combinations thereof, with a single character.


## 0.15 - 15 Jan, 2020

Add Creator field

Quit kernel if any messages on startup

Add color provider

Require using `File[]` wrapper


## 0.14 - 28 Oct, 2019

Add support for CodeActions

Add ConfidenceLevel setting

Handle other CodeAction commands

Remove Lints that are shadowed

Only convert bytes to string if debug logging


### Fixes

Handle `$/` messages gracefully

Fix handling of non-ASCII characters


## 0.13 - 16 Sep, 2019

Use `"AdditionalSources"` for Lints


## 0.12 - 5 Aug, 2019

Add some error handling for missing files and directories.

Add --extra argument for extra arguments to WolframKernel

Various bug fixes.


## 0.11 - 10 Jun, 2019

Added LSPServer paclet to CodeTools suite.

Various bug fixes.
