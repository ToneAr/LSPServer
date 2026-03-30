# LSPServer

LSPServer is a package that implements the
[Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
for Wolfram Language and allows a Wolfram Language kernel to run as an LSP server.

[Developing Wolfram Language Code in Other Editors and IDEs with LSP from WTC 2021: Watch Video (youtube)](https://www.youtube.com/watch?v=nXVEOUMZbzQ)

LSPServer implements several LSP features:
* Code diagnostics with workspace-wide analysis
* Suggestions for fixes (including quick-fix suppression actions)
* Formatting files and selections
* Semantic highlighting (including scoped variables in common math iterator forms)
* Expand / shrink selection
* Outline
* Color swatches
* Symbol references
* Documentation on hover with full context paths and alias-context mappings
* Smart autocompletion (system symbols, workspace symbols, association keys, context-qualified symbols, and alias-qualified package symbols)
* Workspace symbol search (cross-file go-to-definition)
* ESLint-style diagnostic suppression (`wl-disable` comments and `.wllintrc` config)
* Inlay hints (context annotations)
* Code folding for all multi-line constructs

This repo is for users who are interested in adding LSP support for Wolfram Language to LSP clients.

There are official Wolfram LSP clients for [Sublime Text](https://github.com/WolframResearch/Sublime-WolframLanguage) and [Visual Studio Code](https://github.com/WolframResearch/vscode-wolfram).


## Features

### Smart Autocompletion

LSPServer provides intelligent autocompletion from multiple sources:

- **System symbols**: All built-in Wolfram Language functions and constants
- **Workspace symbols**: Symbols defined in your project, with highest completion priority
- **External packages**: Symbols from packages loaded via `Needs[]` or `Get[]`, including alias-qualified package symbols
- **Kernel contexts**: Context-qualified symbols like `Internal`Bag` or `Developer`ToPackedArray`, discovered dynamically via `Contexts[]` and `Names[]`
- **Options**: Known options with automatic ` -> ` insertion
- **Context paths**: All available contexts when typing after a backtick, including alias contexts introduced by ``Needs["Package`" -> "Alias`"]``

#### Association Key Completion

When working with associations, LSPServer suggests keys based on the variable's known structure:

```wl
data = <|
	"name" -> "Alice",
	"age" -> 30,
	"address" -> <|
		"city" -> "NYC"
	|>
|>;
data[""              (* Suggests: "name", "age", "address" *)]
data["address", ""   (* Suggests: "city" *)]

items = {
	<|"id" -> 1|>,
	<|"id" -> 2|>
};
items[[              (* Suggests: 1, 2 *)]]
items[[1, ""         (* Suggests: "id" *)]]
```

Supports bracket-only triggers (`data[`), Part syntax (`data[[`), chained access, comma-separated paths, `All`/`Span` indexing, and per-element key tracking for lists of associations.

Package aliases also participate in completion. If you load a package with ``Needs["DatabaseLink`" -> "DBL`"]``, typing ``DBL` `` or an alias-qualified symbol resolves against the target package context.

### Diagnostic Suppression

Suppress diagnostics using ESLint-style comments:

```wl
x = undefinedVar  (* wl-disable-line UndefinedSymbol *)
```
```wl
(* wl-disable-next-line *)
y = anotherUndefined
```
```wl
(* wl-disable UndefinedSymbol *)
a = foo
b = bar
(* wl-enable UndefinedSymbol *)
```
```wl
(* wl-disable-file UnloadedContext *)
```

Project-wide configuration via `.wllintrc.json`:

```json
{
  "rules": {
    "UndefinedSymbol": "off",
    "Experimental*": "off"
  },
  "ignorePatterns": ["**/Tests/**"]
}
```

Quick-fix code actions are available on any diagnostic to insert suppression comments.

See [docs/ignore-patterns.md](docs/ignore-patterns.md) for full documentation.

### Enhanced Hover

Hovering over a symbol shows its full context path, usage message, and definition patterns:

- **System symbols**: Context, usage, and link to web documentation
- **User-defined symbols**: Context (from workspace index), in-file usage message, and definition patterns
- **External symbols**: Resolved context, usage, definition patterns from `DownValues`/`UpValues`/`SubValues`, and documentation link
- **Alias-qualified symbols**: When a package is loaded through an alias context, hover shows both the alias you typed and the resolved target context

### Semantic Highlighting

Semantic highlighting recognizes scoped and iterator variables in common mathematical forms such as `Integrate`, `NIntegrate`, `D`, `Solve`, `NSolve`, `Reduce`, `FindInstance`, `FindRoot`, `DSolve`, `NDSolve`, `Series`, `Residue`, and `Limit`.

### Workspace Intelligence

LSPServer indexes your entire workspace on startup:

- **Cross-file go-to-definition**: Jump to where any symbol is defined
- **Workspace symbol search**: Find symbols across all files (Ctrl+T / Cmd+T)
- **Dependency tracking**: Automatically detects and loads external package dependencies
- **Context-aware diagnostics**: Warns about unloaded contexts (but not for kernel-resident contexts like `` Internal` ``, `` Developer` ``, etc.)


## Setup

LSPServer depends on [CodeParser paclet](https://github.com/WolframResearch/codeparser), [CodeInspector paclet](https://github.com/WolframResearch/codeinspector), and [CodeFormatter paclet](https://github.com/WolframResearch/codeformatter).

LSPServer and its dependencies are included in Mathematica 13.0 and above.

Install LSPServer paclet and dependencies from the public paclet server:
```wl
PacletInstall["CodeParser"]
PacletInstall["CodeInspector"]
PacletInstall["CodeFormatter"]
PacletInstall["LSPServer"]
```

### Installing a Pre-Built Release

Pre-built `.paclet` archives for all platforms are attached to each [GitHub Release](../../releases). Download the archive for your platform and install directly:

```wl
PacletInstall["/path/to/LSPServer-2.9.28.paclet"]
```

### Building from Source

[Build and install the LSPServer paclet locally](HowToBuild.md)

Automated CI builds run on GitHub Actions for Linux, macOS, and Windows (x86-64 and ARM64). The build workflow is defined in [.github/workflows/build.yml](.github/workflows/build.yml).


## Using LSPServer

99% of users will not need to worry about using LSPServer directly. LSPServer is used internally when an LSP client launches a Wolfram kernel as an LSP server. This all happens in the background.

But it can be useful to run LSPServer when developing a new LSP client.

Create a file named server.wl:
```wl
Needs["LSPServer`"]

StartServer[]
```

And run from the command-line:
```
brenton@brenton2maclap % WolframKernel -noprompt -run Get\[\"server.wl\"\]
14:03:48.605 $CommandLine: {WolframKernel, -noprompt, -run, Get["server.wl"]}
14:03:48.607


14:03:48.608 $commProcess: StdIO
14:03:48.608


14:03:48.608 $ProcessID: 54603
14:03:48.609


14:03:48.609 $ParentProcessID: 54582
14:03:48.609


14:03:48.609 Starting server... (If this is the last line you see, then StartServer[] may have been called in an unexpected way and the server is hanging.)
14:03:48.610
```

Notice the proper character escapes on the command-line.

The kernel process is blocked waiting on input to its stdin.

Properly formed LSP JSON-RPC can be sent to the kernel, and the kernel would send its response to stdout.


## Troubleshooting

Make sure that the paclets can be found on your system:
```wl
Needs["LSPServer`"]
```

You may get `LibraryFunction` messages:
```
14:49:15.663 $CommandLine: {/Applications/Mathematica.app/Contents/MacOS/WolframKernel, -noinit, -noprompt, -nopaclet, -nostartuppaclets, -noicon, -run, Needs["LSPServer`"];LSPServer`StartServer["/Users/user/logs/"]}
14:49:15.664


14:49:15.664 $commProcess: StdIO
14:49:15.664


14:49:15.665 $ProcessID: 22400
14:49:15.665


14:49:15.666 $ParentProcessID: 22394
14:49:15.666


14:49:15.667 Directory[]: /private/var/folders/90/4fbnjdqx3f791xb65c02fm2m000bfy/T/Wolfram-LSPServer
14:49:15.667


14:49:15.668 Starting server... (If this is the last line you see, then StartServer[] may have been called in an unexpected way and the server is hanging.)
14:49:15.668



LibraryFunction::version: The version number 7 of the library is not consistent with the current or any previous WolframLibraryVersion.

LibraryFunction::initerr: A nonzero error code 7 was returned during the initialization of the library /Users/user/Library/Mathematica/Paclets/Repository/LSPServer-1.6/LibraryResources/MacOSX-x86-64/LSPServer.dylib.

LibraryFunction::libload: The function GetStartupError_LibraryLink was not loaded from the file /Users/user/Library/Mathematica/Paclets/Repository/LSPServer-1.6/LibraryResources/MacOSX-x86-64/LSPServer.dylib.
14:49:16.129


14:49:16.129 Initialization failed: Failure["LibraryFunctionLoad", <|"Result" -> $Failed|>]
14:49:16.484


14:49:16.485 Language Server kernel did not shutdown properly.
14:49:16.485
14:49:16.487 This is the command that was used:
14:49:16.488 {/Applications/Mathematica.app/Contents/MacOS/WolframKernel, -noinit, -noprompt, -nopaclet, -nostartuppaclets, -noicon, -run, Needs["LSPServer`"];LSPServer`StartServer["/Users/user/logs/"]}
14:49:16.488
14:49:16.489 To help diagnose the problem, run this in a notebook:
             Needs["LSPServer`"]
             LSPServer`RunServerDiagnostic[{"/Applications/Mathematica.app/Contents/MacOS/WolframKernel", "-noinit", "-noprompt", "-nopaclet", "-nostartuppaclets", "-noicon", "-run", "Needs[\"LSPServer`\"];LSPServer`StartServer[\"/Users/user/logs/\"]"}]
14:49:16.490
14:49:16.490 Fix any problems then restart and try again.
14:49:16.491


14:49:16.492 KERNEL IS EXITING HARD
14:49:16.492
```

This means that LSPServer was built with a newer version of Wolfram System than your system supports.

To fix this, build LSPServer from source with the version of Wolfram System that you will use.


### Server settings

Turn on debug logging from the kernel.

Give a string argument to StartServer[]. This is a directory that kernel logs will be written to.

```wl
Needs["LSPServer`"];LSPServer`StartServer["/path/to/log/directory/"]
```
