(* ::Package:: *)

BeginPackage["LSPServer`PacletIndex`"]

(*
Paclet-wide Symbol Index

This module provides workspace-wide symbol tracking for the LSP server.
It indexes all symbol definitions and usages across files in the workspace,
enabling features like:
- Cross-file go-to-definition
- Workspace-wide symbol search
- Paclet-aware autocomplete
- Cross-file references
*)

InitializePacletIndex
GetPacletSymbols
GetSymbolDefinitions
GetSymbolReferences
GetSymbolsForCompletion
UpdateFileIndex
RemoveFileFromIndex
GetAllWorkspaceSymbols
SearchWorkspaceSymbols
GetSymbolContext
GetPublicSymbols
GetContextSymbols
IsSymbolPublic
GetDependencyContexts
IsDependencyContext
GetFileContextLoads
GetFileExplicitContextRefs
IsContextLoadedInFile
GetContextLoadErrors
GetFileLoadedContexts
GetKernelContextsCached
GetSymbolUsages
GetContextAliases
ProcessPendingIndexFiles
GetLoadedDependencySymbols

$PacletIndex
$WorkspaceRoot
$PendingIndexFiles

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


(*
$PacletIndex structure:
<|
  "Symbols" -> <|
    "symbolName" -> <|
      "Definitions" -> {
        <| "uri" -> "file:///...", "source" -> {{line, col}, {line, col}}, "kind" -> "function"|"constant"|"option"|"attribute", "context" -> "Context`" |>,
        ...
      },
      "References" -> {
        <| "uri" -> "file:///...", "source" -> {{line, col}, {line, col}} |>,
        ...
      },
      "Usages" -> { ... usage strings from ::usage ... }
    |>,
    ...
  |>,
  "Files" -> <|
    "file:///path/to/file.wl" -> <|
      "LastIndexed" -> DateObject[...],
      "Symbols" -> {"sym1", "sym2", ...},
      "Dependencies" -> {"Dep1`", "Dep2`", ...},  (* Contexts loaded via BeginPackage deps *)
      "ContextLoads" -> {  (* Ordered list of context loading statements *)
        <| "context" -> "Developer`", "source" -> {{line, col}, {line, col}}, "method" -> "Needs"|"Get"|"BeginPackage" |>,
        ...
      },
      "ExplicitContextRefs" -> {  (* References to symbols with explicit context like Developer`ToPackedArray *)
        <| "symbol" -> "ToPackedArray", "context" -> "Developer`", "fullName" -> "Developer`ToPackedArray", "source" -> {{line, col}, {line, col}} |>,
        ...
      },
      "PackageContext" -> "MyPackage`" | None  (* The main package context if this is a package file *)
    |>,
    ...
  |>,
  "Contexts" -> <|
    "MyPackage`" -> {"symbol1", "symbol2", ...},
    "MyPackage`Private`" -> {...},
    ...
  |>,
  "Dependencies" -> {"Developer`", "GeneralUtilities`", ...},  (* All dependency contexts used in workspace *)
  "ContextAliases" -> <| "Alias`" -> "FullContext`", ... |>  (* alias context -> full context, from Needs["Full`" -> "Alias`"] *)
|>
*)

$PacletIndex = <|
  "Symbols" -> <||>,
  "Files" -> <||>,
  "Contexts" -> <||>,
  "Dependencies" -> {},
  "ContextAliases" -> <||>
|>

$WorkspaceRoot = None

(*
Files queued for background indexing (populated by InitializePacletIndex,
consumed by ProcessPendingIndexFiles during idle loop iterations)
*)
$PendingIndexFiles = {}

(*
Cached set of non-API symbol names for fast exclusion in extractPublicDeclarations
*)
$nonAPISymbols = <|
  "Begin" -> True, "End" -> True, "BeginPackage" -> True, "EndPackage" -> True,
  "Needs" -> True, "Get" -> True, "Set" -> True, "SetDelayed" -> True,
  "MessageName" -> True, "String" -> True, "List" -> True, "Rule" -> True,
  "CompoundExpression" -> True, "Null" -> True
|>


(*
Initialize the paclet index by scanning the workspace
*)
(*
Patterns for directories to exclude from indexing
*)
$ExcludedDirectoryPatterns = {"build*", "node_modules", ".git", "__pycache__", "*.egg-info"}

(*
Check if a file path should be excluded based on directory patterns
*)
shouldExcludeFile[filePath_String] :=
Module[{pathParts},
  pathParts = FileNameSplit[filePath];
  AnyTrue[pathParts, Function[{part},
    AnyTrue[$ExcludedDirectoryPatterns, StringMatchQ[part, #]&]
  ]]
]

InitializePacletIndex[workspaceRoot_String] :=
Module[{files, filteredFiles},

  If[$Debug2,
    log["InitializePacletIndex: starting for ", workspaceRoot]
  ];

  $WorkspaceRoot = workspaceRoot;

  (*
  Reset the index
  *)
  $PacletIndex = <|
    "Symbols" -> <||>,
    "Files" -> <||>,
    "Contexts" -> <||>,
    "Dependencies" -> {}
  |>;

  (*
  Find all Wolfram Language files in the workspace
  *)
  files = FileNames[{"*.wl", "*.m", "*.wls"}, workspaceRoot, Infinity];

  (*
  Filter out files in excluded directories (build*, node_modules, .git, etc.)
  *)
  filteredFiles = Select[files, !shouldExcludeFile[#]&];

  If[$Debug2,
    log["InitializePacletIndex: found ", Length[files], " files, ", Length[filteredFiles], " after filtering"]
  ];

  (*
  Queue files for background indexing so the initialized handler can return
  immediately and the server can begin accepting requests
  *)
  $PendingIndexFiles = filteredFiles;

  If[$Debug2,
    log["InitializePacletIndex: queued ", Length[$PendingIndexFiles], " files for background indexing"]
  ];

  $PacletIndex
]


(*
Process a batch of pending index files.
Called by ProcessScheduledJobs during idle loop iterations so that workspace
indexing happens in the background without blocking LSP responses.
Returns True if there are still files remaining to index.
*)
ProcessPendingIndexFiles[] :=
Module[{n, batch},
  If[Length[$PendingIndexFiles] == 0,
    Return[False]
  ];
  n = Min[5, Length[$PendingIndexFiles]];
  batch = $PendingIndexFiles[[;;n]];
  $PendingIndexFiles = $PendingIndexFiles[[n + 1 ;;]];
  Scan[indexFile, batch];
  If[$Debug2,
    log["ProcessPendingIndexFiles: indexed batch of ", n, ", ", Length[$PendingIndexFiles], " files remaining"]
  ];
  Length[$PendingIndexFiles] > 0
]


(*
Index a single file
*)
indexFile[filePath_String] :=
Catch[
Module[{text, cst, ast, uri, symbols, definitions, usages, fileSymbols, fileDeps,
  contextLoads, explicitContextRefs, packageContext, fileAliases},

  If[$Debug2,
    log["indexFile: ", filePath]
  ];

  (*
  Read the file
  *)
  text = Quiet[Import[filePath, "Text"]];

  If[!StringQ[text],
    If[$Debug2, log["indexFile: failed to read file"]];
    Throw[Null]
  ];

  (*
  Parse the file
  *)
  cst = Quiet[CodeConcreteParse[text]];

  If[FailureQ[cst],
    If[$Debug2, log["indexFile: failed to parse file"]];
    Throw[Null]
  ];

  (*
  Abstract the CST to get definitions
  *)
  ast = Quiet[CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[cst]]];

  If[FailureQ[ast],
    If[$Debug2, log["indexFile: failed to abstract file"]];
    Throw[Null]
  ];

  uri = "file://" <> filePath;

  (*
  Extract definitions from the AST
  *)
  definitions = extractDefinitions[ast, uri];

  (*
  Extract usage messages
  *)
  usages = extractUsages[ast, uri];

  (*
  Extract symbol references
  *)
  symbols = extractSymbolReferences[cst, uri];

  (*
  Extract package dependencies (from BeginPackage and Needs)
  *)
  fileDeps = extractDependencies[ast];

  (*
  Extract detailed context loading information
  *)
  contextLoads = extractContextLoads[ast];

  (*
  Extract explicit context references (e.g., Developer`ToPackedArray)
  *)
  explicitContextRefs = extractExplicitContextRefs[cst];

  (*
  Extract the main package context if this is a package file
  *)
  packageContext = extractPackageContext[ast];

  (*
  Extract context alias mappings from Needs["Full`" -> "Alias`"] calls
  *)
  fileAliases = extractContextAliases[ast];

  (*
  Add dependencies to global list and load external packages
  *)
  If[Length[fileDeps] > 0,
    $PacletIndex["Dependencies"] = DeleteDuplicates[
      Join[$PacletIndex["Dependencies"], fileDeps]
    ];
    If[$Debug2,
      log["indexFile: found dependencies: ", fileDeps]
    ];
    (*
    Load external dependency packages so their symbols are available
    for context detection. Skip workspace-internal packages.
    *)
    loadExternalDependencies[fileDeps]
  ];

  (*
  Update the index using batch operations to avoid quadratic AppendTo
  *)
  addFileToIndex[uri, definitions, usages, symbols, fileDeps, contextLoads, explicitContextRefs, packageContext, fileAliases];
]]


(*
Batch add a file's data to the PacletIndex.
Uses grouped-by-name operations instead of per-item AppendTo to avoid O(n^2).
*)
addFileToIndex[uri_, definitions_, usages_, symbols_, fileDeps_, contextLoads_, explicitContextRefs_, packageContext_, fileAliases_:{}] :=
Module[{fileSymbolsBag, defsByName, usagesByName, refsByName},

  fileSymbolsBag = Internal`Bag[];

  (*
  Group definitions by symbol name for batch insert
  *)
  defsByName = GroupBy[definitions, #["name"]&];

  KeyValueMap[
    Function[{name, defs},
      Internal`StuffBag[fileSymbolsBag, name];

      If[!KeyExistsQ[$PacletIndex["Symbols"], name],
        $PacletIndex["Symbols", name] = <|
          "Definitions" -> {},
          "References" -> {},
          "Usages" -> {}
        |>
      ];

      (* Batch append all definitions for this symbol at once *)
      $PacletIndex["Symbols", name, "Definitions"] = Join[
        $PacletIndex["Symbols", name, "Definitions"],
        KeyDrop[#, "name"]& /@ defs
      ];

      (* Track by context — use first definition's context *)
      Module[{ctx},
        ctx = defs[[1]]["context"];
        If[StringQ[ctx],
          If[!KeyExistsQ[$PacletIndex["Contexts"], ctx],
            $PacletIndex["Contexts", ctx] = {}
          ];
          If[!MemberQ[$PacletIndex["Contexts", ctx], name],
            $PacletIndex["Contexts", ctx] = Append[$PacletIndex["Contexts", ctx], name]
          ]
        ]
      ]
    ],
    defsByName
  ];

  (*
  Group usages by symbol name for batch insert
  *)
  usagesByName = GroupBy[usages, #["name"]&];

  KeyValueMap[
    Function[{name, usageList},
      If[KeyExistsQ[$PacletIndex["Symbols"], name],
        Module[{existingUsages, newUsages},
          existingUsages = $PacletIndex["Symbols", name, "Usages"];
          newUsages = DeleteDuplicates[Join[existingUsages, #["usage"]& /@ usageList]];
          $PacletIndex["Symbols", name, "Usages"] = newUsages
        ]
      ]
    ],
    usagesByName
  ];

  (*
  Group references by symbol name for batch insert
  *)
  refsByName = GroupBy[symbols, #["name"]&];

  KeyValueMap[
    Function[{name, refs},
      If[!KeyExistsQ[$PacletIndex["Symbols"], name],
        $PacletIndex["Symbols", name] = <|
          "Definitions" -> {},
          "References" -> {},
          "Usages" -> {}
        |>
      ];

      (* Batch append all references for this symbol at once *)
      $PacletIndex["Symbols", name, "References"] = Join[
        $PacletIndex["Symbols", name, "References"],
        KeyDrop[#, "name"]& /@ refs
      ]
    ],
    refsByName
  ];

  (*
  Track file in index with enhanced context information
  *)
  $PacletIndex["Files", uri] = <|
    "LastIndexed" -> Now,
    "Symbols" -> DeleteDuplicates[Internal`BagPart[fileSymbolsBag, All]],
    "Dependencies" -> fileDeps,
    "ContextLoads" -> contextLoads,
    "ExplicitContextRefs" -> explicitContextRefs,
    "PackageContext" -> packageContext,
    "ContextAliases" -> fileAliases
  |>;

  (*
  Merge file aliases into the global ContextAliases map (alias -> fullContext).
  *)
  Scan[
    Function[{rec},
      $PacletIndex["ContextAliases", rec["alias"]] = rec["fullContext"]
    ],
    fileAliases
  ];
]


(*
Extract definitions from AST
*)
extractDefinitions[ast_, uri_] :=
Module[{definitions},
  definitions = Internal`Bag[];
  walkASTForDefinitions[definitions, ast, None, False, uri];
  Internal`BagPart[definitions, All]
]


(*
Extract package dependencies from AST.
BeginPackage["MyPackage`", {"Dep1`", "Dep2`"}] creates a PackageNode with
tag = {LeafNode[String, "MyPackage`", ...], CallNode[List, {LeafNode[String, "Dep1`", ...], ...}, ...]}

Also extracts Needs["Package`"] calls at the top level.
*)
extractDependencies[ast_] :=
Module[{packageDeps, needsDeps, needsAliasDeps, allDeps},

  (*
  Extract from PackageNode tags - dependencies are in the List following the main context
  *)
  packageDeps = Cases[ast,
    PackageNode[{LeafNode[String, _, _], CallNode[LeafNode[Symbol, "List", _], deps_List, _], ___}, _, _] :>
      Cases[deps, LeafNode[String, s_String, _] :> abstractContextString[s]],
    {0, 2}
  ] // Flatten;

  (*
  Also extract from Needs["Package`"] and Get["Package`"] calls anywhere in the file.
  These may be at top level, inside PackageNode, inside ContextNode (Private), etc.
  *)
  needsDeps = Cases[ast,
    CallNode[LeafNode[Symbol, "Needs" | "Get", _], {LeafNode[String, s_String, _], ___}, _] :>
      abstractContextString[s],
    Infinity
  ];

  (*
  Also extract the full context from the aliased form: Needs["FullContext`" -> "Alias`"]
  The full context string is the left-hand side of the Rule.
  After Abstract, -> becomes CallNode[LeafNode[Symbol, "Rule", _], ...].
  *)
  needsAliasDeps = Cases[ast,
    CallNode[LeafNode[Symbol, "Needs", _],
      {CallNode[LeafNode[Symbol, "Rule", _], {LeafNode[String, s_String, _], LeafNode[String, _, _]}, _]}, _] :>
      abstractContextString[s],
    Infinity
  ];

  allDeps = DeleteDuplicates[Join[packageDeps, needsDeps, needsAliasDeps]];

  (* Filter out None values from failed parsing *)
  Select[allDeps, StringQ]
]


(*
Extract detailed context loading information from AST.
Returns a list of context load records with source locations.
*)
extractContextLoads[ast_] :=
Module[{loads, packageLoads, needsLoads, needsAliasLoads, getLoads, packageContext},
  loads = {};

  (*
  Extract BeginPackage context and its dependencies
  *)
  packageLoads = Cases[ast,
    PackageNode[{LeafNode[String, ctx_String, KeyValuePattern[Source -> src_]], rest___}, _, _] :>
      Module[{mainCtx, deps},
        mainCtx = abstractContextString[ctx];
        (* The main package context is implicitly "loaded" *)
        deps = Cases[{rest},
          CallNode[LeafNode[Symbol, "List", _], depList_List, _] :>
            Cases[depList, LeafNode[String, s_String, KeyValuePattern[Source -> depSrc_]] :>
              <| "context" -> abstractContextString[s], "source" -> depSrc, "method" -> "BeginPackage" |>
            ],
          {1}
        ] // Flatten;
        (* Return both the main context and dependencies *)
        Prepend[deps, <| "context" -> mainCtx, "source" -> src, "method" -> "BeginPackage" |>]
      ],
    {0, 2}
  ] // Flatten;

  (*
  Extract Needs["Package`"] calls with source locations
  *)
  needsLoads = Cases[ast,
    CallNode[LeafNode[Symbol, "Needs", _], {LeafNode[String, s_String, _], ___}, KeyValuePattern[Source -> src_]] :>
      <| "context" -> abstractContextString[s], "source" -> src, "method" -> "Needs" |>,
    Infinity
  ];

  (*
  Extract aliased Needs["FullContext`" -> "Alias`"] calls with source locations.
  Record the full context as the loaded context, and include the alias for reference.
  After Abstract, -> becomes CallNode[LeafNode[Symbol, "Rule", _], ...].
  *)
  needsAliasLoads = Cases[ast,
    CallNode[LeafNode[Symbol, "Needs", _],
      {CallNode[LeafNode[Symbol, "Rule", _], {LeafNode[String, s_String, _], LeafNode[String, a_String, _]}, _]},
      KeyValuePattern[Source -> src_]] :>
      <| "context" -> abstractContextString[s], "alias" -> abstractContextString[a], "source" -> src, "method" -> "Needs" |>,
    Infinity
  ];

  needsLoads = Join[needsLoads, needsAliasLoads];

  (*
  Extract Get["Package`"] calls with source locations
  *)
  getLoads = Cases[ast,
    CallNode[LeafNode[Symbol, "Get", _], {LeafNode[String, s_String, _], ___}, KeyValuePattern[Source -> src_]] :>
      <| "context" -> abstractContextString[s], "source" -> src, "method" -> "Get" |>,
    Infinity
  ];

  loads = Join[packageLoads, needsLoads, getLoads];

  (* Filter out None values and sort by source location *)
  loads = Select[loads, StringQ[#["context"]] &];
  loads = SortBy[loads, #["source"][[1]] &];

  loads
]


(*
Extract context alias mappings from AST.
Handles the form Needs["FullContext`" -> "Alias`"] which sets up $ContextAliases.
Returns a list of associations: <| "fullContext" -> "FullContext`", "alias" -> "Alias`" |>
*)
extractContextAliases[ast_] :=
Module[{aliases},
  (* After Abstract, -> becomes CallNode[LeafNode[Symbol, "Rule", _], ...] *)
  aliases = Cases[ast,
    CallNode[LeafNode[Symbol, "Needs", _],
      {CallNode[LeafNode[Symbol, "Rule", _], {LeafNode[String, fullCtx_String, _], LeafNode[String, aliasCtx_String, _]}, _]},
      _] :>
      <| "fullContext" -> abstractContextString[fullCtx], "alias" -> abstractContextString[aliasCtx] |>,
    Infinity
  ];
  Select[aliases, StringQ[#["fullContext"]] && StringQ[#["alias"]] &]
]


(*
Extract explicit context references from CST.
These are symbols written with explicit context like Developer`ToPackedArray.
Returns list of records with symbol name, context, and source.
*)
extractExplicitContextRefs[cst_] :=
Module[{refs},
  (*
  Find all symbol tokens that contain a backtick (explicit context)
  *)
  refs = Cases[cst,
    LeafNode[Symbol, name_String /; StringContainsQ[name, "`"], KeyValuePattern[Source -> src_]] :>
      Module[{parts, ctx, sym},
        parts = StringSplit[name, "`"];
        If[Length[parts] >= 2,
          (* Context is everything except the last part *)
          ctx = StringJoin[Riffle[Most[parts], "`"]] <> "`";
          sym = Last[parts];
          <| "symbol" -> sym, "context" -> ctx, "fullName" -> name, "source" -> src |>,
          Nothing
        ]
      ],
    Infinity
  ];

  refs
]


(*
Extract the main package context from AST if this is a package file.
*)
extractPackageContext[ast_] :=
Module[{packageContexts},
  packageContexts = Cases[ast,
    PackageNode[{LeafNode[String, ctx_String, _], ___}, _, _] :> abstractContextString[ctx],
    {0, 2}
  ];

  If[Length[packageContexts] > 0,
    First[packageContexts],
    None
  ]
]


(*
Load external dependency packages so their symbols are available for context detection.
Skips packages that appear to be workspace-internal (defined in the workspace).
Only loads packages that are likely system/external packages.
*)
loadExternalDependencies[deps_List] :=
Module[{externalDeps, workspaceContexts},

  (*
  Get contexts that are defined within the workspace - don't try to load these
  *)
  workspaceContexts = Keys[$PacletIndex["Contexts"]];

  (*
  Filter to external dependencies:
  - Not in workspace contexts
  - Not already a subcontext of a workspace context
  *)
  externalDeps = Select[deps,
    Function[{dep},
      And[
        !MemberQ[workspaceContexts, dep],
        !AnyTrue[workspaceContexts, StringStartsQ[dep, #] &]
      ]
    ]
  ];

  (*
  Try to load each external dependency
  *)
  Scan[
    Function[{dep},
      If[$Debug2,
        log["loadExternalDependencies: loading ", dep]
      ];
      Quiet[
        Check[
          Needs[dep],
          If[$Debug2, log["loadExternalDependencies: failed to load ", dep]],
          {Needs::nocont, Get::noopen}
        ],
        {Needs::nocont, Get::noopen, General::stop}
      ]
    ],
    externalDeps
  ]
]


(*
Walk AST to extract definitions
Arguments:
  bag - Internal`Bag to collect definitions
  node - current AST node
  currentContext - current context string (e.g., "MyPackage`")
  inPrivate - True if we're inside a Private` context (Begin["`Private`"])
  uri - file URI
*)
walkASTForDefinitions[bag_, node_, currentContext_, inPrivate_, uri_] :=
Module[{newContext, contextStrings, newInPrivate, packageContext},

  newContext = currentContext;
  newInPrivate = inPrivate;

  (*
  Track context from PackageNode/ContextNode
  The AST abstracts BeginPackage/EndPackage into PackageNode and Begin/End into ContextNode
  The context is stored in the first element as a list of LeafNode[String, ...]
  *)
  Which[
    MatchQ[node, PackageNode[{LeafNode[String, _, _], ___}, _, _]],
      (* Extract context from PackageNode tag - first element is a list of context strings *)
      contextStrings = Cases[node[[1]], LeafNode[String, s_String, _] :> abstractContextString[s]];
      If[Length[contextStrings] > 0,
        newContext = contextStrings[[1]];
        (* Reset private flag when entering a new package *)
        newInPrivate = False;

        (*
        Extract public symbol declarations from the package
        These are symbols that appear between BeginPackage and Begin["`Private`"]
        They are the exported API of the package
        *)
        extractPublicDeclarations[bag, node, newContext, uri]
      ]
    ,
    MatchQ[node, ContextNode[{LeafNode[String, _, _], ___}, _, _]],
      (* ContextNode represents Begin["`Private`"] etc. *)
      contextStrings = Cases[node[[1]], LeafNode[String, s_String, _] :> abstractContextString[s]];
      If[Length[contextStrings] > 0,
        Module[{subContext, baseContext},
          subContext = contextStrings[[1]];
          newContext = If[StringQ[currentContext],
            (* Join contexts properly *)
            (* e.g., "MyPackage`" + "`Private`" -> "MyPackage`Private`" *)
            If[StringStartsQ[subContext, "`"],
              (* Relative context: remove trailing ` from current, then append sub *)
              baseContext = If[StringEndsQ[currentContext, "`"],
                StringDrop[currentContext, -1],
                currentContext
              ];
              baseContext <> subContext,
              (* Absolute context: use as-is *)
              subContext
            ],
            subContext
          ];
          (* Check if entering Private context *)
          If[StringContainsQ[subContext, "Private"],
            newInPrivate = True
          ]
        ]
      ]
  ];

  (*
  Extract definitions from Set/SetDelayed
  Note: Pattern variables in MatchQ don't bind, so we extract values directly with [[2]]
  *)
  Which[
    (*
    Function definition: f[x_] := ...
    *)
    MatchQ[node, CallNode[LeafNode[Symbol, "SetDelayed" | "Set", _], {CallNode[_, _, _], _}, KeyValuePattern["Definitions" -> _List]]],
      Scan[
        Function[{def},
          If[MatchQ[def, LeafNode[Symbol, _String, _]],
            Internal`StuffBag[bag, <|
              "name" -> def[[2]],
              "uri" -> uri,
              "source" -> node[[3, Key[Source]]],
              "kind" -> "function",
              "context" -> newContext,
              "visibility" -> If[newInPrivate, "private", "public"]
            |>]
          ]
        ],
        node[[3, Key["Definitions"]]]
      ]
    ,
    (*
    Constant definition: c = 5
    *)
    MatchQ[node, CallNode[LeafNode[Symbol, "Set", _], {LeafNode[Symbol, _, _], _}, KeyValuePattern["Definitions" -> _List]]],
      Scan[
        Function[{def},
          If[MatchQ[def, LeafNode[Symbol, _String, _]],
            Internal`StuffBag[bag, <|
              "name" -> def[[2]],
              "uri" -> uri,
              "source" -> node[[3, Key[Source]]],
              "kind" -> "constant",
              "context" -> newContext,
              "visibility" -> If[newInPrivate, "private", "public"]
            |>]
          ]
        ],
        node[[3, Key["Definitions"]]]
      ]
    ,
    (*
    Options definition: Options[f] = {...}
    *)
    MatchQ[node, CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _],
      {CallNode[LeafNode[Symbol, "Options", _], {LeafNode[Symbol, _String, _]}, _], _}, _]],
      Internal`StuffBag[bag, <|
        "name" -> node[[2, 1, 2, 1, 2]],
        "uri" -> uri,
        "source" -> node[[3, Key[Source]]],
        "kind" -> "option",
        "context" -> newContext,
        "visibility" -> If[newInPrivate, "private", "public"]
      |>]
    ,
    (*
    Attributes definition: Attributes[f] = {...}
    *)
    MatchQ[node, CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _],
      {CallNode[LeafNode[Symbol, "Attributes", _], {LeafNode[Symbol, _String, _]}, _], _}, _]],
      Internal`StuffBag[bag, <|
        "name" -> node[[2, 1, 2, 1, 2]],
        "uri" -> uri,
        "source" -> node[[3, Key[Source]]],
        "kind" -> "attribute",
        "context" -> newContext,
        "visibility" -> If[newInPrivate, "private", "public"]
      |>]
  ];

  (*
  Recurse into children
  Note: Switch patterns do NOT bind variables, so we must use node[[2]] directly
  *)
  Switch[node,
    ContainerNode[_, _List, _],
      Scan[walkASTForDefinitions[bag, #, newContext, newInPrivate, uri]&, node[[2]]]
    ,
    PackageNode[_, _List, _],
      Scan[walkASTForDefinitions[bag, #, newContext, newInPrivate, uri]&, node[[2]]]
    ,
    ContextNode[_, _List, _],
      Scan[walkASTForDefinitions[bag, #, newContext, newInPrivate, uri]&, node[[2]]]
    ,
    CallNode[_, _List, _],
      Scan[walkASTForDefinitions[bag, #, newContext, newInPrivate, uri]&, node[[2]]]
    ,
    _[_, _List, _],
      Scan[walkASTForDefinitions[bag, #, newContext, newInPrivate, uri]&, node[[2]]]
  ]
]


(*
Extract public symbol declarations from a PackageNode
These are symbols that appear between BeginPackage and Begin["`Private`"]
Symbols declared here form the public API of the package

Pattern:
  BeginPackage["MyPackage`"]

  myFunction::usage = "..."   <- public declaration
  myConstant                  <- public declaration (just mentioning creates it)

  Begin["`Private`"]
  ...
  End[]
  EndPackage[]
*)
extractPublicDeclarations[bag_, packageNode_, packageContext_, uri_] :=
Module[{children, publicSection, publicSymbols, privateStart},

  children = packageNode[[2]];

  (*
  Find the index of the first ContextNode (Begin["`Private`"])
  Everything before that is public declarations
  *)
  privateStart = FirstPosition[children,
    ContextNode[{LeafNode[String, s_String, _], ___}, _, _] /;
      StringContainsQ[Quiet[ToExpression[s]], "Private"],
    {Length[children] + 1},
    {1}
  ][[1]];

  (*
  Get the public section (before Private)
  *)
  publicSection = Take[children, privateStart - 1];

  (*
  Find all symbol references in the public section
  These are the declared public API
  *)
  publicSymbols = Cases[publicSection,
    LeafNode[Symbol, name_String, KeyValuePattern[Source -> src_]] /;
      (* Exclude common non-API symbols - use Association for O(1) lookup *)
      !KeyExistsQ[$nonAPISymbols, name],
    Infinity
  ];

  (*
  Add each public symbol as a declaration
  *)
  Scan[
    Function[{sym},
      Module[{name, src},
        name = sym[[2]];
        src = sym[[3, Key[Source]]];

        Internal`StuffBag[bag, <|
          "name" -> name,
          "uri" -> uri,
          "source" -> src,
          "kind" -> "declaration",
          "context" -> packageContext,
          "visibility" -> "public"
        |>]
      ]
    ],
    (* Remove duplicates by name, keep first occurrence *)
    DeleteDuplicatesBy[publicSymbols, #[[2]]&]
  ]
]


(*
Convert a quoted string from the AST (e.g., "\"Developer`\"") to an actual string.
We strip the outer quotes directly instead of using ToExpression, which would
convert context strings like "Developer`" into symbols.
*)
abstractContextString[str_String /; StringStartsQ[str, "\""]] :=
  StringTake[str, {2, -2}]  (* Remove first and last quote characters *)

abstractContextString[_] := None


(*
Extract usage messages from AST
*)
extractUsages[ast_, uri_] :=
Module[{usages},
  usages = Cases[ast,
    CallNode[
      LeafNode[Symbol, "Set" | "SetDelayed", _],
      {
        CallNode[
          LeafNode[Symbol, "MessageName", _],
          {
            LeafNode[Symbol, name_String, _],
            LeafNode[String, "\"usage\"", _],
            ___
          },
          _
        ],
        LeafNode[String, msg_String, _]
      },
      _
    ] :> <| "name" -> name, "usage" -> Quiet[ToExpression[msg], Syntax::stresc] |>,
    Infinity
  ];

  Select[usages, StringQ[#["usage"]]&]
]


(*
Extract symbol references from CST
*)
extractSymbolReferences[cst_, uri_] :=
Module[{symbols},
  symbols = Cases[cst,
    LeafNode[Symbol, name_String, KeyValuePattern[Source -> src_]] :> <|
      "name" -> name,
      "uri" -> uri,
      "source" -> src
    |>,
    Infinity
  ];

  symbols
]


(*
Update the index for a single file when it changes
*)
UpdateFileIndex[uri_String, text_String] :=
Catch[
Module[{cst, ast, filePath, oldSymbols, definitions, usages, symbols, fileSymbols, fileDeps,
  contextLoads, explicitContextRefs, packageContext, fileAliases},

  (*
  Skip files in excluded directories
  *)
  filePath = StringReplace[uri, "file://" -> ""];
  If[shouldExcludeFile[filePath],
    If[$Debug2,
      log["UpdateFileIndex: skipping excluded file: ", uri]
    ];
    Throw[Null]
  ];

  If[$Debug2,
    log["UpdateFileIndex: ", uri]
  ];

  (*
  Remove old entries for this file
  *)
  RemoveFileFromIndex[uri];

  (*
  Parse the new content
  *)
  cst = Quiet[CodeConcreteParse[text]];

  If[FailureQ[cst],
    If[$Debug2, log["UpdateFileIndex: failed to parse"]];
    Throw[Null]
  ];

  ast = Quiet[CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[cst]]];

  If[FailureQ[ast],
    If[$Debug2, log["UpdateFileIndex: failed to abstract"]];
    Throw[Null]
  ];

  (*
  Extract and add new entries
  *)
  definitions = extractDefinitions[ast, uri];
  usages = extractUsages[ast, uri];
  symbols = extractSymbolReferences[cst, uri];

  (*
  Extract package dependencies and load external ones
  *)
  fileDeps = extractDependencies[ast];
  If[Length[fileDeps] > 0,
    $PacletIndex["Dependencies"] = DeleteDuplicates[
      Join[$PacletIndex["Dependencies"], fileDeps]
    ];
    loadExternalDependencies[fileDeps]
  ];

  (*
  Extract detailed context loading information
  *)
  contextLoads = extractContextLoads[ast];

  (*
  Extract explicit context references (e.g., Developer`ToPackedArray)
  *)
  explicitContextRefs = extractExplicitContextRefs[cst];

  (*
  Extract the main package context if this is a package file
  *)
  packageContext = extractPackageContext[ast];

  (*
  Extract context alias mappings from Needs["Full`" -> "Alias`"] calls
  *)
  fileAliases = extractContextAliases[ast];

  (*
  Update the index using batch operations to avoid quadratic AppendTo
  *)
  addFileToIndex[uri, definitions, usages, symbols, fileDeps, contextLoads, explicitContextRefs, packageContext, fileAliases];
]]


(*
Remove a file from the index
*)
RemoveFileFromIndex[uri_String] :=
Module[{fileEntry, fileSymbols},

  fileEntry = Lookup[$PacletIndex["Files"], uri, Null];

  If[fileEntry === Null,
    Return[Null]
  ];

  fileSymbols = fileEntry["Symbols"];

  (*
  Remove definitions and references from this file
  *)
  Scan[
    Function[{symName},
      If[KeyExistsQ[$PacletIndex["Symbols"], symName],
        $PacletIndex["Symbols", symName, "Definitions"] =
          DeleteCases[$PacletIndex["Symbols", symName, "Definitions"], KeyValuePattern["uri" -> uri]];
        $PacletIndex["Symbols", symName, "References"] =
          DeleteCases[$PacletIndex["Symbols", symName, "References"], KeyValuePattern["uri" -> uri]];

        (*
        Remove symbol entry if empty
        *)
        If[$PacletIndex["Symbols", symName, "Definitions"] === {} &&
           $PacletIndex["Symbols", symName, "References"] === {} &&
           $PacletIndex["Symbols", symName, "Usages"] === {},
          $PacletIndex["Symbols", symName] =.
        ]
      ]
    ],
    fileSymbols
  ];

  (*
  Remove file entry
  *)
  $PacletIndex["Files", uri] =.;

  (*
  Recompute global Dependencies from remaining files to prevent stale entries
  *)
  $PacletIndex["Dependencies"] = DeleteDuplicates[
    Flatten[Lookup[Values[$PacletIndex["Files"]], "Dependencies", {}]]
  ];

  (*
  Recompute global ContextAliases from remaining files
  *)
  $PacletIndex["ContextAliases"] = Association[
    Flatten[
      Map[
        Function[{rec}, rec["alias"] -> rec["fullContext"]],
        Flatten[Lookup[Values[$PacletIndex["Files"]], "ContextAliases", {}]]
      ]
    ]
  ];

  (*
  Clean up Contexts: remove symbol names that no longer have any definitions
  *)
  Scan[
    Function[{ctx},
      $PacletIndex["Contexts", ctx] = Select[
        $PacletIndex["Contexts", ctx],
        KeyExistsQ[$PacletIndex["Symbols"], #]&
      ];
      If[$PacletIndex["Contexts", ctx] === {},
        $PacletIndex["Contexts", ctx] =.
      ]
    ],
    Keys[$PacletIndex["Contexts"]]
  ];
]


(*
Get all definitions for a symbol
*)
GetSymbolDefinitions[symbolName_String] :=
  Lookup[$PacletIndex["Symbols"], symbolName, <||>]["Definitions"] // Replace[_Missing -> {}]


(*
Get all references for a symbol
*)
GetSymbolReferences[symbolName_String] :=
  Lookup[$PacletIndex["Symbols"], symbolName, <||>]["References"] // Replace[_Missing -> {}]


(*
Get symbols for completion with optional prefix filter
*)
GetSymbolsForCompletion[prefix_String:""] :=
Module[{allSymbols, filtered},
  allSymbols = Keys[$PacletIndex["Symbols"]];

  If[prefix === "",
    filtered = allSymbols,
    filtered = Select[allSymbols, StringStartsQ[#, prefix, IgnoreCase -> True]&]
  ];

  (*
  Return symbols with their kind and usage info
  *)
  Association /@ Table[
    Module[{symData, defs, kind, usage},
      symData = $PacletIndex["Symbols", sym];
      defs = symData["Definitions"];
      kind = If[Length[defs] > 0, defs[[1]]["kind"], "unknown"];
      usage = If[Length[symData["Usages"]] > 0, symData["Usages"][[1]], None];

      <|
        "name" -> sym,
        "kind" -> kind,
        "usage" -> usage,
        "definitionCount" -> Length[defs]
      |>
    ],
    {sym, filtered}
  ]
]


(*
Get all paclet symbols (those defined in the workspace)
*)
GetPacletSymbols[] :=
Module[{},
  Select[
    Keys[$PacletIndex["Symbols"]],
    Length[$PacletIndex["Symbols", #, "Definitions"]] > 0 &
  ]
]


(*
Get short-name symbols from dependency contexts that are actually loaded in the kernel.
Used by UndefinedSymbol diagnostics to suppress false positives for symbols
from BeginPackage/Needs dependencies that are available at runtime.
*)
GetLoadedDependencySymbols[uri_String] :=
Module[{fileData, contextLoads, depContexts, kernelContexts, loadedCtxs},
  fileData = Lookup[$PacletIndex["Files"], uri, <||>];
  If[fileData === <||>, Return[{}]];

  contextLoads = Lookup[fileData, "ContextLoads", {}];
  depContexts = DeleteDuplicates[Cases[#["context"]& /@ contextLoads, _String]];

  (* Only include contexts actually present in the kernel — i.e. successfully loaded *)
  kernelContexts = GetKernelContextsCached[];
  loadedCtxs = Select[depContexts, MemberQ[kernelContexts, #]&];

  (* Return short (unqualified) names of all symbols from those contexts *)
  DeleteDuplicates @ Flatten[
    Function[{ctx}, StringReplace[Names[ctx <> "*"], ctx -> ""]] /@ loadedCtxs
  ]
]


(*
Get all workspace symbols for workspace/symbol search
*)
GetAllWorkspaceSymbols[] :=
Module[{bag},
  bag = Internal`Bag[];

  KeyValueMap[
    Function[{name, data},
      Scan[
        Function[{def},
          Internal`StuffBag[bag, <|
            "name" -> name,
            "kind" -> def["kind"],
            "location" -> <|
              "uri" -> def["uri"],
              "range" -> <|
                "start" -> <| "line" -> def["source"][[1, 1]] - 1, "character" -> def["source"][[1, 2]] - 1 |>,
                "end" -> <| "line" -> def["source"][[2, 1]] - 1, "character" -> def["source"][[2, 2]] - 1 |>
              |>
            |>,
            "containerName" -> Replace[def["context"], None -> ""]
          |>]
        ],
        data["Definitions"]
      ]
    ],
    $PacletIndex["Symbols"]
  ];

  Internal`BagPart[bag, All]
]


(*
Search workspace symbols by query
*)
SearchWorkspaceSymbols[query_String] :=
Module[{allSymbols},
  allSymbols = GetAllWorkspaceSymbols[];

  If[query === "",
    allSymbols,
    Select[allSymbols, StringContainsQ[#["name"], query, IgnoreCase -> True]&]
  ]
]


(*
Get the context for a symbol from the paclet index
Returns the context string or None if not found
*)
GetSymbolContext[symbolName_String] :=
Module[{symData, defs},
  symData = Lookup[$PacletIndex["Symbols"], symbolName, Null];

  If[symData === Null,
    Return[None]
  ];

  defs = symData["Definitions"];

  If[Length[defs] === 0,
    None
    ,
    (*
    Return the context of the first definition
    *)
    Replace[defs[[1]]["context"], _Missing -> None]
  ]
]


(*
Get all public symbols from a given context
Returns a list of symbol names
*)
GetPublicSymbols[context_String] :=
Module[{contextSymbols, publicSymbols},
  (*
  Get all symbols in this context
  *)
  contextSymbols = Lookup[$PacletIndex["Contexts"], context, {}];

  (*
  Filter to only public symbols
  *)
  publicSymbols = Select[contextSymbols,
    Function[{symName},
      Module[{defs},
        defs = Lookup[$PacletIndex["Symbols"], symName, <||>]["Definitions"];
        If[ListQ[defs] && Length[defs] > 0,
          (*
          Check if any definition in this context is public
          *)
          AnyTrue[defs,
            #["context"] === context && #["visibility"] === "public" &
          ]
          ,
          False
        ]
      ]
    ]
  ];

  publicSymbols
]


(*
Get all symbols in a given context (both public and private)
Returns a list of symbol names
*)
GetContextSymbols[context_String] :=
  Lookup[$PacletIndex["Contexts"], context, {}]


(*
Check if a symbol is public (exported from its package)
Returns True if public, False if private or unknown
*)
IsSymbolPublic[symbolName_String] :=
Module[{symData, defs},
  symData = Lookup[$PacletIndex["Symbols"], symbolName, Null];

  If[symData === Null,
    Return[False]
  ];

  defs = symData["Definitions"];

  If[!ListQ[defs] || Length[defs] === 0,
    False
    ,
    (*
    Check if any definition is marked as public
    *)
    AnyTrue[defs, #["visibility"] === "public" &]
  ]
]


(*
Get all dependency contexts used in the workspace.
These are contexts loaded via BeginPackage[..., {deps}] or Needs[].
*)
GetDependencyContexts[] := $PacletIndex["Dependencies"]


(*
Get the context alias map for the workspace.
Returns an Association mapping alias context strings to their full context strings,
built from Needs["FullContext`" -> "Alias`"] calls across all indexed files.
*)
GetContextAliases[] := $PacletIndex["ContextAliases"]


(*
Check if a context is a known dependency (loaded via BeginPackage or Needs).
*)
IsDependencyContext[context_String] := MemberQ[$PacletIndex["Dependencies"], context]
(* Note: Dependencies list is typically small (< 20 items), MemberQ is acceptable *)


(*
Get the list of context loading statements in a file.
Returns list of <| "context" -> "Pkg`", "source" -> {{l,c},{l,c}}, "method" -> "Needs"|"Get"|"BeginPackage" |>
*)
GetFileContextLoads[uri_String] :=
  Lookup[Lookup[$PacletIndex["Files"], uri, <||>], "ContextLoads", {}]


(*
Get explicit context references in a file (e.g., Developer`ToPackedArray).
Returns list of <| "symbol" -> "name", "context" -> "Ctx`", "fullName" -> "Ctx`name", "source" -> ... |>
*)
GetFileExplicitContextRefs[uri_String] :=
  Lookup[Lookup[$PacletIndex["Files"], uri, <||>], "ExplicitContextRefs", {}]


(*
Get all contexts that are loaded/available in a file.
This includes:
- All kernel-known contexts (from Contexts[]) — these are always available
  without needing Needs[]. This covers System`, Global`, Developer`, Internal`,
  Compile`, JLink`, and many others.
- The file's own package context (if it's a package)
- Contexts loaded via Needs/Get/BeginPackage
- Workspace-defined contexts
*)
(*
Cached kernel contexts — Contexts[] is expensive and the result changes
only when new packages are loaded. Refresh periodically by comparing
the count to the cached count.
*)
$kernelContextsCache = {};
$kernelContextsCacheLen = 0;

GetKernelContextsCached[] :=
Module[{ctx},
  ctx = Quiet[Contexts[], {Contexts::argx}];
  If[!ListQ[ctx], ctx = {"System`", "Global`"}];
  If[Length[ctx] =!= $kernelContextsCacheLen,
    $kernelContextsCache = ctx;
    $kernelContextsCacheLen = Length[ctx]
  ];
  $kernelContextsCache
]

GetFileLoadedContexts[uri_String] :=
Module[{fileData, contextLoads, packageContext, loadedContexts, workspaceContexts, kernelContexts},

  fileData = Lookup[$PacletIndex["Files"], uri, <||>];

  (*
  Get all contexts known to the kernel (cached).
  *)
  kernelContexts = GetKernelContextsCached[];

  If[fileData === <||>,
    Return[DeleteDuplicates[kernelContexts]]
  ];

  (* Start with all kernel-known contexts *)
  loadedContexts = kernelContexts;

  (* Add the file's own package context if it exists *)
  packageContext = Lookup[fileData, "PackageContext", None];
  If[StringQ[packageContext],
    AppendTo[loadedContexts, packageContext];
    (* Also add the Private subcontext *)
    AppendTo[loadedContexts, packageContext <> "Private`"]
  ];

  (* Add contexts from Needs/Get/BeginPackage *)
  contextLoads = Lookup[fileData, "ContextLoads", {}];
  loadedContexts = Join[loadedContexts, #["context"]& /@ contextLoads];

  (* Add alias contexts from Needs["Full`" -> "Alias`"] so that Alias`Symbol is not flagged as unloaded *)
  loadedContexts = Join[loadedContexts, #["alias"]& /@ Lookup[fileData, "ContextAliases", {}]];

  (* Add all workspace-defined contexts (symbols defined in the paclet) *)
  workspaceContexts = Keys[$PacletIndex["Contexts"]];
  loadedContexts = Join[loadedContexts, workspaceContexts];

  DeleteDuplicates[loadedContexts]
]


(*
Check if a specific context is loaded/available in a file.
*)
IsContextLoadedInFile[uri_String, context_String] :=
  MemberQ[GetFileLoadedContexts[uri], context]


(*
Get context loading errors for a file.
Returns list of errors where explicit context references use contexts that aren't loaded.
Only System` and Global` are implicitly available - all other contexts must be explicitly loaded
via Needs[], Get[], or BeginPackage[..., {deps}].

Each error: <| "symbol" -> "name", "context" -> "Ctx`", "fullName" -> "Ctx`name", "source" -> ..., "error" -> "message" |>
*)
GetContextLoadErrors[uri_String] :=
Module[{explicitRefs, loadedContexts, loadedContextsSet},

  explicitRefs = GetFileExplicitContextRefs[uri];
  loadedContexts = GetFileLoadedContexts[uri];

  (* Build Association for O(1) exact-match lookup instead of repeated MemberQ *)
  loadedContextsSet = Association[Thread[loadedContexts -> True]];

  (* Functional approach: Map + Nothing avoids quadratic AppendTo *)
  Function[{ref},
    Module[{ctx},
      ctx = ref["context"];
      If[!KeyExistsQ[loadedContextsSet, ctx] &&
         !AnyTrue[loadedContexts, StringStartsQ[ctx, #] &],
        Append[ref, "error" -> "Context \"" <> ctx <> "\" is not loaded. Add Needs[\"" <> ctx <> "\"] to load it."],
        Nothing
      ]
    ]
  ] /@ explicitRefs
]


(*
Get usage messages for a symbol from the PacletIndex.
Returns a list of usage strings, or {} if none found.
*)
GetSymbolUsages[symbolName_String] :=
Module[{symData},
  symData = Lookup[$PacletIndex["Symbols"], symbolName, Null];
  If[symData === Null,
    {},
    Replace[symData["Usages"], _Missing -> {}]
  ]
]


End[]

EndPackage[]
