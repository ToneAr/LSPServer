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
GetVisibleSymbolDefinitions
GetSymbolOptionNames
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
GetFileBareNameContexts
GetKernelContextsCached
GetSymbolUsages
GetContextAliases
IsWorkspaceSymbol
GetIndexedDependencySymbols
ProcessPendingIndexFiles
ExtractDocComments
ExtractLHSInputPatterns
ExtractFunctionSignatureInfo
DefinitionAcceptsCallArgsQ
GetSymbolInferredPattern
InferVariablePattern
GetLoadedDependencySymbols

$PacletIndex
$WorkspaceRoot
$PendingIndexFiles
$PendingReferenceFiles

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]

(* Load pre-generated builtin pattern map into this package's private context.
   PacletIndex.wl is loaded before Diagnostics.wl, so $BuiltinPatterns cannot
   come from there - each package needs its own copy in its own private context. *)
With[{builtinFile = FileNameJoin[{
    DirectoryName[AbsoluteFileName[$InputFileName]],
    "..", "Resources", "BuiltinInputPatterns.wl"}]},
  If[FileExistsQ[builtinFile],
    Get[builtinFile],
    $BuiltinPatterns = <||>
  ]
];

(*
  builtinSpecToPattern[s]
  Convert a single input spec from $BuiltinPatterns into a WL pattern
  expression suitable for MatchQ against a representative sample value.

  New format (WL patterns stored directly):
    __Type  - BlankSequence[Type]      (1+ expressions with head Type)
    ___Type - BlankNullSequence[Type]  (0+ expressions with head Type)
    __      - BlankSequence[]          (1+ untyped)
    ___     - BlankNullSequence[]      (0+ untyped)
    _?Pred  - PatternTest[Blank[], Pred]
    _Type   - Blank[Type]              (exactly one typed arg)
    None    - Blank[]                  (exactly one untyped arg)

  Legacy string format (still accepted for backward compatibility):
    "Type..." - BlankSequence[Symbol["Type"]]
    "Type*"   - BlankNullSequence[Symbol["Type"]]
    "..."     - BlankSequence[]
    "*"       - BlankNullSequence[]
    "_?Pred"  - ToExpression["_?Pred"]
    "Type"    - Blank[Symbol["Type"]]
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
builtinSpecToPattern[p_] := p  (* already a WL pattern - pass through *)


(*
$PacletIndex stores workspace symbols, per-file metadata, known contexts,
dependency contexts, and alias mappings used by completion, diagnostics,
hover, and cross-file navigation.
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
$PendingReferenceFiles = {}
$PendingExternalDepFiles = {}
$StructuredPackageLoaderCache = <||>

(*
==============================================================================
Workspace-level disk cache
==============================================================================

One WXF file per workspace root stores index data for every source file in
that workspace.  Loading a single file is far cheaper than one Import per
source file.

  Location:  $UserBaseDirectory/ApplicationData/LSPServer/WorkspaceCache/
             <Hex(Hash(workspaceRoot))>.wxf

  Structure: <| "file:///a.wl" -> <| "ModTime" -> t, "Data" -> <|...|> |>, ... |>

  Validation: an entry is fresh iff its "ModTime" equals
              AbsoluteTime[FileDate[filePath, "Modification"]] at load time.

The cache is loaded once in InitializePacletIndex (one Import call regardless
of workspace size).  Cached files are processed synchronously during init
so the index is immediately available for completions/hover.  Only files
without a valid cache entry are queued for background parsing.

The cache is flushed to disk whenever the background parsing queue empties.
==============================================================================
*)

$WorkspaceIndexCache = <||>
$WorkspaceIndexCacheDirty = False
$WorkspaceCacheRoot = None
$WorkspaceCacheSchemaVersion = 3

$WorkspaceCacheDir := FileNameJoin[{
  $UserBaseDirectory, "ApplicationData", "LSPServer", "WorkspaceCache"}]

workspaceCacheFile[root_String] :=
  FileNameJoin[{$WorkspaceCacheDir, IntegerString[Hash[root], 16] <> ".wxf"}]

(*
Load the workspace cache from disk into $WorkspaceIndexCache.
One Import call at the start of InitializePacletIndex.
*)
loadWorkspaceCache[workspaceRoot_String] :=
Module[{cacheFile, loaded, migratedCount},
  $WorkspaceCacheRoot = workspaceRoot;
  cacheFile = workspaceCacheFile[workspaceRoot];
  If[!FileExistsQ[cacheFile],
    $WorkspaceIndexCache = <||>;
    Return[]
  ];
  loaded = Quiet[Import[cacheFile, "WXF"]];
  $WorkspaceIndexCache = If[AssociationQ[loaded], loaded, <||>];

  (*
  Migration: old cache entries stored symbol references under "Symbols" in the
  data payload.  References are now excluded from the disk cache to keep it
  lean (they were typically 10x larger than everything else combined).
  Strip the "Symbols" key from every entry that has it and mark the cache dirty
  so the lean version is written to disk when the background queue drains.
  *)
  migratedCount = 0;
  $WorkspaceIndexCache = Map[
    Function[{entry},
      If[AssociationQ[entry] &&
         AssociationQ[Lookup[entry, "Data", None]] &&
         KeyExistsQ[entry["Data"], "Symbols"],
        migratedCount++;
        Append[entry, "Data" -> KeyDrop[entry["Data"], "Symbols"]],
        entry
      ]
    ],
    $WorkspaceIndexCache
  ];
  If[migratedCount > 0,
    $WorkspaceIndexCacheDirty = True;
    If[$Debug2,
      log["loadWorkspaceCache: migrated ", migratedCount, " old-format entries (stripped Symbols)"]
    ]
  ];

  If[$Debug2,
    log["loadWorkspaceCache: loaded entries for ", Length[$WorkspaceIndexCache], " files"]
  ]
]

(*
Persist $WorkspaceIndexCache to disk.  No-op if nothing has changed.
*)
saveWorkspaceCache[] :=
Module[{cacheFile},
  If[!$WorkspaceIndexCacheDirty || !StringQ[$WorkspaceCacheRoot], Return[]];
  If[!DirectoryQ[$WorkspaceCacheDir],
    Quiet[CreateDirectory[$WorkspaceCacheDir, CreateIntermediateDirectories -> True]]
  ];
  cacheFile = workspaceCacheFile[$WorkspaceCacheRoot];
  Quiet[Export[cacheFile, $WorkspaceIndexCache, "WXF"]];
  $WorkspaceIndexCacheDirty = False;
  If[$Debug2,
    log["saveWorkspaceCache: saved ", Length[$WorkspaceIndexCache], " entries"]
  ]
]

(*
Look up a single file's index data from $WorkspaceIndexCache.
Returns the data Association on a cache hit, or Missing[...] on a miss/stale.
filePath is the local filesystem path (used for mtime check).
*)
readWorkspaceCacheEntry[uri_String, filePath_String] :=
Catch[
Module[{entry, cachedModTime, currentModTime, schemaVersion},
  If[!KeyExistsQ[$WorkspaceIndexCache, uri], Throw[Missing["NoCacheEntry"]]];
  entry = $WorkspaceIndexCache[uri];
  If[!AssociationQ[entry], Throw[Missing["InvalidEntry"]]];
  schemaVersion = Lookup[entry, "SchemaVersion", Missing[]];
  If[schemaVersion =!= $WorkspaceCacheSchemaVersion, Throw[Missing["SchemaVersionMismatch"]]];
  cachedModTime = Lookup[entry, "ModTime", Missing[]];
  If[MissingQ[cachedModTime], Throw[Missing["NoModTime"]]];
  currentModTime = Quiet[AbsoluteTime[FileDate[filePath, "Modification"]]];
  If[!NumberQ[currentModTime], Throw[Missing["FileNotFound"]]];
  If[cachedModTime =!= currentModTime, Throw[Missing["Stale"]]];
  Lookup[entry, "Data", Missing["NoData"]]
]]

(*
Write a file's index data into $WorkspaceIndexCache in memory.
Call saveWorkspaceCache[] to flush to disk.
*)
writeWorkspaceCacheEntry[uri_String, modTime_?NumberQ, data_Association] :=
(
  $WorkspaceIndexCache[uri] = <|
    "SchemaVersion" -> $WorkspaceCacheSchemaVersion,
    "ModTime" -> modTime,
    "Data" -> data
  |>;
  $WorkspaceIndexCacheDirty = True
)


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
Module[{pathParts },
  pathParts = FileNameSplit[filePath];
  AnyTrue[pathParts, Function[{part},
    AnyTrue[$ExcludedDirectoryPatterns, StringMatchQ[part, #]&]
  ]]
]

uriPath[uri_String] := StringReplace[uri, "file://" -> ""]

isWorkspaceURI[uri_String] :=
  StringQ[$WorkspaceRoot] && StringStartsQ[uriPath[uri], $WorkspaceRoot]

InitializePacletIndex[workspaceRoot_String] :=
Module[{files, filteredFiles, cacheHits, cacheMisses, allDepsFromCache, uri, cd, reaped},

  If[$Debug2,
    log["InitializePacletIndex: starting for ", workspaceRoot]
  ];

  $WorkspaceRoot = workspaceRoot;
  $LoadedExternalDependencies = <||>;
  $StructuredPackageLoaderCache = <||>;

  (*
  Reset the index
  *)
  $PacletIndex = <|
    "Symbols" -> <||>,
    "Files" -> <||>,
    "Contexts" -> <||>,
    "Dependencies" -> {},
    "ContextAliases" -> <||>
  |>;

  (*
  Load the workspace-level cache from disk - one Import for the whole workspace
  instead of one per file.
  *)
  loadWorkspaceCache[workspaceRoot];

  (*
  Find all Wolfram Language files in the workspace
  *)
  files = FileNames[LSPServer`Private`workspaceSourceFilePatterns[], workspaceRoot, Infinity];

  (*
  Filter out files in excluded directories (build*, node_modules, .git, etc.)
  *)
  filteredFiles = Select[files, !shouldExcludeFile[#]&];

  If[$Debug2,
    log["InitializePacletIndex: found ", Length[files], " files, ", Length[filteredFiles], " after filtering"]
  ];

  (*
  Partition files into cache hits (valid, up-to-date entry in $WorkspaceIndexCache)
  and cache misses (no entry or stale mtime).
  *)
  allDepsFromCache = {};

  reaped = Association @ Reap[
    Scan[
      Function[{fp},
        uri = "file://" <> fp;
        cd  = readWorkspaceCacheEntry[uri, fp];
        If[AssociationQ[cd],
          Sow[{fp, uri, cd}, "hit"];
          allDepsFromCache = Join[allDepsFromCache, Lookup[cd, "Dependencies", {}]],
          (* else - stale or missing *)
          Sow[fp, "miss"]
        ]
      ],
      filteredFiles
    ],
    _,
    Rule
  ][[2]];

  cacheHits = Lookup[reaped, "hit", {}];
  cacheMisses = Lookup[reaped, "miss", {}];

  If[$Debug2,
    log["InitializePacletIndex: ", Length[cacheHits], " cache hits, ",
      Length[cacheMisses], " cache misses"]
  ];

  (*
  Process all cache hits synchronously - no parse/abstract cycle needed;
  just restore the pre-computed data into the in-memory index.
  Symbol references (Symbols) are intentionally skipped here: they are large
  and only needed for "find all references", not for hover/diagnostics/completions.
  They are populated lazily via $PendingReferenceFiles in the background.
  This makes repeated startups fast regardless of workspace size.
  *)
  Scan[
    Function[{triple},
      Module[{fp, u, data},
        {fp, u, data} = triple;
        addFileToIndex[
          u,
          Lookup[data, "Definitions",        {}],
          Lookup[data, "Usages",             {}],
          {},   (* skip refs during sync init; populated by $PendingReferenceFiles *)
          Lookup[data, "Dependencies",       {}],
          Lookup[data, "ContextLoads",       {}],
          Lookup[data, "ExplicitContextRefs",{}],
          Lookup[data, "PackageContext",     None],
          Lookup[data, "PackageScopeContext", None],
          Lookup[data, "PrivateContext",     None],
          Lookup[data, "ContextAliases",     {}]
        ]
      ]
    ],
    cacheHits
  ];

  (*
  Update the global dependencies list and queue external package source files
  once, using the union of every cached file's dependencies. Deduplicated
  before calling loadExternalDependencies so that each unique package context
  is resolved at most once.
  *)
  allDepsFromCache = DeleteDuplicates[allDepsFromCache];
  If[Length[allDepsFromCache] > 0,
    $PacletIndex["Dependencies"] = DeleteDuplicates[
      Join[$PacletIndex["Dependencies"], allDepsFromCache]
    ]
  ];

  (* Initialize background queues BEFORE loading external deps so that
     queueExternalPackageFiles can AppendTo $PendingIndexFiles safely *)
  $PendingIndexFiles = cacheMisses;
  $PendingReferenceFiles = Map[#[[1]]&, cacheHits];  (* file paths only *)
  $PendingExternalDepFiles = {};

  If[Length[allDepsFromCache] > 0,
    loadExternalDependencies[allDepsFromCache]
  ];

  If[$Debug2,
    log["InitializePacletIndex: queued ", Length[$PendingIndexFiles], " full-parse + ",
      Length[$PendingReferenceFiles], " ref-only files for background indexing"]
  ];

  (*
  Ingest the full workspace during initialization so cross-file references,
  workspace symbols, and diagnostics are ready immediately after startup.
  External dependency files can continue in the background once the workspace
  itself has been fully indexed.
  *)
  While[
    Length[$PendingIndexFiles] > 0 ||
    Length[$PendingReferenceFiles] > 0 ||
    Length[$PendingExternalDepFiles] > 0,
    ProcessPendingIndexFiles[]
  ];

  If[$Debug2,
    log["InitializePacletIndex: fully ingested workspace on init; remaining ext-dep files=",
      Length[$PendingExternalDepFiles]]
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

  (* Priority 0: full-parse queue (cache misses need parse+abstract+extract+cache write) *)
  If[Length[$PendingIndexFiles] > 0,
    (* Parse+abstract is expensive; 20 per batch balances responsiveness vs. throughput. *)
    n = Min[20, Length[$PendingIndexFiles]];
    batch = $PendingIndexFiles[[;;n]];
    $PendingIndexFiles = $PendingIndexFiles[[n + 1 ;;]];
    Scan[indexFile, batch];
    If[$Debug2,
      log["ProcessPendingIndexFiles: full-indexed batch of ", n, ", ",
        Length[$PendingIndexFiles], " full + ", Length[$PendingReferenceFiles], " ref files remaining"]
    ];
    Return[
      Length[$PendingIndexFiles] > 0 ||
      Length[$PendingReferenceFiles] > 0 ||
      Length[$PendingExternalDepFiles] > 0
    ]
  ];

  (* Priority 1: reference-extraction queue (cache hits need only CST parse for refs) *)
  If[Length[$PendingReferenceFiles] > 0,
    n = Min[20, Length[$PendingReferenceFiles]];
    batch = $PendingReferenceFiles[[;;n]];
    $PendingReferenceFiles = $PendingReferenceFiles[[n + 1 ;;]];
    Scan[extractFileReferences, batch];
    If[$Debug2,
      log["ProcessPendingIndexFiles: extracted refs batch of ", n, ", ",
        Length[$PendingReferenceFiles], " ref files remaining"]
    ];
    Return[
      Length[$PendingIndexFiles] > 0 ||
      Length[$PendingReferenceFiles] > 0 ||
      Length[$PendingExternalDepFiles] > 0
    ]
  ];

  (* Priority 2: external dependency files. These are lower priority so a large
     dependency graph cannot starve workspace indexing and refresh. *)
  If[Length[$PendingExternalDepFiles] > 0,
    n = Min[20, Length[$PendingExternalDepFiles]];
    batch = $PendingExternalDepFiles[[;;n]];
    $PendingExternalDepFiles = $PendingExternalDepFiles[[n + 1 ;;]];
    Scan[indexFile, batch];
    log[0, "DBG-TBDU ProcessPendingIndexFiles: indexed ext-dep batch of ", n, ", ",
      Length[$PendingExternalDepFiles], " ext-dep + ", Length[$PendingIndexFiles],
      " full + ", Length[$PendingReferenceFiles], " ref files remaining"];
    Return[
      Length[$PendingIndexFiles] > 0 ||
      Length[$PendingReferenceFiles] > 0 ||
      Length[$PendingExternalDepFiles] > 0
    ]
  ];

  (* Both queues empty - flush any pending cache changes to disk. *)
  saveWorkspaceCache[];
  False
]


(*
Extract and add symbol references for a file that is already in the index.
Called by the background $PendingReferenceFiles queue for cache-hit files that
were restored during init without references (for faster startup).
Parses just the CST (cheaper than full abstract+extract), extracts symbol
references, and merges them into $PacletIndex["Symbols"][name]["References"].
*)
extractFileReferences[filePath_String] :=
Catch[
Module[{text, cst, uri, symbols, refsByName},
  uri = "file://" <> filePath;

  (* Skip if the file is no longer in the index (deleted or workspace changed) *)
  If[!KeyExistsQ[$PacletIndex["Files"], uri], Throw[Null]];

  text = Quiet[Import[filePath, "Text"]];
  If[!StringQ[text], Throw[Null]];

  cst = Quiet[CodeConcreteParse[text]];
  If[FailureQ[cst], Throw[Null]];

  symbols = extractSymbolReferences[cst, uri];

  refsByName = GroupBy[symbols, #["name"]&];

  (* Clear any existing refs for this URI then add new ones.
     Avoids duplicates if UpdateFileIndex was called between init and this background step. *)
  KeyValueMap[
    Function[{name, refs},
      If[KeyExistsQ[$PacletIndex["Symbols"], name],
        $PacletIndex["Symbols", name, "References"] =
          DeleteCases[$PacletIndex["Symbols", name, "References"],
            KeyValuePattern["uri" -> uri]
          ]
      ,
        $PacletIndex["Symbols", name] = <| "Definitions" -> {}, "References" -> {}, "Usages" -> {} |>
      ];
      $PacletIndex["Symbols", name, "References"] = Join[
        $PacletIndex["Symbols", name, "References"],
        KeyDrop[#, "name"]& /@ refs
      ]
    ],
    refsByName
  ];

  If[$Debug2,
    log["extractFileReferences: ", Length[symbols], " refs from ", filePath]
  ]
]]


(*
Index a single file
*)
indexFile[filePath_String] :=
Catch[
Module[{text, cst, ast, uri, symbols, definitions, usages, fileDeps,
  contextLoads, explicitContextRefs, structuredMetadata, mergedStructuredData, packageContext,
  packageScopeContext, privateContext, fileAliases, cacheData, modTime},

  If[$Debug2,
    log["indexFile: ", filePath]
  ];

  uri = "file://" <> filePath;

  (*
  Check the workspace cache. For files in $PendingIndexFiles this will be a
  fast miss (KeyExistsQ returns False). For files added via workspace-folder
  events (not screened by InitializePacletIndex) this may be a valid hit.
  *)
  cacheData = readWorkspaceCacheEntry[uri, filePath];
  If[AssociationQ[cacheData],
    If[$Debug2, log["indexFile: cache hit for ", filePath]];
    Module[{deps = Lookup[cacheData, "Dependencies", {}]},
      If[Length[deps] > 0,
        $PacletIndex["Dependencies"] = DeleteDuplicates[
          Join[$PacletIndex["Dependencies"], deps]
        ];
        If[isWorkspaceURI[uri],
          loadExternalDependencies[deps]
        ]
      ]
    ];
    addFileToIndex[
      uri,
      Lookup[cacheData, "Definitions", {}],
      Lookup[cacheData, "Usages", {}],
      Lookup[cacheData, "Symbols", {}],
      Lookup[cacheData, "Dependencies", {}],
      Lookup[cacheData, "ContextLoads", {}],
      Lookup[cacheData, "ExplicitContextRefs", {}],
      Lookup[cacheData, "PackageContext", None],
      Lookup[cacheData, "PackageScopeContext", None],
      Lookup[cacheData, "PrivateContext", None],
      Lookup[cacheData, "ContextAliases", {}]
    ];
    Throw[Null]
  ];

  text = Quiet[Import[filePath, "Text"]];
  If[!StringQ[text],
    If[$Debug2, log["indexFile: failed to read file"]];
    Throw[Null]
  ];

  cst = Quiet[CodeConcreteParse[text]];
  If[FailureQ[cst],
    If[$Debug2, log["indexFile: failed to parse file"]];
    Throw[Null]
  ];

  ast = Quiet[CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[cst]]];
  If[FailureQ[ast],
    If[$Debug2, log["indexFile: failed to abstract file"]];
    Throw[Null]
  ];

  definitions = extractDefinitions[ast, cst, uri];
  usages = extractUsages[ast, uri];
  symbols = extractSymbolReferences[cst, uri];
  fileDeps = extractDependencies[ast];
  contextLoads = extractContextLoads[ast];
  explicitContextRefs = extractExplicitContextRefs[cst];
  structuredMetadata = structuredPackageMetadata[filePath, ast];
  mergedStructuredData = mergeStructuredDependencyData[fileDeps, contextLoads, structuredMetadata];
  fileDeps = Lookup[mergedStructuredData, "Dependencies", fileDeps];
  contextLoads = Lookup[mergedStructuredData, "ContextLoads", contextLoads];
  packageContext = Lookup[structuredMetadata, "PackageContext", extractPackageContext[ast]];
  packageScopeContext = Lookup[structuredMetadata, "PackageScopeContext", None];
  privateContext = Lookup[structuredMetadata, "PrivateContext", None];
  fileAliases = extractContextAliases[ast];

  If[Length[fileDeps] > 0,
    $PacletIndex["Dependencies"] = DeleteDuplicates[
      Join[$PacletIndex["Dependencies"], fileDeps]
    ];
    If[$Debug2,
      log["indexFile: found dependencies: ", fileDeps]
    ];
    If[isWorkspaceURI[uri],
      loadExternalDependencies[fileDeps]
    ]
  ];

  addFileToIndex[
    uri,
    definitions,
    usages,
    symbols,
    fileDeps,
    contextLoads,
    explicitContextRefs,
    packageContext,
    packageScopeContext,
    privateContext,
    fileAliases
  ];

  modTime = Quiet[AbsoluteTime[FileDate[filePath, "Modification"]]];
  If[NumberQ[modTime],
    writeWorkspaceCacheEntry[uri, modTime, <|
      "Definitions" -> definitions,
      "Usages" -> usages,
      (* "Symbols" (references) intentionally excluded from disk cache: they are
         large (every symbol occurrence in the file) and would bloat the cache
         file ~10x. They are populated in the background via $PendingReferenceFiles
         using extractFileReferences, which is cheap since it only needs the CST. *)
      "Dependencies" -> fileDeps,
      "ContextLoads" -> contextLoads,
      "ExplicitContextRefs" -> explicitContextRefs,
      "PackageContext" -> packageContext,
      "PackageScopeContext" -> packageScopeContext,
      "PrivateContext" -> privateContext,
      "ContextAliases" -> fileAliases
    |>]
  ];
]]


(*
Pure (side-effect-free) version of indexFile.
Performs the same parse/extract steps as indexFile but returns the result
as an Association instead of mutating $PacletIndex.  Used during parallel
startup indexing via ParallelMap.

Returns <|"URI"->..., "FilePath"->..., "Definitions"->..., ..., "FromCache"->bool|>
or $Failed.
*)
indexFilePure[filePath_String] :=
Catch[
Module[{text, cst, ast, uri, definitions, usages, symbols, fileDeps,
  contextLoads, explicitContextRefs, structuredMetadata, mergedStructuredData, packageContext,
  packageScopeContext, privateContext, fileAliases, cacheData, modTime},

  uri = "file://" <> filePath;

  (* Check the disk cache first — avoids a full parse on warm starts *)
  cacheData = readWorkspaceCacheEntry[uri, filePath];
  If[AssociationQ[cacheData],
    Throw[<|
      "URI"                -> uri,
      "FilePath"           -> filePath,
      "Definitions"        -> Lookup[cacheData, "Definitions",         {}],
      "Usages"             -> Lookup[cacheData, "Usages",              {}],
      "Symbols"            -> {},  (* refs extracted separately via $PendingReferenceFiles *)
      "Dependencies"       -> Lookup[cacheData, "Dependencies",        {}],
      "ContextLoads"       -> Lookup[cacheData, "ContextLoads",        {}],
      "ExplicitContextRefs"-> Lookup[cacheData, "ExplicitContextRefs", {}],
      "PackageContext"     -> Lookup[cacheData, "PackageContext",       None],
      "PackageScopeContext" -> Lookup[cacheData, "PackageScopeContext", None],
      "PrivateContext"     -> Lookup[cacheData, "PrivateContext",       None],
      "ContextAliases"     -> Lookup[cacheData, "ContextAliases",      {}],
      "FromCache"          -> True
    |>]
  ];

  (* Cache miss: read + parse + extract *)
  text = Quiet[Import[filePath, "Text"]];
  If[!StringQ[text], Throw[$Failed]];

  cst = Quiet[CodeConcreteParse[text]];
  If[FailureQ[cst], Throw[$Failed]];

  ast = Quiet[CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[cst]]];
  If[FailureQ[ast], Throw[$Failed]];

  definitions        = extractDefinitions[ast, cst, uri];
  usages             = extractUsages[ast, uri];
  symbols            = extractSymbolReferences[cst, uri];
  fileDeps           = extractDependencies[ast];
  contextLoads       = extractContextLoads[ast];
  explicitContextRefs= extractExplicitContextRefs[cst];
  structuredMetadata = structuredPackageMetadata[filePath, ast];
  mergedStructuredData = mergeStructuredDependencyData[fileDeps, contextLoads, structuredMetadata];
  fileDeps = Lookup[mergedStructuredData, "Dependencies", fileDeps];
  contextLoads = Lookup[mergedStructuredData, "ContextLoads", contextLoads];
  packageContext     = Lookup[structuredMetadata, "PackageContext", extractPackageContext[ast]];
  packageScopeContext = Lookup[structuredMetadata, "PackageScopeContext", None];
  privateContext     = Lookup[structuredMetadata, "PrivateContext", None];
  fileAliases        = extractContextAliases[ast];

  <|
    "URI"                -> uri,
    "FilePath"           -> filePath,
    "Definitions"        -> definitions,
    "Usages"             -> usages,
    "Symbols"            -> symbols,
    "Dependencies"       -> fileDeps,
    "ContextLoads"       -> contextLoads,
    "ExplicitContextRefs"-> explicitContextRefs,
    "PackageContext"     -> packageContext,
    "PackageScopeContext" -> packageScopeContext,
    "PrivateContext"     -> privateContext,
    "ContextAliases"     -> fileAliases,
    "FromCache"          -> False
  |>
]]


(*
Apply a single result from indexFilePure to the live $PacletIndex.
Also writes the disk cache entry for future warm starts.
*)
mergeIndexResult[result_?AssociationQ] :=
Module[{uri, filePath, fileDeps, modTime},
  uri      = result["URI"];
  filePath = result["FilePath"];
  fileDeps = Lookup[result, "Dependencies", {}];

  If[Length[fileDeps] > 0,
    $PacletIndex["Dependencies"] = DeleteDuplicates[
      Join[$PacletIndex["Dependencies"], fileDeps]
    ];
    loadExternalDependencies[fileDeps]
  ];

  addFileToIndex[
    uri,
    Lookup[result, "Definitions",         {}],
    Lookup[result, "Usages",              {}],
    Lookup[result, "Symbols",             {}],
    fileDeps,
    Lookup[result, "ContextLoads",        {}],
    Lookup[result, "ExplicitContextRefs", {}],
    Lookup[result, "PackageContext",      None],
    Lookup[result, "PackageScopeContext", None],
    Lookup[result, "PrivateContext",      None],
    Lookup[result, "ContextAliases",      {}]
  ];

  (* Write disk cache for future startups (only for fresh parses) *)
  If[!TrueQ[result["FromCache"]],
    modTime = Quiet[AbsoluteTime[FileDate[filePath, "Modification"]]];
    If[NumberQ[modTime],
      writeWorkspaceCacheEntry[uri, modTime, <|
        "Definitions"        -> result["Definitions"],
        "Usages"             -> result["Usages"],
        "Dependencies"       -> fileDeps,
        "ContextLoads"       -> result["ContextLoads"],
        "ExplicitContextRefs"-> result["ExplicitContextRefs"],
        "PackageContext"     -> result["PackageContext"],
        "PackageScopeContext" -> Lookup[result, "PackageScopeContext", None],
        "PrivateContext"     -> Lookup[result, "PrivateContext", None],
        "ContextAliases"     -> result["ContextAliases"]
      |>]
    ]
  ]
]

mergeIndexResult[$Failed] := Null
mergeIndexResult[_] := Null


(*
Batch add a file's data to the PacletIndex.
Uses grouped-by-name operations instead of per-item AppendTo to avoid O(n^2).
*)
addFileToIndex[uri_, definitions_, usages_, symbols_, fileDeps_, contextLoads_, explicitContextRefs_, packageContext_, packageScopeContext_, privateContext_, fileAliases_:{}] :=
Module[{fileSymbolsBag, defsByName, usagesByName, refsByName},

  fileSymbolsBag = Internal`Bag[];

  (*
  Group definitions by symbol name for batch insert
  *)
  defsByName = GroupBy[definitions, #["name"]&];

  KeyValueMap[
    Function[{name, defs},
      Internal`StuffBag[fileSymbolsBag, name];

      (* If the key exist in the paclet index, reset its index *)
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

      (* Track by context - use first definition's context *)
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
    "PackageScopeContext" -> packageScopeContext,
    "PrivateContext" -> privateContext,
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
=== Doc-comment support ===

Doc-comments follow the JSDoc-style form:

  (* Description:  Some description of the function.
   * Return:       _Integer
   *)
  f[x_Integer] := ...

Description: is optional.  For overloaded functions, any overload may use the
minimal form with only Return:, and the first overload in source order that has
a Description: will have its description shown for all overloads in the hover tooltip:

  (* Description: Compute the foo.
   * Return:      _Integer
   *)
  foo[x_Integer] := ...

  (* Return: _String *)
  foo[x_String] := ...

Doc-comments must appear on the line(s) immediately before the function definition
(no blank lines between the comment end and the definition start).

parseDocComment[commentText_String]
  Parses the raw text of a (* ... *) comment node.
  Returns <| "Description" -> "..." | None, "ReturnPattern" -> expr |>  or  None
  if the comment does not contain a Return: field.
  Description: is optional; if absent, "Description" -> None is stored and
  the hover renderer will inherit a description from the first sibling overload
  that has one.
*)

parseDocComment[commentText_String] :=
Module[{inner, lines, descLines, returnLine, description, returnStr, returnExpr},

  (*
  Strip the (* and *) delimiters, leaving the inner body.
  *)
  inner = commentText;
  inner = StringReplace[inner, StartOfString ~~ "(*" -> ""];
  inner = StringReplace[inner, "*)" ~~ EndOfString -> ""];

  (*
  Split into lines, stripping leading " * " decoration on each line.
  *)
  lines = StringSplit[inner, {"\r\n", "\n", "\r"}];
  lines = StringReplace[#, StartOfString ~~ (WhitespaceCharacter... ~~ "* " | WhitespaceCharacter...) -> ""]& /@ lines;

  (*
  Find the Description: (optional) and Return: (required) fields.
  *)
  descLines = {};
  returnLine = None;

  Scan[
    Function[{line},
      Which[
        StringStartsQ[line, "Description:"],
          AppendTo[descLines, StringTrim[StringDrop[line, StringLength["Description:"]]]]
        ,
        StringStartsQ[line, "Return:"],
          returnLine = StringTrim[StringDrop[line, StringLength["Return:"]]]
        ,
        (*
        Continuation lines for Description (lines that don't start a new field
        and follow after we've started collecting description).
        *)
        descLines =!= {} && returnLine === None && StringLength[StringTrim[line]] > 0,
          AppendTo[descLines, line]
      ]
    ],
    lines
  ];

  (*
  Return: is the only required field.
  *)
  If[returnLine === None,
    Return[None]
  ];

  description = If[descLines === {},
    None,
    StringTrim[StringRiffle[descLines, " "]]
  ];
  returnStr = StringTrim[returnLine];

  (*
  Parse the ReturnPattern string into a WL expression.
  We use ToHeldExpression to avoid evaluating patterns like _Integer -> True etc.
  *)
  returnExpr = Quiet[
    Check[
      ToExpression[returnStr, InputForm, HoldComplete],
      None,
      {Syntax::sntxi, Syntax::sntxb, ToExpression::sntx}
    ],
    {Syntax::sntxi, Syntax::sntxb, ToExpression::sntx}
  ];

  If[MatchQ[returnExpr, HoldComplete[_]],
    returnExpr = First[returnExpr],
    returnExpr = None
  ];

  <| "Description" -> description, "ReturnPattern" -> returnExpr, "ReturnPatternString" -> returnStr |>
]

parseDocComment[_] := None


(*
extractDocComments[cst]
  Scans the CST for Token`Comment leaves that match the doc-comment format.
  Returns an Association mapping  endLine (1-based) -> parsed doc-comment assoc,
  where endLine is the last line of the comment token.

  A valid doc-comment requires only Return: - Description: is optional.

  The caller uses this to check whether the line immediately before a definition's
  start line contains a doc-comment end.
*)
extractDocComments[cst_] :=
Module[{commentNodes, result},

  commentNodes = Cases[cst,
    LeafNode[Token`Comment, text_String, KeyValuePattern[Source -> src_]] /;
      StringContainsQ[text, "Return:"] :>
      {src, text},
    Infinity
  ];

  result = <||>;
  Scan[
    Function[{pair},
      Module[{src, text, endLine, parsed},
        src  = pair[[1]];
        text = pair[[2]];
        endLine = src[[2, 1]];  (* {{startLine, startCol}, {endLine, endCol}} *)
        parsed = parseDocComment[text];
        If[AssociationQ[parsed],
          result[endLine] = parsed
        ]
      ]
    ],
    commentNodes
  ];

  result
]

(* Public alias *)
ExtractDocComments = extractDocComments;


optionRuleNodeQ[node_] :=
  MatchQ[node, CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], {_, _}, _]]


stripNamedPatternWrapper[argNode_] :=
  If[MatchQ[argNode, CallNode[LeafNode[Symbol, "Pattern", _], {_, _}, _]],
    argNode[[2, 2]],
    argNode
  ]


extractOptionNameFromLHS[lhsNode_] :=
  Which[
    MatchQ[lhsNode, LeafNode[Symbol, _String, _]],
      lhsNode[[2]]
    ,
    MatchQ[lhsNode, LeafNode[String, _String, _]],
      StringTake[lhsNode[[2]], {2, -2}]
    ,
    MatchQ[lhsNode, CallNode[LeafNode[Symbol, "HoldPattern", _], {inner_}, _]],
      extractOptionNameFromLHS[lhsNode[[2, 1]]]
    ,
    True,
      None
  ]


extractOptionTargetSymbols[node_] :=
  Which[
    MatchQ[node, LeafNode[Symbol, _String, _]],
      {node[[2]]}
    ,
    MatchQ[node, CallNode[LeafNode[Symbol, "List" | "Alternatives", _], _List, _]],
      DeleteDuplicates[Flatten[extractOptionTargetSymbols /@ node[[2]]]]
    ,
    MatchQ[node, CallNode[LeafNode[Symbol, "HoldPattern", _], {inner_}, _]],
      extractOptionTargetSymbols[node[[2, 1]]]
    ,
    True,
      {}
  ]


optionsPatternNodeQ[argNode_] :=
Module[{inner},
  inner = stripNamedPatternWrapper[argNode];
  MatchQ[inner, CallNode[LeafNode[Symbol, "OptionsPattern", _], _, _]]
]


extractOptionsPatternTargets[argNode_, defaultName_String] :=
Module[{inner, args, targets},
  inner = stripNamedPatternWrapper[argNode];

  If[!MatchQ[inner, CallNode[LeafNode[Symbol, "OptionsPattern", _], _, _]],
    Return[{}]
  ];

  args = inner[[2]];

  If[args === {},
    Return[{defaultName}]
  ];

  targets = DeleteDuplicates[Flatten[extractOptionTargetSymbols /@ args]];

  If[targets === {},
    {defaultName},
    targets
  ]
]


extractOptionDefinitionEntries[node_] :=
  Which[
    optionRuleNodeQ[node],
      Module[{name},
        name = extractOptionNameFromLHS[node[[2, 1]]];
        If[StringQ[name],
          {<|"OptionName" -> name|>},
          {}
        ]
      ]
    ,
    MatchQ[node, CallNode[LeafNode[Symbol, "List" | "Join" | "Sequence", _], _List, _]],
      Flatten[extractOptionDefinitionEntries /@ node[[2]], 1]
    ,
    MatchQ[node, CallNode[LeafNode[Symbol, "Flatten", _], {_, ___}, _]],
      extractOptionDefinitionEntries[node[[2, 1]]]
    ,
    MatchQ[node, CallNode[LeafNode[Symbol, "Options", _], {target_}, _]],
      (<|"OptionTarget" -> #|> & /@ extractOptionTargetSymbols[node[[2, 1]]])
    ,
    True,
      {}
  ]


extractOptionDefinitionData[rhsNode_] :=
Module[{entries},
  entries = extractOptionDefinitionEntries[rhsNode];

  <|
    "OptionNames" -> DeleteDuplicates[
      Cases[entries, KeyValuePattern["OptionName" -> name_String] :> name]
    ],
    "OptionTargets" -> DeleteDuplicates[
      Cases[entries, KeyValuePattern["OptionTarget" -> target_String] :> target]
    ]
  |>
]


(*
extractArgPatternExpr[argNode]
  Given one argument AST node from a function definition LHS, returns a WL
  pattern expression that can be used directly with MatchQ.  Examples:
    x_Integer                                      -> _Integer
    x_                                             -> _
    _Real                                          -> _Real
    {x_Real, y_Real}                               -> {_Real, _Real}
    x__                                            -> __
    KeyValuePattern[{"k" -> x_Integer, ...}]     -> KeyValuePattern[{"k" -> _Integer, ...}]
    x:KeyValuePattern[...]                         -> KeyValuePattern[...]   (strips Pattern name)
    anything else                                  -> _   (conservative)
*)
extractArgPatternExpr[argNode_] :=
  Which[
    (* Named typed blank: x_H -> _H *)
    MatchQ[argNode, CallNode[LeafNode[Symbol, "Pattern", _],
      {_, CallNode[LeafNode[Symbol, "Blank", _], {LeafNode[Symbol, _, _]}, _]}, _]],
      Blank[Symbol[argNode[[2, 2, 2, 1, 2]]]]
    ,
    (* Bare typed blank: _H -> _H *)
    MatchQ[argNode, CallNode[LeafNode[Symbol, "Blank", _], {LeafNode[Symbol, _, _]}, _]],
      Blank[Symbol[argNode[[2, 1, 2]]]]
    ,
    (* Named bare blank: x_ -> _ *)
    MatchQ[argNode, CallNode[LeafNode[Symbol, "Pattern", _],
      {_, CallNode[LeafNode[Symbol, "Blank", _], {}, _]}, _]],
      Blank[]
    ,
    (* Bare blank: _ -> _ *)
    MatchQ[argNode, CallNode[LeafNode[Symbol, "Blank", _], {}, _]],
      Blank[]
    ,
    (* List pattern {p1, p2, ...} -> recurse into each element *)
    MatchQ[argNode, CallNode[LeafNode[Symbol, "List", _], _List, _]],
      extractArgPatternExpr /@ argNode[[2]]
    ,
    (* Named BlankSequence: x__ or x__H *)
    MatchQ[argNode, CallNode[LeafNode[Symbol, "Pattern", _],
      {_, CallNode[LeafNode[Symbol, "BlankSequence", _], _, _]}, _]],
      BlankSequence[]
    ,
    (* Bare BlankSequence: __ or __H *)
    MatchQ[argNode, CallNode[LeafNode[Symbol, "BlankSequence", _], _, _]],
      BlankSequence[]
    ,
    (* Named BlankNullSequence: x___ or x___H *)
    MatchQ[argNode, CallNode[LeafNode[Symbol, "Pattern", _],
      {_, CallNode[LeafNode[Symbol, "BlankNullSequence", _], _, _]}, _]],
      BlankNullSequence[]
    ,
    (* Bare BlankNullSequence: ___ or ___H *)
    MatchQ[argNode, CallNode[LeafNode[Symbol, "BlankNullSequence", _], _, _]],
      BlankNullSequence[]
    ,
    (* Named complex pattern not covered above (e.g. x:KeyValuePattern[...]): strip name, recurse *)
    MatchQ[argNode, CallNode[LeafNode[Symbol, "Pattern", _], {_, _}, _]],
      extractArgPatternExpr[argNode[[2, 2]]]
    ,
    (* KeyValuePattern[{"key" -> pat, ...}] - reconstruct for MatchQ structural checks *)
    MatchQ[argNode, CallNode[LeafNode[Symbol, "KeyValuePattern", _],
      {CallNode[LeafNode[Symbol, "List", _], _List, _]}, _]],
      Module[{listNode, kvpairs},
        listNode = argNode[[2, 1]];
        kvpairs = Cases[listNode[[2]],
          CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _],
            {LeafNode[String, keyStr_String, _], valNode_}, _] :>
            (StringTake[keyStr, {2, -2}] -> extractArgPatternExpr[valNode])
        ];
        KeyValuePattern[kvpairs]
      ]
    ,
    (* Optional argument: conservative - accept anything *)
    MatchQ[argNode, CallNode[LeafNode[Symbol, "Optional", _], _, _]],
      Blank[]
    ,
    (* Complex patterns (PatternTest, Alternatives, Condition, etc.) -> _ conservative *)
    True,
      Blank[]
  ]

(*
extractLHSInputPatterns[lhsCallNode]
  Given the LHS CallNode of a function definition, returns a list with one WL
  pattern expression per argument, suitable for use with MatchQ.
  Examples:
    f[x_Integer, y_String]    -> {_Integer, _String}
    f[{x_Real, y_Real}]       -> {{_Real, _Real}}
    f[x_]                     -> {_}
  Returns {} if lhsCallNode is not a CallNode.
*)
extractLHSInputPatterns[lhsNode_] :=
  If[MatchQ[lhsNode, CallNode[_, _List, _]],
    extractArgPatternExpr /@ lhsNode[[2]],
    {}
  ]

extractFunctionSignatureInfo[funcName_String, lhsNode_] :=
Module[{args, inputPatterns, variadic, hasOptionsPattern, optionTargets},
  If[!MatchQ[lhsNode, CallNode[_, _List, _]],
    Return[<|
      "InputPatterns" -> {},
      "Variadic" -> False,
      "HasOptionsPattern" -> False,
      "OptionTargets" -> {}
    |>]
  ];

  args = lhsNode[[2]];
  inputPatterns = {};
  variadic = False;
  hasOptionsPattern = False;
  optionTargets = {};

  Scan[
    Function[{arg},
      If[optionsPatternNodeQ[arg],
        hasOptionsPattern = True;
        optionTargets = DeleteDuplicates[
          Join[optionTargets, extractOptionsPatternTargets[arg, funcName]]
        ],
        AppendTo[inputPatterns, extractArgPatternExpr[arg]];
        If[MatchQ[arg,
          CallNode[LeafNode[Symbol, "Pattern", _],
            {_, CallNode[LeafNode[Symbol, "BlankSequence" | "BlankNullSequence", _], _, _]}, _] |
          CallNode[LeafNode[Symbol, "BlankSequence" | "BlankNullSequence", _], _, _] |
          CallNode[LeafNode[Symbol, "Optional", _], _, _]
        ],
          variadic = True
        ]
      ]
    ],
    args
  ];

  <|
    "InputPatterns" -> inputPatterns,
    "Variadic" -> variadic,
    "HasOptionsPattern" -> hasOptionsPattern,
    "OptionTargets" -> If[hasOptionsPattern,
      DeleteDuplicates[Replace[optionTargets, {} :> {funcName}]],
      {}
    ]
  |>
]

ExtractFunctionSignatureInfo = extractFunctionSignatureInfo;

ExtractLHSInputPatterns = extractLHSInputPatterns;

(*
inferLiteralNodePattern[node]
  Infers a WL pattern for a literal AST node, resolving all the way down to
  expression atoms (integers, reals, strings, rationals, booleans).  Recurses
  into List and Association constructors so structured literals yield structured
  patterns rather than the coarse _List / _Association.

  Returns:
    Blank[Integer]            for integer literals
    Blank[Real]               for real literals
    Blank[String]             for string literals
    Blank[Rational]           for rational literals
    _?BooleanQ                for True / False literals
    {___T}                    homogeneous list of atom-type T (allows empty)
    {(T1|T2)..}               list with multiple known element types
    <|k -> _T, ...|>          association with all string keys and known value types
    None                      when pattern cannot be determined
*)
inferLiteralNodePattern[node_] :=
  Which[
    MatchQ[node, LeafNode[Integer,  _, _]], Blank[Integer],
    MatchQ[node, LeafNode[Real,     _, _]], Blank[Real],
    MatchQ[node, LeafNode[String,   _, _]], Blank[String],
    MatchQ[node, LeafNode[Rational, _, _]], Blank[Rational],
    MatchQ[node, LeafNode[Symbol, "True" | "False", _]], _?BooleanQ,

    (* List literal: recurse into elements, build {___T} or {(T1|T2)..} *)
    MatchQ[node, CallNode[LeafNode[Symbol, "List", _], _List, _]],
      Module[{elemPats, deduped},
        elemPats = DeleteCases[inferLiteralNodePattern /@ node[[2]], None];
        deduped  = DeleteDuplicates[elemPats];
        Which[
          Length[deduped] === 0,
            None,  (* empty list or all-unknown elements: caller falls back to _List *)
          Length[deduped] === 1 && MatchQ[deduped[[1]], _Blank],
            (* Homogeneous atom list: {___T} via BlankNullSequence[T] *)
            List[BlankNullSequence @@ deduped[[1]]],
          Length[deduped] === 1,
            (* Homogeneous nested-structure elements: {pat..} via Repeated *)
            List[Repeated[deduped[[1]]]],
          True,
            (* Mixed element types: {(p1|p2)..} via Repeated[Alternatives[...]] *)
            List[Repeated[Alternatives @@ deduped]]
        ]
      ],

    (* Association literal: build <|key -> _T, ...|> for string-keyed entries *)
    MatchQ[node, CallNode[LeafNode[Symbol, "Association", _], _List, _]],
      Module[{pairs},
        pairs = Cases[node[[2]],
          CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _],
            {LeafNode[String, keyStr_String, _], valNode_}, _] :>
            (StringTake[keyStr, {2, -2}] -> inferLiteralNodePattern[valNode])
        ];
        If[Length[pairs] === 0 || AnyTrue[pairs[[All, 2]], # === None &],
          None,
          Association @@ pairs
        ]
      ],

    True, None
  ];

(*
sampleToPattern[sample]
  Converts an inferArgSampleValueForRHS result (a concrete WL value such as 0,
  "", 0., {}, <||>) to a WL pattern expression.  Used for _[1] passthrough.
*)
sampleToPattern[s_] :=
  Which[
    IntegerQ[s],          _Integer,
    MatchQ[s, _Real],     _Real,
    StringQ[s],           _String,
    ListQ[s],             _List,
    AssociationQ[s],      _Association,
    MatchQ[s, _Complex],  _Complex,
    MatchQ[s, _Rational], _Rational,
    s === True || s === False, _?BooleanQ,
    True,                 None
  ];

(*
inferArgSampleValueForRHS[argNode]
  Lightweight version of the same helper in Diagnostics - infers a representative
  sample value from a literal call-site argument node, recursing into List constructors.
  Used by inferPatternFromRHS to match call arguments against overload InputPatterns.
*)
inferArgSampleValueForRHS[argNode_] :=
  Which[
    MatchQ[argNode, LeafNode[Integer,   _, _]], 0,
    MatchQ[argNode, LeafNode[String,    _, _]], "",
    MatchQ[argNode, LeafNode[Real,      _, _]], 0.,
    MatchQ[argNode, LeafNode[Rational,  _, _]], 1/2,
    MatchQ[argNode, LeafNode[Symbol, "True",  _]], True,
    MatchQ[argNode, LeafNode[Symbol, "False", _]], False,
    MatchQ[argNode, CallNode[LeafNode[Symbol, "Association", _], _List, _]],
      Module[{pairs},
        pairs = Cases[argNode[[2]],
          CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _],
            {LeafNode[String, keyStr_String, _], valNode_}, _] :>
            (StringTake[keyStr, {2, -2}] -> inferArgSampleValueForRHS[valNode])
        ];
        If[Length[pairs] === 0 || AnyTrue[pairs[[All, 2]], # === Missing["Unknown"] &],
          Missing["Unknown"],
          Association[pairs]
        ]
      ],
    MatchQ[argNode, CallNode[LeafNode[Symbol, "List", _], _List, _]],
      Module[{elems = inferArgSampleValueForRHS /@ argNode[[2]]},
        If[AnyTrue[elems, # === Missing["Unknown"] &],
          Missing["Unknown"],
          elems
        ]
      ],
    (* CompoundExpression[e1, e2, ...] returns its last element *)
    MatchQ[argNode, CallNode[LeafNode[Symbol, "CompoundExpression", _], _List, _]],
      If[Length[argNode[[2]]] === 0, Missing["Unknown"],
        inferArgSampleValueForRHS[Last[argNode[[2]]]]
      ],
    True, Missing["Unknown"]
  ];


ruleLikeArgNodeQ[argNode_] := optionRuleNodeQ[argNode]


splitTrailingOptionArgs[argNodes_List] :=
Module[{splitPos},
  splitPos = Length[argNodes];

  While[splitPos >= 1 && ruleLikeArgNodeQ[argNodes[[splitPos]]],
    splitPos--
  ];

  {Take[argNodes, splitPos], Drop[argNodes, splitPos]}
]


extractCallOptionName[argNode_] :=
  If[ruleLikeArgNodeQ[argNode],
    extractOptionNameFromLHS[argNode[[2, 1]]],
    None
  ]


getIndexedSymbolOptionNames[symbolName_String, visited_List:{}] :=
Module[{defs, directNames, inheritedTargets},
  If[MemberQ[visited, symbolName],
    Return[{}]
  ];

  defs = Select[
    Lookup[Lookup[$PacletIndex["Symbols"], symbolName, <||>], "Definitions", {}],
    Lookup[#, "kind", None] === "option" &
  ];

  directNames = DeleteDuplicates[Flatten[Lookup[defs, "OptionNames", {}]]];
  inheritedTargets = DeleteDuplicates[Flatten[Lookup[defs, "OptionTargets", {}]]];

  DeleteDuplicates[
    Join[
      Cases[directNames, _String],
      Flatten[
        getIndexedSymbolOptionNames[#, Append[visited, symbolName]] & /@
          Cases[inheritedTargets, _String]
      ]
    ]
  ]
]


definitionOptionNames[funcName_String, def_Association] :=
Module[{targets},
  targets = Lookup[def, "OptionTargets", {}];
  targets = Cases[Replace[targets, {} :> {funcName}], _String];
  DeleteDuplicates[Flatten[getIndexedSymbolOptionNames /@ targets]]
]


definitionAcceptsCallArgsQ[funcName_String, def_Association, argNodes_List, argSamples_List, validateOptionNames_:False] :=
Module[{pats, isVar, hasOptionsPattern, positionalNodes, optionNodes,
  positionalSamples, fixedN, varElemPat, optionNames, allowedOptionNames},

  pats = Lookup[def, "InputPatterns", {}];
  isVar = Lookup[def, "Variadic", False];
  hasOptionsPattern = TrueQ[Lookup[def, "HasOptionsPattern", False]];

  {positionalNodes, optionNodes} = If[hasOptionsPattern,
    splitTrailingOptionArgs[argNodes],
    {argNodes, {}}
  ];

  positionalSamples = Take[argSamples, Length[positionalNodes]];

  If[hasOptionsPattern,
    optionNames = extractCallOptionName /@ optionNodes;

    If[TrueQ[validateOptionNames],
      If[MemberQ[optionNames, None],
        Return[False]
      ];

      allowedOptionNames = definitionOptionNames[funcName, def];

      If[Length[allowedOptionNames] === 0 && Length[optionNodes] > 0,
        Return[False]
      ];

      If[!AllTrue[optionNames, MemberQ[allowedOptionNames, #] &],
        Return[False]
      ]
    ]
  ];

  If[isVar && Length[pats] > 0,
    fixedN = Length[pats] - 1;
    varElemPat = With[{lp = Last[pats]},
      If[Head[lp] === BlankSequence && Length[lp] === 1, Blank[lp[[1]]],
      If[Head[lp] === BlankNullSequence && Length[lp] === 1, Blank[lp[[1]]],
      Blank[]]]
    ];
    Length[positionalSamples] >= fixedN &&
    (fixedN === 0 || AllTrue[
      Transpose[{Take[positionalSamples, fixedN], Take[pats, fixedN]}],
      (#[[1]] === Missing["Unknown"] || MatchQ[#[[1]], #[[2]]]) &
    ]) &&
    AllTrue[Drop[positionalSamples, fixedN],
      (# === Missing["Unknown"] || MatchQ[#, varElemPat]) &
    ],
    Length[pats] === Length[positionalSamples] &&
    AllTrue[Transpose[{positionalSamples, pats}],
      (#[[1]] === Missing["Unknown"] || MatchQ[#[[1]], #[[2]]]) &
    ]
  ]
]

DefinitionAcceptsCallArgsQ = definitionAcceptsCallArgsQ;

$InferPatternLocalBindings = <||>;

inferPatternRecordLocalBinding[bindings_Association, name_String, valueNode_, docComments_Association, uri_String] :=
Module[{pat},
  pat = Block[{$InferPatternLocalBindings = bindings},
    inferPatternFromRHS[valueNode, docComments, uri]
  ];
  If[MatchQ[pat, None | _Missing],
    KeyDrop[bindings, name],
    Append[bindings, name -> pat]
  ]
]

inferPatternScopeBindings[varListNode_, docComments_Association, uri_String] :=
Module[{bindings = <||>},
  If[!MatchQ[varListNode, CallNode[LeafNode[Symbol, "List", _], _List, _]],
    Return[bindings]
  ];

  Scan[
    Function[{varNode},
      If[MatchQ[varNode, CallNode[LeafNode[Symbol, "Set", _], {LeafNode[Symbol, _String, _], _}, _]],
        bindings = inferPatternRecordLocalBinding[
          bindings,
          varNode[[2, 1, 2]],
          varNode[[2, 2]],
          docComments,
          uri
        ]
      ]
    ],
    varListNode[[2]]
  ];

  bindings
]

inferPatternFromSequence[nodes_List, docComments_Association, uri_String] :=
Module[{bindings = $InferPatternLocalBindings, pat = None},
  Scan[
    Function[{node},
      If[MatchQ[node, CallNode[LeafNode[Symbol, "Set", _], {LeafNode[Symbol, _String, _], _}, _]],
        Module[{name = node[[2, 1, 2]]},
          bindings = inferPatternRecordLocalBinding[bindings, name, node[[2, 2]], docComments, uri];
          pat = Lookup[bindings, name, None]
        ],
        pat = Block[{$InferPatternLocalBindings = bindings},
          inferPatternFromRHS[node, docComments, uri]
        ]
      ]
    ],
    nodes
  ];

  pat
]

(*
inferPatternFromRHS[rhsNode, docComments, uri]
  Given the RHS of a Set assignment (as an AST node), infer a WL pattern expression
  for the value that will be stored in the variable.  Returns a pattern expression
  (e.g. _Integer, _List, ...) or None if no inference is possible.

  Resolution priority:
    1. Literal leaf nodes  -> _Integer / _Real / _String / _Symbol
    2. List / Association  -> _List / _Association
    3. f[args] call - find the first definition of f whose InputPatterns match the
       actual call arguments (using structural MatchQ), and return its ReturnPattern.
       This allows overload-sensitive inference: f[{1., 1.}] resolves to _Real when
       the matching overload f[{x_Real, y_Real}] carries Return: _Real.
    4. None
*)
inferPatternFromRHS[rhsNode_, docComments_Association, uri_String] :=
Module[{head, headName, calleeDefs, retPat},

  Switch[rhsNode,
    (*
    Literal integer
    *)
    LeafNode[Integer, _, _],
      _Integer
    ,
    (*
    Literal real number
    *)
    LeafNode[Real, _, _],
      _Real
    ,
    (*
    Literal string
    *)
    LeafNode[String, _, _],
      _String
    ,
    (*
    Local symbol reference inside a scoped body: resolve from the most recent
    binding collected while walking Module/Block/With bodies.
    *)
    LeafNode[Symbol, _String, _] /; KeyExistsQ[$InferPatternLocalBindings, rhsNode[[2]]],
      $InferPatternLocalBindings[rhsNode[[2]]]
    ,
    (*
    List constructor: resolve element types down to atoms, yielding {___T} for
    homogeneous lists, {(T1|T2)..} for mixed-atom lists, or _List when the
    element types cannot be determined.
    *)
    CallNode[LeafNode[Symbol, "List", _], _, _],
      With[{pat = inferLiteralNodePattern[rhsNode]},
        If[pat === None, _List, pat]
      ]
    ,
    (*
    Association constructor: resolve value types down to atoms, yielding a
    structured <|key -> _T, ...|> pattern when all string-keyed values are
    inferrable, or _Association otherwise.
    *)
    CallNode[LeafNode[Symbol, "Association", _], _, _],
      With[{pat = inferLiteralNodePattern[rhsNode]},
        If[pat === None, _Association, pat]
      ]
    ,
    (*
    Map[f, expr] / Map[f, expr, levelspec] always returns a list.
    (Map[f] with a single arg is the operator form; we don't infer a return type for that.)
    MapIndexed and MapThread also always return a list when given 2+ args.
    *)
    CallNode[LeafNode[Symbol, "Map" | "MapIndexed" | "MapThread", _], {_, _, ___}, _],
      _List
    ,
    (*
    Which[t1, v1, t2, v2, ...]: even-position args are value branches; return type is the
    union of all their inferred types.  Handled before the generic CallNode case so the
    variadic arg structure is resolved without needing fixed-arity overloads in $BuiltinPatterns.
    *)
    CallNode[LeafNode[Symbol, "Which", _], _List, _],
      Module[{argNodes, nargs, valueIdxs, valuePats},
        argNodes  = rhsNode[[2]];
        nargs     = Length[argNodes];
        valueIdxs = Range[2, nargs, 2];  (* even positions: v1, v2, ... *)
        valuePats = DeleteCases[
          Map[
            Function[{n},
              If[n <= nargs, inferPatternFromRHS[argNodes[[n]], docComments, uri], None]
            ],
            valueIdxs
          ],
          None
        ];
        With[{deduped = DeleteDuplicates[valuePats]},
          Switch[Length[deduped], 0, None, 1, deduped[[1]], _, Apply[Alternatives, deduped]]
        ]
      ]
    ,
    (*
    Switch[expr, p1, v1, p2, v2, ...]: odd-position args >= 3 are value branches; return type
    is the union of all their inferred types.
    *)
    CallNode[LeafNode[Symbol, "Switch", _], _List, _],
      Module[{argNodes, nargs, valueIdxs, valuePats},
        argNodes  = rhsNode[[2]];
        nargs     = Length[argNodes];
        valueIdxs = Range[3, nargs, 2];  (* positions 3, 5, 7, ...: v1, v2, ... *)
        valuePats = DeleteCases[
          Map[
            Function[{n},
              If[n <= nargs, inferPatternFromRHS[argNodes[[n]], docComments, uri], None]
            ],
            valueIdxs
          ],
          None
        ];
        With[{deduped = DeleteDuplicates[valuePats]},
          Switch[Length[deduped], 0, None, 1, deduped[[1]], _, Apply[Alternatives, deduped]]
        ]
      ]
    ,
    (*
    CompoundExpression[e1, e2, ...]: return type = type of the last element.
    This is needed so that If/Which/Switch branch bodies like (b; 2.) are correctly
    inferred as _Real (the type of the last expression, 2.).
    *)
    CallNode[LeafNode[Symbol, "CompoundExpression", _], _List, _],
      If[Length[rhsNode[[2]]] === 0, None,
        inferPatternFromSequence[rhsNode[[2]], docComments, uri]
      ]
    ,
    (*
    If[cond, trueBranch, falseBranch, ...]: branch-aware return type.
    Three cases:
      1. cond is sym op literal (comparison: >, >=, <, <=):
           true branch last expr = sym  ->  PatternTest[_Integer, (# op lit &)]
           false branch           ->  infer normally
           returns Alternatives[falsePat, truePat]  (false first = common/wider type)
      2. cond is sym == literal or sym === literal (equality):
           returns None so position-aware hover falls back to a prior assignment
           with a more specific PatternTest type.
      3. other conditions:
           union of all branch types (same as Which/Switch)
    *)
    CallNode[LeafNode[Symbol, "If", _], _List, _],
      Module[{args, cond, symName, opName, lv, trueBranch, falseBranch,
              truePat, falsePat, lastOfBranch, branchPats},
        args = rhsNode[[2]];
        If[Length[args] < 2, None,
          cond = args[[1]];
          lastOfBranch = Function[{bn},
            If[MatchQ[bn, CallNode[LeafNode[Symbol, "CompoundExpression", _], _List, _]],
              Last[bn[[2]]], bn]
          ];
          Which[
            (* Comparison condition on a symbol: sym > lit, sym < lit, etc. *)
            MatchQ[cond, CallNode[
                LeafNode[Symbol, "Greater"|"GreaterEqual"|"Less"|"LessEqual", _],
                {LeafNode[Symbol, _String, _], LeafNode[Integer|Real|Rational, _, _]},
                _]],
              symName = cond[[2, 1, 2]];
              opName  = cond[[1, 2]];
              lv      = Quiet[ToExpression[cond[[2, 2, 2]]], {ToExpression::shdw}];
              trueBranch  = args[[2]];
              falseBranch = If[Length[args] >= 3, args[[3]], None];
              (* True branch: if last expr is the condition symbol, apply PatternTest *)
              truePat = With[{lastNode = lastOfBranch[trueBranch]},
                If[MatchQ[lastNode, LeafNode[Symbol, symName, _]],
                  With[{op = Symbol[opName], v = lv},
                    PatternTest[_Integer, Function[op[Slot[1], v]]]
                  ],
                  inferPatternFromRHS[trueBranch, docComments, uri]
                ]
              ];
              falsePat = If[falseBranch =!= None,
                inferPatternFromRHS[falseBranch, docComments, uri], None];
              With[{pats = DeleteCases[DeleteDuplicates[{falsePat, truePat}], None]},
                Switch[Length[pats], 0, None, 1, pats[[1]], _, Apply[Alternatives, pats]]
              ],

            (* Equality condition: too specific for a PatternTest union.
               Return None so position-aware hover falls back to a prior assignment
               that carries a comparison-based PatternTest. *)
            MatchQ[cond, CallNode[LeafNode[Symbol, "Equal"|"SameQ", _],
                {LeafNode[Symbol, _String, _], _} |
                {_, LeafNode[Symbol, _String, _]}, _]],
              None,

            (* Other condition: union of all branch types, like Which/Switch *)
            True,
              branchPats = DeleteCases[
                Map[Function[n, If[n <= Length[args],
                    inferPatternFromRHS[args[[n]], docComments, uri], None]],
                  Range[2, Length[args]]],
                None
              ];
              With[{deduped = DeleteDuplicates[branchPats]},
                Switch[Length[deduped], 0, None, 1, deduped[[1]], _, Apply[Alternatives, deduped]]
              ]
          ]
        ]
      ]
    ,
    (*
    Enclose[body, Function[param, cbody]] special case.
    The failure callback always receives _Failure, so:
      - arg 1 type  = inferPatternFromRHS of the body expression
      - arg 2 type  = _Failure when the callback body is the parameter itself,
                      otherwise inferred from the callback body (approximation)
      - 1-arg form  = arg1type | _Failure  (callback omitted; Enclose can throw Failure)
    This is needed because _<2> in the generic builtin spec would be None (Function nodes
    have no stand-alone inferred type).
    *)
    CallNode[LeafNode[Symbol, "Enclose", _], _List, _],
      Module[{args, bodyPat, cbPat, pats, cb, paramName, cbBody},
        args = rhsNode[[2]];
        bodyPat = If[Length[args] >= 1,
          inferPatternFromRHS[args[[1]], docComments, uri], None];
        cbPat = Which[
          (* 1-arg: no explicit callback; Enclose may raise _Failure itself *)
          Length[args] < 2, Blank[Failure],
          (* 2+ args: analyse the callback *)
          True,
            cb = args[[2]];
            Which[
              (* Function[{param,...}, body] — list-form params *)
              MatchQ[cb, CallNode[LeafNode[Symbol, "Function", _],
                           {CallNode[LeafNode[Symbol, "List", _],
                                     {LeafNode[Symbol, _, _]}, _], _}, _]],
                paramName = cb[[2, 1, 2, 1, 2]];
                cbBody    = cb[[2, 2]];
                If[MatchQ[cbBody, LeafNode[Symbol, paramName, _]],
                  Blank[Failure],
                  With[{bp = inferPatternFromRHS[cbBody, docComments, uri]},
                    If[bp =!= None, bp, Blank[Failure]]]],
              (* Function[param, body] — bare-symbol param *)
              MatchQ[cb, CallNode[LeafNode[Symbol, "Function", _],
                           {LeafNode[Symbol, _, _], _}, _]],
                paramName = cb[[2, 1, 2]];
                cbBody    = cb[[2, 2]];
                If[MatchQ[cbBody, LeafNode[Symbol, paramName, _]],
                  Blank[Failure],
                  With[{bp = inferPatternFromRHS[cbBody, docComments, uri]},
                    If[bp =!= None, bp, Blank[Failure]]]],
              (* Unknown callback form — conservative: _Failure *)
              True, Blank[Failure]
            ]
        ];
        pats = DeleteCases[DeleteDuplicates[{bodyPat, cbPat}], None];
        Switch[Length[pats], 0, None, 1, pats[[1]], _, Apply[Alternatives, pats]]
      ]
    ,
    (*
    Module[{vars...}, body] / Block[{vars...}, body] / With[{vars...}, body]:
    return type = type of the body expression (last argument).
    Handled before the generic CallNode case so we recurse directly into the body
    rather than attempting to look up Module/Block/With in $BuiltinPatterns.
    *)
    CallNode[LeafNode[Symbol, "Module" | "Block" | "With", _], _List, _],
      Module[{args = rhsNode[[2]], scopeBindings},
        If[Length[args] < 2,
          None,
          scopeBindings = inferPatternScopeBindings[First[args], docComments, uri];
          Block[{$InferPatternLocalBindings = Join[$InferPatternLocalBindings, scopeBindings]},
            inferPatternFromRHS[Last[args], docComments, uri]
          ]
        ]
      ]
    ,
    (*
    Apply[Join, expr] / Join @@ expr: flattening a list-valued local binding still
    returns a list even when we only know the outer collection pattern.
    *)
    CallNode[LeafNode[Symbol, "Apply", _], {LeafNode[Symbol, "Join", _], _, ___}, _],
      With[{exprPat = inferPatternFromRHS[rhsNode[[2, 2]], docComments, uri]},
        Which[
          exprPat === Blank[List], _List,
          Head[exprPat] === List, _List,
          Head[exprPat] === Alternatives && AllTrue[List @@ exprPat,
            # === Blank[List] || Head[#] === List &], _List,
          True, None
        ]
      ]
    ,
    (*
    Part[expr, i] — propagate the element type of the collection.
      {1,2,3}[[2]]   -> _Integer
      Range[n][[i]]  -> _Integer
    *)
    CallNode[LeafNode[Symbol, "Part", _], {exprArg_, __}, _],
      Module[{elems, pats},
        Which[
          MatchQ[exprArg, CallNode[LeafNode[Symbol, "List", _], {_, ___}, _]],
            elems = exprArg[[2]];
            pats  = DeleteDuplicates[DeleteCases[
              inferPatternFromRHS[#, docComments, uri]& /@ elems, None]];
            Switch[Length[pats], 0, None, 1, pats[[1]], _, Alternatives @@ pats],
          MatchQ[exprArg, CallNode[LeafNode[Symbol, "Range", _], {__}, _]], Blank[Integer],
          True, None
        ]
      ]
    ,
    (*
    Lookup[assoc, key] — propagate the value type of the association.
      Lookup[<|"a"->1, "b"->2|>, key]  -> _Integer
    *)
    CallNode[LeafNode[Symbol, "Lookup", _], {assocArg_, _, ___}, _],
      Which[
        MatchQ[assocArg, CallNode[LeafNode[Symbol, "Association", _], _List, _]],
          Module[{valuePats},
            valuePats = DeleteDuplicates[DeleteCases[
              Cases[assocArg[[2]],
                CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], {_, valNode_}, _] :>
                  inferPatternFromRHS[valNode, docComments, uri],
                1],
              None]];
            Switch[Length[valuePats], 0, None, 1, valuePats[[1]], _, Alternatives @@ valuePats]
          ],
        True, None
      ]
    ,
    (*
    General function call - find the best-matching overload and return its ReturnPattern.
    "Best" means InputPatterns match the actual call arguments via structural MatchQ.
    Falls back to $BuiltinPatterns for System` functions not in the PacletIndex.
    *)
    CallNode[LeafNode[Symbol, headName_String, _], _, _],
      headName = rhsNode[[1, 2]];

      Module[{callArgSamples, allDefs, matchingDef},
        (* Infer sample values from the actual call arguments *)
        callArgSamples = inferArgSampleValueForRHS /@ rhsNode[[2]];

        (* Gather all PacletIndex definitions for this function.
           Fall back to $BuiltinPatterns for System` builtins not indexed locally.
           Note: PacletIndex may contain System symbols with empty Definitions (referenced
           but not defined locally); check Length > 0 before using to ensure we fall through
           to $BuiltinPatterns for those. *)
        allDefs = Which[
          KeyExistsQ[$PacletIndex["Symbols"], headName] &&
            Length[$PacletIndex["Symbols", headName, "Definitions"]] > 0,
            $PacletIndex["Symbols", headName, "Definitions"],
          (* $BuiltinPatterns stores overloads as {inputPatList, retPatStr} pairs.
             Convert to the same <|"InputPatterns"->..., "DocComment"->...|> shape. *)
          KeyExistsQ[$BuiltinPatterns, headName],
            Map[
              Function[{overload},
                Module[{specs = overload[[1]], isVar,
                        inPats, retPat = overload[[2]]},
                  inPats = builtinSpecToPattern /@ specs;
                  isVar = Length[inPats] > 0 && MatchQ[Last[inPats], _BlankSequence | _BlankNullSequence];
                  <|
                    "InputPatterns" -> inPats,
                    "Variadic" -> isVar,
                    "DocComment" -> Which[
                      retPat === None, None,
                      (* Parametric "_<N>" / "_<N>|_<M>" patterns kept as string literals.
                         Store Blank[] as a non-None marker so the first-pass search finds it. *)
                      StringQ[retPat],
                        <|"ReturnPattern" -> Blank[],
                          "ReturnPatternString" -> retPat|>,
                      (* Actual WL pattern — store directly, no ReturnPatternString needed *)
                      True,
                        <|"ReturnPattern" -> retPat|>
                    ]
                  |>
                ]
              ],
              $BuiltinPatterns[headName]
            ],
          True, {}
        ];

        (*
        Find the first definition whose InputPatterns match the call arguments.
        A definition matches when:
          - its InputPatterns length equals the number of call arguments, AND
          - every argument sample satisfies the corresponding pattern expression
            (Missing["Unknown"] args always match - unresolvable literals pass through).
        Among matching definitions, prefer one with a non-None ReturnPattern.
        *)
        matchingDef = Catch[
          (* First pass: matching definition WITH a ReturnPattern *)
          Scan[
            Function[{def},
              Module[{dc},
                dc = Lookup[def, "DocComment", None];
                If[
                  definitionAcceptsCallArgsQ[headName, def, rhsNode[[2]], callArgSamples] &&
                  AssociationQ[dc] && !MatchQ[dc["ReturnPattern"], None | _Missing],
                  Throw[def]
                ]
              ]
            ],
            allDefs
          ];

          (* Second pass: any matching definition regardless of ReturnPattern *)
          Scan[
            Function[{def},
              If[
                definitionAcceptsCallArgsQ[headName, def, rhsNode[[2]], callArgSamples],
                Throw[def]
              ]
            ],
            allDefs
          ];

          None
        ];

        If[AssociationQ[matchingDef],
          With[{dc = Lookup[matchingDef, "DocComment", None]},
            Which[
              (* "_<N>" / "_<N>|_<M>" / "_<N>|Null" passthrough: return type = inferred type of the
                 Nth call arg (or a union of multiple args, possibly including Null for
                 branching functions like If[t,a] 2-arg form).  When the argument is itself
                 a function call (not a literal), we recurse into inferPatternFromRHS so
                 that chains like  var = Echo @ f[{1., 1.}]  resolve transitively.
                 Note: use ___ wildcard suffix to match trailing "|..." union parts. *)
              AssociationQ[dc] && StringMatchQ[Lookup[dc, "ReturnPatternString", ""],
                "_<" ~~ DigitCharacter.. ~~ ">" ~~ ___],
                Module[{retStr, argIdxs, hasNull, argNodes, argPats},
                  retStr   = dc["ReturnPatternString"];
                  (* Parse all arg indices out of e.g. "_<2>|_<3>" or "_<2>|Null" *)
                  argIdxs  = ToExpression /@ StringCases[retStr,
                                "_<" ~~ n:DigitCharacter.. ~~ ">" :> n];
                  hasNull  = StringContainsQ[retStr, "|Null"];
                  argNodes = rhsNode[[2]];
                  (* For each referenced arg, try literal fast path then full recursion *)
                  argPats = DeleteCases[
                    Map[
                      Function[{n},
                        If[n > Length[argNodes], None,
                          With[{sp = sampleToPattern[
                                  If[n <= Length[callArgSamples],
                                    callArgSamples[[n]], Missing["Unknown"]]]},
                            If[sp =!= None,
                              sp,
                              (* Recursive: arg is a call like f[{1.,1.}] *)
                              inferPatternFromRHS[argNodes[[n]], docComments, uri]
                            ]
                          ]
                        ]
                      ],
                      argIdxs
                    ],
                    None
                  ];
                  If[hasNull, AppendTo[argPats, Null]];
                  With[{deduped = DeleteDuplicates[argPats]},
                    Switch[Length[deduped],
                      0, None,
                      1, deduped[[1]],
                      _, Apply[Alternatives, deduped]
                    ]
                  ]
                ],
              (* Normal fixed return type *)
              AssociationQ[dc] && !MatchQ[dc["ReturnPattern"], None | _Missing],
                dc["ReturnPattern"],
              True, None
            ]
          ],
          (* No overload matched arguments - fall back to first definition with ReturnPattern *)
          Catch[
            Scan[
              Function[{def},
                Module[{dc},
                  dc = Lookup[def, "DocComment", None];
                  If[AssociationQ[dc] && !MatchQ[dc["ReturnPattern"], None | _Missing]
                       && !StringMatchQ[Lookup[dc, "ReturnPatternString", ""],
                            "_<" ~~ DigitCharacter.. ~~ ">" ~~ ___],
                    Throw[dc["ReturnPattern"]]
                  ]
                ]
              ],
              allDefs
            ];
            None
          ]
        ]
      ],
    _,
      None
  ]
]


(*
GetSymbolInferredPattern[symbolName, uri]
  Returns the inferred pattern for the given symbol defined in the given file URI,
  or None if no pattern could be inferred.  Looks at all definitions for the symbol
  in that URI and returns the first non-None InferredPattern found.
*)
GetSymbolInferredPattern[symbolName_String, uri_String] :=
Module[{defs},
  If[!KeyExistsQ[$PacletIndex["Symbols"], symbolName],
    Return[None]
  ];
  defs = Select[$PacletIndex["Symbols", symbolName, "Definitions"],
    #["uri"] === uri &
  ];
  Catch[
    Scan[
      Function[{def},
        Module[{ip},
          ip = Lookup[def, "InferredPattern", None];
          If[ip =!= None && !MatchQ[ip, _Missing],
            Throw[ip]
          ]
        ]
      ],
      defs
    ];
    None
  ]
]

(*
InferVariablePattern[symbolName, uri]
  Public API: returns the inferred pattern string for varName in uri, or None.
*)
InferVariablePattern[symbolName_String, uri_String] :=
Module[{pat},
  pat = GetSymbolInferredPattern[symbolName, uri];
  If[pat === None,
    None,
    Quiet[ToString[pat, InputForm], {ToString::shdw}]
  ]
]


(*
Extract definitions from AST, with doc-comment association via CST.
The overload with cst_ builds the docComments map and threads it into walkASTForDefinitions.
*)
extractDefinitions[ast_, cst_, uri_] :=
Module[{definitions, docComments, structuredContexts, packageContext, packageScopeContext,
  exportedDeclaredSymbols, scopedDeclaredSymbols, declaredSymbols},
  docComments = ExtractDocComments[cst];
  structuredContexts = structuredPackageMetadata[uriPath[uri], ast];
  packageContext = Lookup[structuredContexts, "PackageContext", None];
  packageScopeContext = Lookup[structuredContexts, "PackageScopeContext", None];
  exportedDeclaredSymbols = structuredPackageDeclaredSymbols[ast, uri, "PackageExported", "public", packageContext];
  scopedDeclaredSymbols = structuredPackageDeclaredSymbols[ast, uri, "PackageScoped", "package", packageScopeContext];
  declaredSymbols = Join[exportedDeclaredSymbols, scopedDeclaredSymbols];
  structuredContexts = Join[structuredContexts, <|
    "ExportedSymbols" -> Replace[Lookup[exportedDeclaredSymbols, "name", {}], _Missing -> {}],
    "ScopedSymbols" -> Replace[Lookup[scopedDeclaredSymbols, "name", {}], _Missing -> {}]
  |>];
  definitions = Internal`Bag[];
  walkASTForDefinitions[definitions, ast, None, False, uri, docComments, structuredContexts];
  resolveInferredPatterns[Join[Internal`BagPart[definitions, All], declaredSymbols], uri]
]


(*
Extract definitions from AST
*)
extractDefinitions[ast_, uri_] :=
Module[{definitions, structuredContexts, packageContext, packageScopeContext,
  exportedDeclaredSymbols, scopedDeclaredSymbols, declaredSymbols},
  structuredContexts = structuredPackageMetadata[uriPath[uri], ast];
  packageContext = Lookup[structuredContexts, "PackageContext", None];
  packageScopeContext = Lookup[structuredContexts, "PackageScopeContext", None];
  exportedDeclaredSymbols = structuredPackageDeclaredSymbols[ast, uri, "PackageExported", "public", packageContext];
  scopedDeclaredSymbols = structuredPackageDeclaredSymbols[ast, uri, "PackageScoped", "package", packageScopeContext];
  declaredSymbols = Join[exportedDeclaredSymbols, scopedDeclaredSymbols];
  structuredContexts = Join[structuredContexts, <|
    "ExportedSymbols" -> Replace[Lookup[exportedDeclaredSymbols, "name", {}], _Missing -> {}],
    "ScopedSymbols" -> Replace[Lookup[scopedDeclaredSymbols, "name", {}], _Missing -> {}]
  |>];
  definitions = Internal`Bag[];
  walkASTForDefinitions[definitions, ast, None, False, uri, <||>, structuredContexts];
  resolveInferredPatterns[Join[Internal`BagPart[definitions, All], declaredSymbols], uri]
]


(*
resolveInferredPatterns[definitions, uri]
  Second pass over the collected definitions: for every constant definition whose
  RHS was a function call (InferredPattern -> None from walkASTForDefinitions),
  attempt to resolve the correct overload using the InputPatterns and DocComment
  ReturnPattern from the function definitions collected in the same file.

  This is necessary because walkASTForDefinitions processes nodes in order - when
  a = f[{1., 1.}] is visited, f's definitions have been collected into the bag but
  not yet committed to $PacletIndex, so inferPatternFromRHS cannot find them there.
  Here, we use the local definitions list instead.
*)
resolveInferredPatterns[definitions_List, uri_String] :=
Module[{localFuncDefs},
  (* Build a name -> {def, def, ...} map for function definitions in this file *)
  localFuncDefs = GroupBy[
    Select[definitions, #["kind"] === "function" &],
    #["name"] &
  ];

  Map[
    Function[{def},
      Which[
        (* Retry constant definitions whose RHS was a function call unresolvable on first pass *)
        def["kind"] === "constant" &&
            (MatchQ[def["InferredPattern"], None | _Missing] ||
             (* Also retry branching builtins: their first-pass may have resolved
                only some arg branches (those that were literals), missing call-based
                args whose return types become available only after the second pass. *)
             (StringQ[def["rhsCallHead"]] &&
              MemberQ[{"If","Which","Switch","Check","Catch","Quiet","Module","Block","With"},
                      def["rhsCallHead"]]) ||
             (* Retry when callee has user-defined overloads (e.g. TagSetDelayed) that
                may provide a more specific return type than the builtin default. *)
             (StringQ[def["rhsCallHead"]] &&
              Length[Lookup[localFuncDefs, def["rhsCallHead"], {}]] > 0)),
          Module[{resolved},
            resolved = resolveCallReturnPattern[def["rhsCallHead"], def["rhsCallArgs"], localFuncDefs];
            If[resolved =!= None,
              Append[def, "InferredPattern" -> resolved],
              def
            ]
          ],
        (* Retry function definitions whose body return type was unresolved on first pass *)
        def["kind"] === "function" &&
            MatchQ[def["InferredReturnPattern"], None | _Missing] &&
            StringQ[def["rhsCallHead"]],
          Module[{resolved},
            resolved = resolveCallReturnPattern[def["rhsCallHead"], def["rhsCallArgs"], localFuncDefs];
            If[resolved =!= None,
              Append[def, "InferredReturnPattern" -> resolved],
              def
            ]
          ],
        True, def
      ]
    ],
    definitions
  ]
]


(*
resolveCallReturnPattern[headName, argNodes, localFuncDefs]
  Given the callee name, list of call argument AST nodes, and a map of local function
  definitions, find the first overload whose InputPatterns match the argument samples
  and return its ReturnPattern.  Returns None if no match is found.
*)
resolveCallReturnPattern[headName_String, argNodes_List, localFuncDefs_Association] :=
Module[{argSamples, localDefs, defs},
  argSamples = inferArgSampleValueForRHS /@ argNodes;
  localDefs = Lookup[localFuncDefs, headName, {}];
  (* Also consult $BuiltinPatterns for System builtins not defined locally.
     This is needed so that passthrough builtins like Echo/Identity are resolved
     in the second pass when f (the argument) is already committed to $PacletIndex
     via localFuncDefs but Echo itself is only in $BuiltinPatterns. *)
  defs = If[Length[localDefs] > 0,
    localDefs,
    If[KeyExistsQ[$BuiltinPatterns, headName],
      Map[
        Function[{overload},
          Module[{specs = overload[[1]], isVar,
                  inPats, retPat = overload[[2]]},
            inPats = builtinSpecToPattern /@ specs;
            isVar = Length[inPats] > 0 && MatchQ[Last[inPats], _BlankSequence | _BlankNullSequence];
            <|
              "InputPatterns" -> inPats,
              "Variadic" -> isVar,
              "DocComment" -> Which[
                retPat === None, None,
                StringQ[retPat],
                  <|"ReturnPattern" -> Blank[],
                    "ReturnPatternString" -> retPat|>,
                True,
                  <|"ReturnPattern" -> retPat|>
              ]
            |>
          ]
        ],
        $BuiltinPatterns[headName]
      ],
      {}
    ]
  ];
  Catch[
    Scan[
      Function[{def},
        Module[{dc, retStr},
          dc     = Lookup[def, "DocComment", None];
          retStr = If[AssociationQ[dc], Lookup[dc, "ReturnPatternString", ""], ""];
          If[
            definitionAcceptsCallArgsQ[headName, def, argNodes, argSamples] &&
            AssociationQ[dc] && !MatchQ[dc["ReturnPattern"], None | _Missing],
            (* _<N> / _<N>|_<M> / _<N>|Null: recurse into the referenced call argument.
               Also handles "|Null" suffix for branching functions like If[t,a] which may
               return Null when the condition is False and no else-branch is given.
               Note: use ___ wildcard suffix to match trailing "|..." union parts. *)
            If[StringMatchQ[retStr, "_<" ~~ DigitCharacter.. ~~ ">" ~~ ___],
              Module[{argIdxs, hasNull, argPats},
                argIdxs = ToExpression /@ StringCases[retStr, "_<" ~~ n:DigitCharacter.. ~~ ">" :> n];
                hasNull = StringContainsQ[retStr, "|Null"];
                argPats = DeleteCases[
                  Map[
                    Function[{n},
                      If[n > Length[argNodes], None,
                        With[{sp = sampleToPattern[If[n <= Length[argSamples],
                                    argSamples[[n]], Missing["Unknown"]]]},
                          If[sp =!= None,
                            sp,
                            (* arg itself is a call - recurse *)
                            If[MatchQ[argNodes[[n]], CallNode[LeafNode[Symbol, _String, _], _List, _]],
                              resolveCallReturnPattern[
                                argNodes[[n, 1, 2]], argNodes[[n, 2]], localFuncDefs],
                              None
                            ]
                          ]
                        ]
                      ]
                    ],
                    argIdxs
                  ],
                  None
                ];
                If[hasNull, AppendTo[argPats, Null]];
                If[Length[argPats] > 0,
                  Throw[With[{deduped = DeleteDuplicates[argPats]},
                    Switch[Length[deduped],
                      1, deduped[[1]],
                      _, Apply[Alternatives, deduped]
                    ]
                  ]]
                ]
              ],
              Throw[dc["ReturnPattern"]]
            ]
          ]
        ]
      ],
      defs
    ];
    (* Second pass: fall back to InferredReturnPattern for definitions that carry an
       automatically-inferred return type but no explicit DocComment Return: annotation.
       InputPattern arity/type matching still applies so we pick the right overload. *)
    Scan[
      Function[{def},
        Module[{irp},
          irp  = Lookup[def, "InferredReturnPattern", None];
          If[
            !MatchQ[irp, None | _Missing] &&
            definitionAcceptsCallArgsQ[headName, def, argNodes, argSamples],
            Throw[irp]
          ]
        ]
      ],
      defs
    ];
    None
  ]
]

(* CompoundExpression: resolve the type of its last element *)
resolveCallReturnPattern["CompoundExpression", argNodes_List, localFuncDefs_Association] :=
  Module[{last, sv},
    If[Length[argNodes] === 0, None,
      last = Last[argNodes];
      sv = sampleToPattern[inferArgSampleValueForRHS[last]];
      If[sv =!= None, sv,
        If[MatchQ[last, CallNode[LeafNode[Symbol, _String, _], _List, _]],
          resolveCallReturnPattern[last[[1, 2]], last[[2]], localFuncDefs],
          None
        ]
      ]
    ]
  ]

(* Module / Block / With: return type = type of the body (last argument).
   Module[{vars...}, body] and Block[{vars...}, body] and With[{vars...}, body]
   all return the value of their body expression.  We ignore the var-list (first
   arg) and recurse into the body exactly like CompoundExpression recurses into
   its last element. *)
resolveCallReturnPattern["Module" | "Block" | "With", argNodes_List, localFuncDefs_Association] :=
  Module[{body, sv},
    If[Length[argNodes] < 2, Return[None]];
    body = Last[argNodes];
    sv = sampleToPattern[inferArgSampleValueForRHS[body]];
    If[sv =!= None, sv,
      If[MatchQ[body, CallNode[LeafNode[Symbol, _String, _], _List, _]],
        resolveCallReturnPattern[body[[1, 2]], body[[2]], localFuncDefs],
        None
      ]
    ]
  ]

(* Which / Switch: return type = union of value-branch types, mirroring inferPatternFromRHS *)
resolveCallReturnPattern["Which", argNodes_List, localFuncDefs_Association] :=
  Module[{argSamples, nargs, valueIdxs, valuePats},
    argSamples = inferArgSampleValueForRHS /@ argNodes;
    nargs      = Length[argNodes];
    valueIdxs  = Range[2, nargs, 2];
    valuePats  = DeleteCases[
      Map[
        Function[{n},
          If[n > nargs, None,
            With[{sp = sampleToPattern[If[n <= Length[argSamples], argSamples[[n]], Missing["Unknown"]]]},
              If[sp =!= None, sp,
                If[MatchQ[argNodes[[n]], CallNode[LeafNode[Symbol, _String, _], _List, _]],
                  resolveCallReturnPattern[argNodes[[n, 1, 2]], argNodes[[n, 2]], localFuncDefs],
                  None
                ]
              ]
            ]
          ]
        ],
        valueIdxs
      ],
      None
    ];
    With[{deduped = DeleteDuplicates[valuePats]},
      Switch[Length[deduped], 0, None, 1, deduped[[1]], _, Apply[Alternatives, deduped]]
    ]
  ]

resolveCallReturnPattern["Switch", argNodes_List, localFuncDefs_Association] :=
  Module[{argSamples, nargs, valueIdxs, valuePats},
    argSamples = inferArgSampleValueForRHS /@ argNodes;
    nargs      = Length[argNodes];
    valueIdxs  = Range[3, nargs, 2];  (* positions 3, 5, 7, ...: v1, v2, ... *)
    valuePats  = DeleteCases[
      Map[
        Function[{n},
          If[n > nargs, None,
            With[{sp = sampleToPattern[If[n <= Length[argSamples], argSamples[[n]], Missing["Unknown"]]]},
              If[sp =!= None, sp,
                If[MatchQ[argNodes[[n]], CallNode[LeafNode[Symbol, _String, _], _List, _]],
                  resolveCallReturnPattern[argNodes[[n, 1, 2]], argNodes[[n, 2]], localFuncDefs],
                  None
                ]
              ]
            ]
          ]
        ],
        valueIdxs
      ],
      None
    ];
    With[{deduped = DeleteDuplicates[valuePats]},
      Switch[Length[deduped], 0, None, 1, deduped[[1]], _, Apply[Alternatives, deduped]]
    ]
  ]

(* If[cond, trueBranch, falseBranch]: mirrors the inferPatternFromRHS If case.
   Used in the second-pass retry for definitions whose first-pass returned None.
   - Comparison cond (sym > lit, etc.): PatternTest for true branch, normal for false.
   - Equality cond (sym == lit): return None so position-aware falls back.
   - Other: union of branch types. *)
resolveCallReturnPattern["If", argNodes_List, localFuncDefs_Association] :=
  Module[{cond, symName, opName, lv, trueBranch, falseBranch,
          truePat, falsePat, lastOfBranch, branchPats},
    If[Length[argNodes] < 2, Return[None]];
    cond        = argNodes[[1]];
    lastOfBranch = Function[{bn},
      If[MatchQ[bn, CallNode[LeafNode[Symbol, "CompoundExpression", _], _List, _]],
        Last[bn[[2]]], bn]
    ];
    Which[
      MatchQ[cond, CallNode[
          LeafNode[Symbol, "Greater"|"GreaterEqual"|"Less"|"LessEqual", _],
          {LeafNode[Symbol, _String, _], LeafNode[Integer|Real|Rational, _, _]},
          _]],
        symName = cond[[2, 1, 2]];
        opName  = cond[[1, 2]];
        lv      = Quiet[ToExpression[cond[[2, 2, 2]]], {ToExpression::shdw}];
        trueBranch  = argNodes[[2]];
        falseBranch = If[Length[argNodes] >= 3, argNodes[[3]], None];
        truePat = With[{lastNode = lastOfBranch[trueBranch]},
          If[MatchQ[lastNode, LeafNode[Symbol, symName, _]],
            With[{op = Symbol[opName], v = lv}, PatternTest[_Integer, Function[op[Slot[1], v]]]],
            resolveCallReturnPattern["CompoundExpression",
              If[MatchQ[trueBranch, CallNode[LeafNode[Symbol, "CompoundExpression", _], _List, _]],
                trueBranch[[2]], {trueBranch}], localFuncDefs]
          ]
        ];
        falsePat = If[falseBranch =!= None,
          resolveCallReturnPattern["CompoundExpression",
            If[MatchQ[falseBranch, CallNode[LeafNode[Symbol, "CompoundExpression", _], _List, _]],
              falseBranch[[2]], {falseBranch}], localFuncDefs],
          None];
        With[{pats = DeleteCases[DeleteDuplicates[{falsePat, truePat}], None]},
          Switch[Length[pats], 0, None, 1, pats[[1]], _, Apply[Alternatives, pats]]
        ],

      MatchQ[cond, CallNode[LeafNode[Symbol, "Equal"|"SameQ", _],
          {LeafNode[Symbol, _String, _], _} |
          {_, LeafNode[Symbol, _String, _]}, _]],
        None,

      True,
        branchPats = DeleteCases[
          Map[Function[n, If[n <= Length[argNodes],
              resolveCallReturnPattern["CompoundExpression",
                With[{bn = argNodes[[n]]},
                  If[MatchQ[bn, CallNode[LeafNode[Symbol, "CompoundExpression", _], _List, _]],
                    bn[[2]], {bn}]], localFuncDefs], None]],
            Range[2, Length[argNodes]]],
          None
        ];
        With[{deduped = DeleteDuplicates[branchPats]},
          Switch[Length[deduped], 0, None, 1, deduped[[1]], _, Apply[Alternatives, deduped]]
        ]
    ]
  ]

(* Enclose[body, Function[param, cbody]]: mirrors the inferPatternFromRHS Enclose case. *)
resolveCallReturnPattern["Enclose", argNodes_List, localFuncDefs_Association] :=
  Module[{bodyPat, cbPat, cb, paramName, cbBody, pats},
    bodyPat = If[Length[argNodes] >= 1,
      inferPatternFromRHS[argNodes[[1]], <||>, ""], None];
    cbPat = Which[
      Length[argNodes] < 2, Blank[Failure],
      True,
        cb = argNodes[[2]];
        Which[
          MatchQ[cb, CallNode[LeafNode[Symbol, "Function", _],
                       {CallNode[LeafNode[Symbol, "List", _],
                                 {LeafNode[Symbol, _, _]}, _], _}, _]],
            paramName = cb[[2, 1, 2, 1, 2]];
            cbBody    = cb[[2, 2]];
            If[MatchQ[cbBody, LeafNode[Symbol, paramName, _]],
              Blank[Failure],
              With[{bp = inferPatternFromRHS[cbBody, <||>, ""]},
                If[bp =!= None, bp, Blank[Failure]]]],
          MatchQ[cb, CallNode[LeafNode[Symbol, "Function", _],
                       {LeafNode[Symbol, _, _], _}, _]],
            paramName = cb[[2, 1, 2]];
            cbBody    = cb[[2, 2]];
            If[MatchQ[cbBody, LeafNode[Symbol, paramName, _]],
              Blank[Failure],
              With[{bp = inferPatternFromRHS[cbBody, <||>, ""]},
                If[bp =!= None, bp, Blank[Failure]]]],
          True, Blank[Failure]
        ]
    ];
    pats = DeleteCases[DeleteDuplicates[{bodyPat, cbPat}], None];
    Switch[Length[pats], 0, None, 1, pats[[1]], _, Apply[Alternatives, pats]]
  ]

(* Part[expr, i] — return the element type of the collection argument *)
resolveCallReturnPattern["Part", argNodes_List, localFuncDefs_Association] :=
  If[Length[argNodes] < 1, None,
    Module[{exprArg = argNodes[[1]], elems, pats},
      Which[
        MatchQ[exprArg, CallNode[LeafNode[Symbol, "List", _], {_, ___}, _]],
          elems = exprArg[[2]];
          pats  = DeleteDuplicates[DeleteCases[
            sampleToPattern[inferArgSampleValueForRHS[#]]& /@ elems, None]];
          Switch[Length[pats], 0, None, 1, pats[[1]], _, Alternatives @@ pats],
        MatchQ[exprArg, CallNode[LeafNode[Symbol, "Range", _], {__}, _]], Blank[Integer],
        True, None
      ]
    ]
  ]

(* Lookup[assoc, key] — return the value type of the association argument *)
resolveCallReturnPattern["Lookup", argNodes_List, localFuncDefs_Association] :=
  If[Length[argNodes] < 1, None,
    Module[{assocArg = argNodes[[1]], valuePats},
      Which[
        MatchQ[assocArg, CallNode[LeafNode[Symbol, "Association", _], _List, _]],
          valuePats = DeleteDuplicates[DeleteCases[
            Cases[assocArg[[2]],
              CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], {_, valNode_}, _] :>
                With[{sp = sampleToPattern[inferArgSampleValueForRHS[valNode]]},
                  If[sp =!= None, sp, inferPatternFromRHS[valNode, <||>, ""]]],
              1],
            None]];
          Switch[Length[valuePats], 0, None, 1, valuePats[[1]], _, Alternatives @@ valuePats],
        True, None
      ]
    ]
  ]

resolveCallReturnPattern[_, _, _] := None


(*
Extract package dependencies from AST.
BeginPackage["MyPackage`", {"Dep1`", "Dep2`"}] creates a PackageNode with
tag = {LeafNode[String, "MyPackage`", ...], CallNode[List, {LeafNode[String, "Dep1`", ...], ...}, ...]}

Also extracts Needs["Package`"] calls at the top level.
*)
extractDependencies[ast_] :=
Module[{packageDeps, needsDeps, needsAliasDeps, structuredHiddenDeps, structuredImportDeps, allDeps},

  (*
  Extract from PackageNode tags - dependencies are in the List following the main context
  *)
  packageDeps = Cases[ast,
    PackageNode[{LeafNode[String, _, _], CallNode[LeafNode[Symbol, "List", _], deps_List, _], ___}, _, _] :>
      Cases[deps, LeafNode[String, s_String, _] :> normalizeContextString[s]],
    {0, 2}
  ] // Flatten;

  (*
  Also extract from Needs["Package`"] and Get["Package`"] calls anywhere in the file.
  These may be at top level, inside PackageNode, inside ContextNode (Private), etc.
  Ignore Get["path/to/file.wl"] style file loads here; those are not valid contexts
  to pass to Needs later.
  *)
  needsDeps = Cases[ast,
    CallNode[LeafNode[Symbol, "Needs" | "Get", _], {LeafNode[String, s_String, _], ___}, _] :>
      normalizeContextString[s],
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
      normalizeContextString[s],
    Infinity
  ];

  structuredHiddenDeps = structuredPackageHiddenImports[ast];
  structuredImportDeps = Lookup[structuredPackageImportRecords[ast], "context", {}];

  allDeps = DeleteDuplicates[Join[packageDeps, needsDeps, needsAliasDeps, structuredHiddenDeps, structuredImportDeps]];

  (* Filter out None values from failed parsing *)
  Select[allDeps, StringQ]
]


(*
Extract detailed context loading information from AST.
Returns a list of context load records with source locations.
*)
extractContextLoads[ast_] :=
Module[{loads, packageLoads, needsLoads, needsAliasLoads, getLoads, structuredInitLoads, structuredImportLoads, initializeCalls, importCalls},
  loads = {};

  (*
  Extract BeginPackage context and its dependencies
  *)
  packageLoads = Cases[ast,
    PackageNode[{LeafNode[String, ctx_String, KeyValuePattern[Source -> src_]], rest___}, _, _] :>
      Module[{mainCtx, deps},
        mainCtx = normalizeContextString[ctx];
        (* The main package context is implicitly "loaded" *)
        deps = Cases[{rest},
          CallNode[LeafNode[Symbol, "List", _], depList_List, _] :>
            Cases[depList, LeafNode[String, s_String, KeyValuePattern[Source -> depSrc_]] :>
              <| "context" -> normalizeContextString[s], "source" -> depSrc, "method" -> "BeginPackage" |>
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
      <| "context" -> normalizeContextString[s], "source" -> src, "method" -> "Needs" |>,
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
      <| "context" -> normalizeContextString[s], "alias" -> normalizeContextString[a], "source" -> src, "method" -> "Needs" |>,
    Infinity
  ];

  needsLoads = Join[needsLoads, needsAliasLoads];

  (*
  Extract Get["Package`"] calls with source locations
  *)
  getLoads = Cases[ast,
    CallNode[LeafNode[Symbol, "Get", _], {LeafNode[String, s_String, _], ___}, KeyValuePattern[Source -> src_]] :>
      <| "context" -> normalizeContextString[s], "source" -> src, "method" -> "Get" |>,
    Infinity
  ];

  initializeCalls = structuredPackageCallNodes[ast, "PackageInitialize"];
  structuredInitLoads = If[initializeCalls === {},
    {},
    Map[
      <|
        "context" -> #,
        "source" -> Replace[
          First[initializeCalls],
          _[_, _, KeyValuePattern[Source -> src_]] :> src
        ],
        "method" -> "PackageInitialize"
      |> &,
      structuredPackageHiddenImports[ast]
    ]
  ];

  importCalls = structuredPackageCallNodes[ast, "PackageImport"];
  structuredImportLoads = Reap[
    Scan[
      Function[{call},
        Module[{args, context, symbols},
          args = callNodeArguments[call];
          If[Length[args] >= 1,
            context = Replace[
              stringNodeValue[args[[1]]],
              {
                value_String /; validContextStringQ[value] :> normalizeContextString[value],
                _ :> None
              }
            ];
            If[StringQ[context],
              symbols = If[Length[args] >= 2,
                extractSymbolNamesFromValue[args[[2]]],
                {}
              ];
              Sow[<|
                "context" -> context,
                "source" -> Replace[
                  call,
                  _[_, _, KeyValuePattern[Source -> src_]] :> src
                ],
                "method" -> "PackageImport",
                "symbols" -> symbols
              |>]
            ]
          ]
        ]
      ],
      importCalls
    ]
  ];
  structuredImportLoads = If[Length[structuredImportLoads[[2]]] > 0,
    structuredImportLoads[[2, 1]],
    {}
  ];

  loads = Join[packageLoads, needsLoads, getLoads, structuredInitLoads, structuredImportLoads];

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
      <| "fullContext" -> normalizeContextString[fullCtx], "alias" -> normalizeContextString[aliasCtx] |>,
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
          ctx = StringRiffle[Most[parts], "`"] <> "`";
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
Module[{packageContexts, structuredContext},
  packageContexts = Cases[ast,
    PackageNode[{LeafNode[String, ctx_String, _], ___}, _, _] :> normalizeContextString[ctx],
    {0, 2}
  ];

  structuredContext = structuredPackagePackageContext[ast];

  If[Length[packageContexts] > 0,
    First[packageContexts],
    If[StringQ[structuredContext], structuredContext, None]
  ]
]


(*
Load external dependency packages so their symbols are available for context detection.
Skips packages that appear to be workspace-internal (defined in the workspace).
Only loads packages that are likely system/external packages.
*)
(*
Resolve an external package context to its WL source files and add them to
$PendingIndexFiles so their symbols become available for hover/completion.
Uses PacletFind to locate paclet-based packages; falls back to FindFile for
packages on $Path.
*)
queueExternalPackageFiles[dep_String] :=
Module[{pacletName, paclets, loc, kernelDir, searchDirs, files, newFiles},
  pacletName = First[StringSplit[dep, "`"], dep];

  paclets = Quiet[PacletFind[pacletName]];

  searchDirs = If[Length[paclets] > 0,
    loc = First[paclets]["Location"];
    (* Prefer Kernel/ subdir; fall back to the full paclet directory *)
    kernelDir = FileNameJoin[{loc, "Kernel"}];
    If[DirectoryQ[kernelDir], {kernelDir}, {loc}]
    ,
    (* Non-paclet: locate via FindFile and use the directory containing the init file *)
    With[{initFile = Quiet[FindFile[dep]]},
      If[StringQ[initFile], {DirectoryName[initFile]}, {}]
    ]
  ];

  If[Length[searchDirs] == 0, Return[]];

  files = Flatten[FileNames["*.wl" | "*.m", #, Infinity]& /@ searchDirs];
  files = Select[files, !shouldExcludeFile[#] &];

  (* Batch-append new files, skipping already-indexed and already-queued entries *)
  newFiles = Select[files,
    Function[{fp},
      !KeyExistsQ[$PacletIndex["Files"], "file://" <> fp] &&
      !MemberQ[$PendingExternalDepFiles, fp] &&
      !MemberQ[$PendingIndexFiles, fp]
    ]
  ];
  If[Length[newFiles] > 0,
    log[0, "DBG-TBDU queueExternalPackageFiles: dep=", dep, " queuing ", Length[newFiles], " files"];
    $PendingExternalDepFiles = Join[$PendingExternalDepFiles, newFiles]
  ]
]


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
        validContextStringQ[dep],
        !MemberQ[workspaceContexts, dep],
        !AnyTrue[workspaceContexts, StringStartsQ[dep, #] &],
        !TrueQ[Lookup[$LoadedExternalDependencies, dep, False]]
      ]
    ]
  ];

  (*
    Queue each external dependency's WL source files for background indexing so
    their symbols appear in hover/completion results.

    Do not call Needs[] here. External package init code can emit stdout/stderr,
    block on I/O, or otherwise interfere with the LSP transport. Source
    discovery via PacletFind / FindFile is sufficient for indexing.
  *)
  Scan[
    Function[{dep},
      $LoadedExternalDependencies[dep] = True;
      log[0, "DBG-TBDU loadExternalDependencies: queueing source files for ", dep];
      queueExternalPackageFiles[dep]
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
  docComments - Association of endLine -> parsed doc-comment (from extractDocComments)
*)
walkASTForDefinitions[bag_, node_, currentContext_, inPrivate_, uri_, docComments_, structuredContexts_] :=
Module[{newContext, contextStrings, newInPrivate, structuredPackageContext, structuredPackageScopeContext,
  structuredPrivateContext, exportedNames, scopedNames, definitionContextFor, definitionVisibilityFor},

  newContext = currentContext;
  newInPrivate = inPrivate;
  structuredPackageContext = Lookup[structuredContexts, "PackageContext", None];
  structuredPackageScopeContext = Lookup[structuredContexts, "PackageScopeContext", None];
  structuredPrivateContext = Lookup[structuredContexts, "PrivateContext", None];
  exportedNames = AssociationMap[True &, Lookup[structuredContexts, "ExportedSymbols", {}]];
  scopedNames = AssociationMap[True &, Lookup[structuredContexts, "ScopedSymbols", {}]];

  definitionContextFor[name_String] := Which[
    TrueQ[Lookup[exportedNames, name, False]] && StringQ[structuredPackageContext], structuredPackageContext,
    TrueQ[Lookup[scopedNames, name, False]] && StringQ[structuredPackageScopeContext], structuredPackageScopeContext,
    True, newContext
  ];
  definitionVisibilityFor[name_String] := Which[
    TrueQ[Lookup[exportedNames, name, False]], "public",
    TrueQ[Lookup[scopedNames, name, False]], "package",
    newInPrivate, "private",
    True, "public"
  ];

  If[StringQ[structuredPrivateContext] && !StringQ[newContext],
    newContext = structuredPrivateContext;
    newInPrivate = True
  ];

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
      Module[{src, defStartLine, docComment},
        src = node[[3, Key[Source]]];
        defStartLine = src[[1, 1]];
        (*
        A doc-comment whose last line is exactly one line before the definition start
        is considered adjacent and belongs to this definition.
        *)
        docComment = Lookup[docComments, defStartLine - 1, None];
        Module[{lhsNode, rhsNode, inferredRetPat, rhsCallHead, rhsCallArgs},
          lhsNode = node[[2, 1]];
          rhsNode = node[[2, 2]];
          (* Infer return pattern from the function body, even without a DocComment.
             This allows callers to chain return-type inference across functions that
             lack explicit Return: doc-comment annotations. *)
          inferredRetPat = inferPatternFromRHS[rhsNode, docComments, uri];
          (* Record call head/args so resolveInferredPatterns can retry using local defs *)
          {rhsCallHead, rhsCallArgs} = If[
            MatchQ[rhsNode, CallNode[LeafNode[Symbol, _String, _], _List, _]],
            {rhsNode[[1, 2]], rhsNode[[2]]},
            {None, {}}
          ];
          Scan[
            Function[{def},
              If[MatchQ[def, LeafNode[Symbol, _String, _]],
                Module[{signatureInfo},
                  signatureInfo = extractFunctionSignatureInfo[def[[2]], lhsNode];
                Internal`StuffBag[bag, <|
                  "name" -> def[[2]],
                  "uri" -> uri,
                  "source" -> src,
                  "kind" -> "function",
                  "context" -> definitionContextFor[def[[2]]],
                  "visibility" -> definitionVisibilityFor[def[[2]]],
                  "DocComment" -> docComment,
                  "InputPatterns" -> signatureInfo["InputPatterns"],
                  "Variadic" -> signatureInfo["Variadic"],
                  "HasOptionsPattern" -> signatureInfo["HasOptionsPattern"],
                  "OptionTargets" -> signatureInfo["OptionTargets"],
                  "InferredReturnPattern" -> inferredRetPat,
                  "rhsCallHead" -> rhsCallHead,
                  "rhsCallArgs" -> rhsCallArgs
                |>]
                ]
              ]
            ],
            node[[3, Key["Definitions"]]]
          ]
        ]
      ]
    ,
    (*
    TagSet/TagSetDelayed definition: tag /: Head[tag, ...] := rhs
    Store under the LHS call head (e.g. "Plot") so that return-type inference
    for  a = Plot[j]  picks up the DocComment ReturnPattern.
    *)
    MatchQ[node, CallNode[LeafNode[Symbol, "TagSetDelayed" | "TagSet", _],
      {LeafNode[Symbol, _, _], CallNode[LeafNode[Symbol, _, _], _, _], _},
      _]],
      Module[{src, defStartLine, docComment, lhsNode, rhsNode, lhsHead, signatureInfo, inferredRetPat, rhsCallHead, rhsCallArgs},
        src = node[[3, Key[Source]]];
        defStartLine = src[[1, 1]];
        docComment = Lookup[docComments, defStartLine - 1, None];
        lhsNode = node[[2, 2]];
        rhsNode = node[[2, 3]];
        lhsHead = lhsNode[[1, 2]];
        signatureInfo = extractFunctionSignatureInfo[lhsHead, lhsNode];
        inferredRetPat = inferPatternFromRHS[rhsNode, docComments, uri];
        {rhsCallHead, rhsCallArgs} = If[
          MatchQ[rhsNode, CallNode[LeafNode[Symbol, _String, _], _List, _]],
          {rhsNode[[1, 2]], rhsNode[[2]]},
          {None, {}}
        ];
        Internal`StuffBag[bag, <|
          "name" -> lhsHead,
          "uri" -> uri,
          "source" -> src,
          "kind" -> "function",
          "context" -> definitionContextFor[lhsHead],
          "visibility" -> definitionVisibilityFor[lhsHead],
          "DocComment" -> docComment,
          "InputPatterns" -> signatureInfo["InputPatterns"],
          "Variadic" -> signatureInfo["Variadic"],
          "HasOptionsPattern" -> signatureInfo["HasOptionsPattern"],
          "OptionTargets" -> signatureInfo["OptionTargets"],
          "InferredReturnPattern" -> inferredRetPat,
          "rhsCallHead" -> rhsCallHead,
          "rhsCallArgs" -> rhsCallArgs
        |>]
      ]
    ,
    (*
    Constant definition: c = 5
    *)
    MatchQ[node, CallNode[LeafNode[Symbol, "Set", _], {LeafNode[Symbol, _, _], _}, KeyValuePattern["Definitions" -> _List]]],
      Module[{src, defStartLine, docComment, rhsNode, inferredPat, rhsCallHead, rhsCallArgs},
        src = node[[3, Key[Source]]];
        defStartLine = src[[1, 1]];
        docComment = Lookup[docComments, defStartLine - 1, None];
        rhsNode = node[[2, 2]];
        inferredPat = inferPatternFromRHS[rhsNode, docComments, uri];
        (* Record call head/args so resolveInferredPatterns can retry using local defs *)
        {rhsCallHead, rhsCallArgs} = If[
          MatchQ[rhsNode, CallNode[LeafNode[Symbol, _String, _], _List, _]],
          {rhsNode[[1, 2]], rhsNode[[2]]},
          {None, {}}
        ];
        Scan[
          Function[{def},
            If[MatchQ[def, LeafNode[Symbol, _String, _]],
              Internal`StuffBag[bag, <|
                "name" -> def[[2]],
                "uri" -> uri,
                "source" -> src,
                "kind" -> "constant",
                "context" -> definitionContextFor[def[[2]]],
                "visibility" -> definitionVisibilityFor[def[[2]]],
                "DocComment" -> docComment,
                "InferredPattern" -> inferredPat,
                "rhsCallHead" -> rhsCallHead,
                "rhsCallArgs" -> rhsCallArgs
              |>]
            ]
          ],
          node[[3, Key["Definitions"]]]
        ]
      ]
    ,
    (*
    Options definition: Options[f] = {...}
    *)
    MatchQ[node, CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _],
      {CallNode[LeafNode[Symbol, "Options", _], {LeafNode[Symbol, _String, _]}, _], _}, _]],
      Module[{optionData},
        optionData = extractOptionDefinitionData[node[[2, 2]]];
        Internal`StuffBag[bag, <|
          "name" -> node[[2, 1, 2, 1, 2]],
          "uri" -> uri,
          "source" -> node[[3, Key[Source]]],
          "kind" -> "option",
          "context" -> definitionContextFor[node[[2, 1, 2, 1, 2]]],
          "visibility" -> definitionVisibilityFor[node[[2, 1, 2, 1, 2]]],
          "DocComment" -> None,
          "OptionNames" -> optionData["OptionNames"],
          "OptionTargets" -> optionData["OptionTargets"]
        |>]
      ]
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
        "context" -> definitionContextFor[node[[2, 1, 2, 1, 2]]],
        "visibility" -> definitionVisibilityFor[node[[2, 1, 2, 1, 2]]],
        "DocComment" -> None
      |>]
  ];

  (*
  Recurse into children
  Note: Switch patterns do NOT bind variables, so we must use node[[2]] directly
  *)
  Switch[node,
    ContainerNode[_, _List, _],
      Scan[walkASTForDefinitions[bag, #, newContext, newInPrivate, uri, docComments, structuredContexts]&, node[[2]]]
    ,
    PackageNode[_, _List, _],
      Scan[walkASTForDefinitions[bag, #, newContext, newInPrivate, uri, docComments, structuredContexts]&, node[[2]]]
    ,
    ContextNode[_, _List, _],
      Scan[walkASTForDefinitions[bag, #, newContext, newInPrivate, uri, docComments, structuredContexts]&, node[[2]]]
    ,
    CallNode[_, _List, _],
      Scan[walkASTForDefinitions[bag, #, newContext, newInPrivate, uri, docComments, structuredContexts]&, node[[2]]]
    ,
    _[_, _List, _],
      Scan[walkASTForDefinitions[bag, #, newContext, newInPrivate, uri, docComments, structuredContexts]&, node[[2]]]
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

abstractContextString[str_String] := str

abstractContextString[_] := None


validContextStringQ[str_String] :=
  StringEndsQ[str, "`"]

validContextStringQ[_] := False


normalizeContextString[str_] :=
Module[{context = abstractContextString[str]},
  If[validContextStringQ[context],
    context,
    None
  ]
]


topLevelExpressions[
  ContainerNode[_, expressions_List, _]
] := expressions
topLevelExpressions[_] := {}


structuredPackageCallNodes[ast_, name_String] :=
  DeleteDuplicates @ Cases[
    topLevelExpressions[ast],
    callNode : CallNode[
      LeafNode[Symbol, symbolName_String, _],
      _,
      _
    ] /; symbolName === name :> callNode,
    {0, Infinity}
  ]


callNodeArguments[
  CallNode[
    _,
    args_List,
    _
  ]
] := args
callNodeArguments[_] := {}


stringNodeValue[
  LeafNode[String, value_String, _]
] := StringTrim[value, "\""]
stringNodeValue[_] := Missing["NotAvailable"]


symbolNodeValue[
  LeafNode[Symbol, value_String, _]
] := value
symbolNodeValue[_] := Missing["NotAvailable"]


associationRuleKeyString[
  BinaryNode[Rule, {
    keyNode_,
    _
  }, _]
] :=
  Replace[
    keyNode,
    {
      LeafNode[String, value_String, _] :> StringTrim[value, "\""],
      LeafNode[Symbol, value_String, _] :> value,
      _ :> Missing["NotAvailable"]
    }
  ]
associationRuleKeyString[
  CallNode[
    LeafNode[Symbol, "Rule" | "RuleDelayed", _],
    {
      keyNode_,
      _
    },
    _
  ]
] :=
  Replace[
    keyNode,
    {
      LeafNode[String, value_String, _] :> StringTrim[value, "\""],
      LeafNode[Symbol, value_String, _] :> value,
      _ :> Missing["NotAvailable"]
    }
  ]
associationRuleKeyString[_] := Missing["NotAvailable"]


extractContextStringsFromValue[node_] :=
  DeleteDuplicates @ Cases[
    node,
    LeafNode[String, value_String, _] /; validContextStringQ[StringTrim[value, "\""]] :>
      normalizeContextString[StringTrim[value, "\""]],
    {0, Infinity}
  ]


extractSymbolNamesFromValue[node_] :=
  DeleteDuplicates @ Cases[
    node,
    LeafNode[Symbol, value_String, _] /;
      !StringContainsQ[value, "`"] &&
      !MemberQ[{"List", "Association", "Rule", "RuleDelayed"}, value] :> value,
    {0, Infinity}
  ]


parseAst[filePath_String, fileFormat_String] :=
Module[{text, cst, agg},
  text = Quiet[Import[filePath, "Text"]];
  If[!StringQ[text],
    Return[$Failed]
  ];

  cst = Quiet[CodeConcreteParse[text, "FileFormat" -> fileFormat]];
  If[FailureQ[cst],
    Return[$Failed]
  ];

  agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
  Quiet[CodeParser`Abstract`Abstract[agg]]
]


structuredPackageInitializeMetadata[ast_] :=
Module[{initializeCalls, args, packageContext, optionsArg, hiddenImports},
  initializeCalls = structuredPackageCallNodes[ast, "PackageInitialize"];
  If[initializeCalls === {},
    Return[<||>]
  ];

  args = callNodeArguments[First[initializeCalls]];
  packageContext = If[Length[args] >= 1,
    Replace[
      stringNodeValue[args[[1]]],
      {
        value_String /; validContextStringQ[value] :> normalizeContextString[value],
        _ :> Missing["NotAvailable"]
      }
    ],
    Missing["NotAvailable"]
  ];

  optionsArg = If[Length[args] >= 2, args[[2]], Missing["NotAvailable"]];
  hiddenImports = DeleteDuplicates @ Flatten @ Cases[
    optionsArg,
    rule : (BinaryNode[Rule, {_, _}, _] | CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], {_, _}, _]) /;
        associationRuleKeyString[rule] === "HiddenImports" :>
      extractContextStringsFromValue[rule],
    Infinity
  ];

  <|
    "PackageContext" -> packageContext,
    "HiddenImports" -> hiddenImports
  |>
]


structuredPackagePackageContext[ast_] :=
  Lookup[structuredPackageInitializeMetadata[ast], "PackageContext", Missing["NotAvailable"]]


structuredPackageHiddenImports[ast_] :=
  DeleteDuplicates @ Lookup[structuredPackageInitializeMetadata[ast], "HiddenImports", {}]


structuredPackageImportRecords[ast_] :=
Module[{calls, args, context, symbols, reaped},
  calls = structuredPackageCallNodes[ast, "PackageImport"];
  reaped = Reap[
    Scan[
      Function[{call},
        args = callNodeArguments[call];
        If[Length[args] >= 1,
          context = Replace[
            stringNodeValue[args[[1]]],
            {
              value_String /; validContextStringQ[value] :> normalizeContextString[value],
              _ :> Missing["NotAvailable"]
            }
          ];
          If[StringQ[context],
            symbols = If[Length[args] >= 2,
              extractSymbolNamesFromValue[args[[2]]],
              {}
            ];
            Sow[<|
              "context" -> context,
              "symbols" -> symbols
            |>]
          ]
        ]
      ],
      calls
    ]
  ];
  If[Length[reaped[[2]]] == 0,
    Return[{}]
  ];

  DeleteDuplicatesBy[
    reaped[[2, 1]],
    {Lookup[#, "context", Missing[]], Sort[Lookup[#, "symbols", {}]]} &
  ]
]


structuredPackageDeclaredSymbols[ast_, uri_String, callName_String, visibility_String, context_String] :=
Module[{calls},
  If[!StringQ[context] || context === "",
    Return[{}]
  ];

  calls = structuredPackageCallNodes[ast, callName];
  Flatten[
    Map[
      Function[{call},
        Map[
          <|
            "name" -> #,
            "uri" -> uri,
            "visibility" -> visibility,
            "context" -> context,
            "kind" -> "declaration",
            "source" -> Replace[
              call,
              _[_, _, KeyValuePattern[Source -> src_]] :> src
            ]
          |> &,
          DeleteDuplicates @ Flatten[extractSymbolNamesFromValue /@ callNodeArguments[call]]
        ]
      ],
      calls
    ],
    1
  ]
]
structuredPackageDeclaredSymbols[_, _, _, _, _] := {}


structuredPackageContextPathParts[packageContext_String] :=
  Select[StringSplit[StringTrim[packageContext, "`"], "`"], # =!= "" &]
structuredPackageContextPathParts[_] := {}


structuredPackagePrivateContext[filePath_String, packageContext_String, loaderPath_String] :=
Module[{packageParts, relativeDir, relativeParts, contextParts},
  If[!StringQ[filePath] || !StringQ[packageContext] || !StringQ[loaderPath],
    Return[Missing["NotAvailable"]]
  ];

  packageParts = structuredPackageContextPathParts[packageContext];
  relativeDir = Quiet @ Check[
    FileNameDrop[DirectoryName[filePath], FileNameDepth[DirectoryName[loaderPath]]],
    DirectoryName[filePath]
  ];
  relativeParts = Select[FileNameSplit[relativeDir], # =!= "" && # =!= "." &];
  contextParts = Join[packageParts, relativeParts, {FileBaseName[filePath], "Private"}];
  StringRiffle[contextParts, "`"] <> "`"
]
structuredPackagePrivateContext[_, _, _] := Missing["NotAvailable"]


structuredPackageLoaderPath[root_String] :=
Module[{cached, searchRoots, candidateForRoot},
  cached = Lookup[$StructuredPackageLoaderCache, root, Missing["NotAvailable"]];
  If[cached =!= Missing["NotAvailable"],
    Return[cached]
  ];

  searchRoots = NestWhileList[DirectoryName, root, # =!= DirectoryName[#] &, 1, 4];

  candidateForRoot[searchRoot_String] :=
    Module[{candidates},
      candidates = Select[
        DeleteDuplicates @ Join[
          FileNames["init.wl", searchRoot, 1],
          FileNames["*.wl" | "*.m", searchRoot, 1]
        ],
        !shouldExcludeFile[#] &
      ];
      SelectFirst[
        SortBy[candidates, {If[FileNameTake[#] === "init.wl", 0, 1] &, StringLength}],
        StringQ[structuredPackagePackageContext @ parseAst[#, LSPServer`SourceFileFormat[#]]] &,
        Missing["NotAvailable"]
      ]
    ];

  cached = SelectFirst[
    candidateForRoot /@ searchRoots,
    StringQ,
    Missing["NotAvailable"]
  ];
  $StructuredPackageLoaderCache[root] = cached;
  cached
]


structuredPackageMetadata[filePath_String, ast_] :=
Module[{packageContext, hiddenImports, importRecords, loaderPath, privateContext, packageScopeContext, loaderAst},
  packageContext = structuredPackagePackageContext[ast];
  hiddenImports = structuredPackageHiddenImports[ast];
  importRecords = structuredPackageImportRecords[ast];
  loaderPath = Missing["NotAvailable"];

  If[!StringQ[packageContext],
    loaderPath = structuredPackageLoaderPath[DirectoryName[filePath]];
    If[StringQ[loaderPath],
      loaderAst = parseAst[loaderPath, LSPServer`SourceFileFormat[loaderPath]];
      packageContext = structuredPackagePackageContext[loaderAst];
      hiddenImports = structuredPackageHiddenImports[loaderAst]
    ]
  ];

  If[StringQ[packageContext] && !StringQ[loaderPath],
    loaderPath = filePath
  ];

  privateContext = If[StringQ[packageContext] && StringQ[loaderPath],
    structuredPackagePrivateContext[filePath, packageContext, loaderPath],
    Missing["NotAvailable"]
  ];
  packageScopeContext = If[StringQ[packageContext], packageContext <> "PackageScope`", Missing["NotAvailable"]];

  <|
    "PackageContext" -> packageContext,
    "PackageScopeContext" -> packageScopeContext,
    "PrivateContext" -> privateContext,
    "HiddenImports" -> hiddenImports,
    "ImportRecords" -> importRecords,
    "LoaderPath" -> loaderPath
  |>
]


mergeStructuredDependencyData[fileDeps_List, contextLoads_List, structuredMetadata_Association] :=
Module[{hiddenImports, mergedLoads},
  hiddenImports = Lookup[structuredMetadata, "HiddenImports", {}];
  mergedLoads = Join[
    contextLoads,
    Map[
      <|"context" -> #, "source" -> {{1, 1}, {1, 1}}, "method" -> "PackageInitialize"|> &,
      hiddenImports
    ]
  ];

  <|
    "Dependencies" -> DeleteDuplicates[Join[fileDeps, hiddenImports]],
    "ContextLoads" -> DeleteDuplicatesBy[
      mergedLoads,
      {
        Lookup[#, "method", None],
        Lookup[#, "context", None],
        Sort[Lookup[#, "symbols", {}]],
        Lookup[#, "alias", None]
      } &
    ]
  |>
]


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
Module[{cst, agg, ast, filePath, definitions, usages, symbols, fileDeps,
  contextLoads, explicitContextRefs, structuredMetadata, mergedStructuredData,
  packageContext, packageScopeContext, privateContext, fileAliases, fileFormat,
  isIPWL, ipwlAnnotations, sourceText},

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
  Pre-process .ipwl files: strip ": ret" annotations into IPWLReturn/IPWLDeclare
  comments so the CodeParser sees valid WL, and collect DeclaredType annotations.
  *)
  isIPWL = StringEndsQ[filePath, ".ipwl"];
  If[isIPWL,
    Module[{preprocessResult},
      preprocessResult = LSPServer`TypeWL`PreprocessIPWL[text];
      sourceText = preprocessResult[[1]];
      ipwlAnnotations = preprocessResult[[2]];
    ],
    sourceText = text;
    ipwlAnnotations = {};
  ];

  RemoveFileFromIndex[uri];

  (*
  Parse the new content.
  Match the FileFormat option used by textDocument/concreteParse so the CST
  can be shared with the diagnostics pipeline and avoid a duplicate parse.
  *)
  fileFormat = LSPServer`SourceFileFormat[filePath];
  cst = Quiet[CodeConcreteParse[sourceText, "FileFormat" -> fileFormat]];

  If[FailureQ[cst],
    If[$Debug2, log["UpdateFileIndex: failed to parse"]];
    Throw[Null]
  ];

  (*
  Apply the same ContainerNode head mutation that textDocument/concreteParse does
  so the stored CST is in the expected form for the diagnostics pipeline.
  *)
  cst[[1]] = File;

  agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
  ast = Quiet[CodeParser`Abstract`Abstract[agg]];

  If[FailureQ[ast],
    If[$Debug2, log["UpdateFileIndex: failed to abstract"]];
    Throw[Null]
  ];

  definitions = extractDefinitions[ast, cst, uri];
  usages = extractUsages[ast, uri];
  symbols = extractSymbolReferences[cst, uri];
  fileDeps = extractDependencies[ast];
  contextLoads = extractContextLoads[ast];
  explicitContextRefs = extractExplicitContextRefs[cst];
  structuredMetadata = structuredPackageMetadata[filePath, ast];
  mergedStructuredData = mergeStructuredDependencyData[fileDeps, contextLoads, structuredMetadata];
  fileDeps = Lookup[mergedStructuredData, "Dependencies", fileDeps];
  contextLoads = Lookup[mergedStructuredData, "ContextLoads", contextLoads];
  packageContext = Lookup[structuredMetadata, "PackageContext", extractPackageContext[ast]];
  packageScopeContext = Lookup[structuredMetadata, "PackageScopeContext", None];
  privateContext = Lookup[structuredMetadata, "PrivateContext", None];
  fileAliases = extractContextAliases[ast];

  If[Length[fileDeps] > 0,
    $PacletIndex["Dependencies"] = DeleteDuplicates[
      Join[$PacletIndex["Dependencies"], fileDeps]
    ];
    Scan[queueExternalPackageFiles, fileDeps]
  ];

  addFileToIndex[
    uri,
    definitions,
    usages,
    symbols,
    fileDeps,
    contextLoads,
    explicitContextRefs,
    packageContext,
    packageScopeContext,
    privateContext,
    fileAliases
  ];

  (*
  For .ipwl files: inject DeclaredType entries from pre-processor annotations into
  the PacletIndex, and tag the Files entry with IsIPWL -> True.
  *)
  If[isIPWL,
    Scan[
      Function[{ann},
        If[ann["kind"] === "DeclaredType",
          Module[{sym = ann["symbol"], dt = ann["DeclaredType"],
                  declEntry, existingDefs},
            If[!KeyExistsQ[$PacletIndex["Symbols"], sym],
              $PacletIndex["Symbols", sym] = <|
                "Definitions" -> {}, "References" -> {}, "Usages" -> {}
              |>
            ];
            existingDefs = $PacletIndex["Symbols", sym, "Definitions"];
            (* Replace existing declaration entry for this URI, or append a new one *)
            declEntry = SelectFirst[existingDefs,
              #["uri"] === uri && #["kind"] === "declaration" &,
              None
            ];
            If[declEntry === None,
              $PacletIndex["Symbols", sym, "Definitions"] =
                Append[existingDefs,
                  <|"uri" -> uri,
                    "source" -> {{ann["line"], 1}, {ann["line"], 1}},
                    "kind" -> "declaration",
                    "DeclaredType" -> dt,
                    "InferredPattern" -> None|>]
            ,
              $PacletIndex["Symbols", sym, "Definitions"] =
                Replace[existingDefs,
                  declEntry -> Append[KeyDrop[declEntry, "DeclaredType"], "DeclaredType" -> dt],
                  {1}]
            ]
          ]
        ]
      ],
      ipwlAnnotations
    ];
    (* Mark file as IPWL in the Files entry *)
    If[KeyExistsQ[$PacletIndex["Files"], uri],
      $PacletIndex["Files", uri, "IsIPWL"] = True
    ]
  ];

  (*
  Return the parsed artifacts so callers can store them in $OpenFilesMap,
  allowing textDocument/concreteParse, textDocument/aggregateParse, and
  textDocument/abstractParse to skip their redundant re-parse steps.
  *)
  {cst, agg, ast}
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
  Remove this file's entry from the workspace cache so stale data is not
  restored on the next server startup.
  *)
  removeWorkspaceCacheEntry[uri];

  If[AssociationQ[LSPServer`$ClosedFileDiagnosticsNotifications],
    LSPServer`$ClosedFileDiagnosticsNotifications =
      KeyDrop[LSPServer`$ClosedFileDiagnosticsNotifications, uri]
  ];

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
Get the definitions for a symbol that are visible from a specific file.
This prefers current-file definitions, then file-visible SPF contexts, and
respects an explicit context hint when one is present in the source.
*)
GetVisibleSymbolDefinitions[uri_String, symbolName_String, preferredContext_:None] :=
Module[{defs, resolvedPreferredContext, sameFileDefs, fileData, contextLoads,
  byNameImportContexts, byNameImportContextSet, bareContextSet,
  packageContext, packageScopeContext, privateContext, visibleRank, visibleDefs},

  defs = GetSymbolDefinitions[symbolName];
  If[!ListQ[defs] || Length[defs] === 0,
    Return[{}]
  ];

  resolvedPreferredContext = Replace[preferredContext,
    {
      ctx_String :> Lookup[$PacletIndex["ContextAliases"], ctx, ctx],
      _ :> None
    }
  ];

  If[StringQ[resolvedPreferredContext],
    Return[Select[defs, Lookup[#, "context", None] === resolvedPreferredContext &]]
  ];

  sameFileDefs = Select[defs, Lookup[#, "uri", None] === uri &];
  If[Length[sameFileDefs] > 0,
    Return[sameFileDefs]
  ];

  fileData = Lookup[$PacletIndex["Files"], uri, <||>];
  If[fileData === <||>,
    Return[defs]
  ];

  contextLoads = Lookup[fileData, "ContextLoads", {}];
  byNameImportContexts = DeleteDuplicates @ Select[
    Cases[
      contextLoads,
      rec_ /; MemberQ[Lookup[rec, "symbols", {}], symbolName] :>
        Lookup[rec, "context", Missing["NotAvailable"]]
    ],
    StringQ
  ];
  byNameImportContextSet = Association[Thread[byNameImportContexts -> True]];
  bareContextSet = Association[Thread[GetFileBareNameContexts[uri] -> True]];
  packageContext = Lookup[fileData, "PackageContext", None];
  packageScopeContext = Lookup[fileData, "PackageScopeContext", None];
  privateContext = Lookup[fileData, "PrivateContext", None];

  visibleRank[def_] :=
    Module[{ctx},
      ctx = Lookup[def, "context", None];
      Which[
        Lookup[def, "uri", None] === uri, 0,
        StringQ[ctx] && KeyExistsQ[byNameImportContextSet, ctx], 1,
        StringQ[ctx] && ctx === privateContext, 2,
        StringQ[ctx] && ctx === packageScopeContext, 3,
        StringQ[ctx] && ctx === packageContext, 4,
        StringQ[ctx] && KeyExistsQ[bareContextSet, ctx], 5,
        True, 6
      ]
    ];

  visibleDefs = Select[defs, visibleRank[#] < 6 &];
  If[Length[visibleDefs] > 0,
    SortBy[visibleDefs, visibleRank],
    defs
  ]
]


GetSymbolOptionNames[symbolName_String] :=
  getIndexedSymbolOptionNames[symbolName]


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
      kind = Which[
        AnyTrue[defs, Lookup[#, "kind", None] === "function" &], "function",
        AnyTrue[defs, Lookup[#, "kind", None] === "constant" &], "constant",
        AnyTrue[defs, Lookup[#, "kind", None] === "option" &], "option",
        AnyTrue[defs, Lookup[#, "kind", None] === "attribute" &], "attribute",
        Length[defs] > 0, defs[[1]]["kind"],
        True, "unknown"
      ];
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
  If[!StringQ[$WorkspaceRoot],
    Return[Select[
      Keys[$PacletIndex["Symbols"]],
      Length[Lookup[$PacletIndex["Symbols", #], "Definitions", {}]] > 0 &
    ]]
  ];
  Select[
    Keys[$PacletIndex["Symbols"]],
    IsWorkspaceSymbol[#] &
  ]
]


(* Check whether a symbol has at least one definition in the workspace root. *)
IsWorkspaceSymbol[symbolName_String] :=
Module[{defs},
  If[!KeyExistsQ[$PacletIndex["Symbols"], symbolName],
    Return[False]
  ];

  defs = Lookup[$PacletIndex["Symbols", symbolName], "Definitions", {}];
  AnyTrue[defs, isWorkspaceURI[Lookup[#, "uri", ""]] &]
]


(* Check whether a symbol has at least one indexed definition outside the workspace root. *)
IsIndexedDependencySymbol[symbolName_String] :=
Module[{defs},
  If[!KeyExistsQ[$PacletIndex["Symbols"], symbolName],
    Return[False]
  ];

  defs = Lookup[$PacletIndex["Symbols", symbolName], "Definitions", {}];
  AnyTrue[defs,
    StringQ[Lookup[#, "uri", ""]] && !isWorkspaceURI[Lookup[#, "uri", ""]] &
  ]
]


(* Get all indexed symbol short names that come from dependency files. *)
GetIndexedDependencySymbols[] :=
Module[{},
  If[!StringQ[$WorkspaceRoot],
    Return[{}]
  ];
  Select[
    Keys[$PacletIndex["Symbols"]],
    IsIndexedDependencySymbol[#] &
  ]
]


(*
Get short-name symbols from dependency contexts that are actually loaded in the kernel.
Used by UndefinedSymbol diagnostics to suppress false positives for symbols
from BeginPackage/Needs dependencies that are available at runtime.
*)
GetLoadedDependencySymbols[uri_String] :=
Module[{fileData, contextLoads, depContexts, explicitImports, kernelContexts, loadedCtxs,
  indexedSymbols, kernelSymbols},
  fileData = Lookup[$PacletIndex["Files"], uri, <||>];
  If[fileData === <||>, Return[{}]];

  contextLoads = Lookup[fileData, "ContextLoads", {}];
  depContexts = DeleteDuplicates @ Lookup[
    Select[contextLoads, Lookup[#, "symbols", {}] === {} &],
    "context",
    {}
  ];
  explicitImports = DeleteDuplicates @ Flatten @ Lookup[
    Select[contextLoads, Lookup[#, "symbols", {}] =!= {} &],
    "symbols",
    {}
  ];

  (* Only include contexts actually present in the kernel — i.e. successfully loaded *)
  kernelContexts = GetKernelContextsCached[];
  loadedCtxs = Select[depContexts, MemberQ[kernelContexts, #]&];
  indexedSymbols = DeleteDuplicates @ Flatten[Lookup[$PacletIndex["Contexts"], depContexts, {}]];
  kernelSymbols = DeleteDuplicates @ Flatten[
    Function[{ctx}, StringReplace[Names[ctx <> "*"], ctx -> ""]] /@ loadedCtxs
  ];

  DeleteDuplicates@Join[explicitImports, indexedSymbols, kernelSymbols]
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
GetSymbolContext[uri_String, symbolName_String, preferredContext_:None] :=
Module[{defs},
  defs = GetVisibleSymbolDefinitions[uri, symbolName, preferredContext];

  If[Length[defs] === 0,
    None,
    Replace[defs[[1]]["context"], _Missing -> None]
  ]
]


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
- All kernel-known contexts (from Contexts[]) - these are always available
  without needing Needs[]. This covers System`, Global`, Developer`, Internal`,
  Compile`, JLink`, and many others.
- The file's own package context (if it's a package)
- Contexts loaded via Needs/Get/BeginPackage
- Workspace-defined contexts
*)
(*
Cached kernel contexts - Contexts[] is expensive and the result changes
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

GetFileBareNameContexts[uri_String] :=
Module[{fileData, contextLoads, packageContext, packageScopeContext, privateContext, bareContexts},

  fileData = Lookup[$PacletIndex["Files"], uri, <||>];
  bareContexts = GetKernelContextsCached[];

  If[fileData === <||>,
    Return[DeleteDuplicates[bareContexts]]
  ];

  packageContext = Lookup[fileData, "PackageContext", None];
  packageScopeContext = Lookup[fileData, "PackageScopeContext", None];
  privateContext = Lookup[fileData, "PrivateContext", None];
  If[StringQ[packageContext],
    AppendTo[bareContexts, packageContext]
  ];
  If[StringQ[packageScopeContext],
    AppendTo[bareContexts, packageScopeContext]
  ];
  If[StringQ[privateContext],
    AppendTo[bareContexts, privateContext],
    If[StringQ[packageContext],
      AppendTo[bareContexts, packageContext <> "Private`"]
    ]
  ];

  contextLoads = Lookup[fileData, "ContextLoads", {}];
  bareContexts = Join[
    bareContexts,
    Lookup[
      Select[contextLoads, Lookup[#, "symbols", {}] === {} &],
      "context",
      {}
    ]
  ];

  DeleteDuplicates[bareContexts]
]

GetFileLoadedContexts[uri_String] :=
Module[{fileData, contextLoads, loadedContexts, workspaceContexts},

  fileData = Lookup[$PacletIndex["Files"], uri, <||>];

  If[fileData === <||>,
    Return[DeleteDuplicates[GetKernelContextsCached[]]]
  ];

  loadedContexts = GetFileBareNameContexts[uri];

  (* Add contexts from Needs/Get/BeginPackage/PackageImport, including by-name imports. *)
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
