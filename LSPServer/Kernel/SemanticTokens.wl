BeginPackage["LSPServer`SemanticTokens`"]

$SemanticTokenTypes

$SemanticTokenModifiers

computeAndCacheSemanticTokens

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`PacletIndex`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Scoping`"]
Needs["CodeParser`Utils`"]


(*
Semantic token types for LSP
Extended to support paclet-wide symbol classification
*)
$SemanticTokenTypes = <|
  "variable" -> 0,        (* Local variables from Module/Block *)
  "parameter" -> 1,       (* Function parameters *)
  "function" -> 2,        (* Functions (system or paclet-defined) *)
  "constant" -> 3,        (* Constants from With or system constants *)
  "class" -> 4,           (* Paclet-defined symbols *)
  "type" -> 5,            (* System symbols *)
  "property" -> 6,        (* Options *)
  "comment" -> 7,         (* Undefined/unknown symbols *)
  "string" -> 8,          (* Reserved *)
  "deprecated" -> 9       (* Experimental/obsolete symbols *)
|>

$SemanticTokenModifiers = <|
  "Module" -> 0,
  "Block" -> 1,
  "With" -> 2,
  "shadowed" -> 3,
  "unused" -> 4,
  "error" -> 5,           (* Error/undefined symbol *)
  "definition" -> 6,
  "readonly" -> 7,        (* System symbol - cannot be modified *)
  "defaultLibrary" -> 8   (* System/builtin symbol *)
|>

(*
Cached sets for fast O(1) lookup
*)
$systemSymbolsSet := $systemSymbolsSet = Association[Thread[
  Join[
    WolframLanguageSyntax`Generate`$builtinFunctions,
    WolframLanguageSyntax`Generate`$constants
  ] -> True
]]

$systemConstantsSet := $systemConstantsSet = Association[Thread[
  WolframLanguageSyntax`Generate`$constants -> True
]]

$systemOptionsSet := $systemOptionsSet = Association[Thread[
  WolframLanguageSyntax`Generate`$options -> True
]]

$experimentalSymbolsSet := $experimentalSymbolsSet = Association[Thread[
  WolframLanguageSyntax`Generate`$experimentalSymbols -> True
]]

$obsoleteSymbolsSet := $obsoleteSymbolsSet = Association[Thread[
  WolframLanguageSyntax`Generate`$obsoleteSymbols -> True
]]

$undocumentedSymbolsSet := $undocumentedSymbolsSet = Association[Thread[
  WolframLanguageSyntax`Generate`$undocumentedSymbols -> True
]]

$sessionSymbolsSet := $sessionSymbolsSet = Association[Thread[
  WolframLanguageSyntax`Generate`$sessionSymbols -> True
]]

$badSymbolsSet := $badSymbolsSet = Association[Thread[
  WolframLanguageSyntax`Generate`$badSymbols -> True
]]

(*
Check if a symbol is defined in the paclet
*)
isPacletSymbol[name_String] :=
  TrueQ[LSPServer`PacletIndex`IsWorkspaceSymbol[name]]

(*
Check if a symbol is a system symbol
*)
isSystemSymbol[name_String] := KeyExistsQ[$systemSymbolsSet, name]

(*
Check if a symbol is a system constant
*)
isSystemConstant[name_String] := KeyExistsQ[$systemConstantsSet, name]

(*
Check if a symbol is an option
*)
isSystemOption[name_String] := KeyExistsQ[$systemOptionsSet, name]

(*
Check if a symbol is experimental (valid System` symbol but not yet stable)
*)
isExperimentalSymbol[name_String] := KeyExistsQ[$experimentalSymbolsSet, name]

(*
Check if a symbol is obsolete/deprecated
*)
isObsoleteSymbol[name_String] := KeyExistsQ[$obsoleteSymbolsSet, name]

(*
Check if a symbol is undocumented (exists in System` but has no public docs)
*)
isUndocumentedSymbol[name_String] := KeyExistsQ[$undocumentedSymbolsSet, name]

(*
Check if a symbol is a session symbol (Echo, Print, In, Out, etc.)
*)
isSessionSymbol[name_String] := KeyExistsQ[$sessionSymbolsSet, name]

(*
Check if a symbol is a commonly confused / "bad" symbol name
*)
isBadSymbol[name_String] := KeyExistsQ[$badSymbolsSet, name]

(*
Check if a symbol is from an external loaded dependency package.
This checks the actual Context[] of the symbol at runtime.
Works for both bare symbols (e.g., "OpenSQLConnection") and
explicitly contexted symbols (e.g., "DatabaseLink`OpenSQLConnection").

Performance: This is called for every non-system symbol during semantic token
classification. The dependency list and set are cached to avoid repeated lookups.
*)

(* Cached dependency data - invalidated when dependency contexts, aliases, or
   indexed dependency symbol names change. *)
$depsCacheKey = None;
$depsSet = <||>;
$depsList = {};
$indexedDependencySymbolSet = <||>;

refreshDepsCache[] :=
Module[{deps, indexedSymbols, cacheKey},
  deps = LSPServer`PacletIndex`GetDependencyContexts[];
  indexedSymbols = LSPServer`PacletIndex`GetIndexedDependencySymbols[];
  cacheKey = {
    Sort[deps],
    Sort[indexedSymbols],
    Sort[Keys[LSPServer`PacletIndex`GetContextAliases[]]]
  };
  If[cacheKey =!= $depsCacheKey,
    $depsCacheKey = cacheKey;
    $depsList = deps;
    $depsSet = Association[Thread[deps -> True]];
    $indexedDependencySymbolSet = Association[Thread[indexedSymbols -> True]]
  ]
]

isExternalDependencySymbol[name_String] :=
Module[{symContext, explicitContext},

  refreshDepsCache[];

  If[Length[$depsList] == 0 && Length[$indexedDependencySymbolSet] == 0,
    Return[False]
  ];

  If[!StringContainsQ[name, "`"] && KeyExistsQ[$indexedDependencySymbolSet, name],
    Return[True]
  ];

  (* Check if symbol has explicit context *)
  If[StringContainsQ[name, "`"],
    (* Extract the context from the explicit name *)
    explicitContext = StringJoin[Riffle[Most[StringSplit[name, "`"]], "`"]] <> "`";

    (* Fast exact match first, then substring check *)
    If[KeyExistsQ[$depsSet, explicitContext] ||
       AnyTrue[$depsList, StringStartsQ[explicitContext, #] &],
      Return[True]
    ];

    (* Resolve alias context (e.g. Alias` -> Full`) and re-check *)
    With[{aliasMap = LSPServer`PacletIndex`GetContextAliases[]},
      If[KeyExistsQ[aliasMap, explicitContext],
        With[{fullCtx = aliasMap[explicitContext]},
          Return[KeyExistsQ[$depsSet, fullCtx] ||
                 AnyTrue[$depsList, StringStartsQ[fullCtx, #] &]]
        ]
      ]
    ];

    Return[False]
  ];

  (* For bare symbols, try to get the symbol's context at runtime *)
  symContext = Quiet[
    Check[Context[name], None, {Context::notfound}],
    {Context::notfound}
  ];

  If[!StringQ[symContext] || symContext === "Global`" || symContext === "System`",
    Return[False]
  ];

  (* Fast exact match first, then substring check *)
  KeyExistsQ[$depsSet, symContext] ||
  AnyTrue[$depsList, StringStartsQ[symContext, #] &]
]

(*
Classify a symbol that's not in local scope.
Handles both bare symbols and explicitly contexted symbols.
*)
classifyGlobalSymbol[name_String] :=
Module[{bareSymbol},
  (* Extract bare symbol name for system symbol checks *)
  bareSymbol = If[StringContainsQ[name, "`"],
    Last[StringSplit[name, "`"]],
    name
  ];

  Which[
    (* Check system symbols using bare name *)
    isSystemConstant[bareSymbol],
      {"constant", {"readonly", "defaultLibrary"}}
    ,
    isSystemSymbol[bareSymbol],
      {"function", {"readonly", "defaultLibrary"}}
    ,
    isSystemOption[bareSymbol],
      {"property", {"readonly"}}
    ,
    (* Check paclet symbols using bare name *)
    isPacletSymbol[bareSymbol],
      {"class", {"definition"}}
    ,
    (* Check if symbol is from an external dependency package *)
    isExternalDependencySymbol[name],
      {"class", {"definition", "defaultLibrary"}}
    ,
    (* Experimental symbols - valid System` symbols but not yet stable *)
    isExperimentalSymbol[bareSymbol],
      {"deprecated", {"defaultLibrary"}}
    ,
    (* Obsolete/deprecated symbols *)
    isObsoleteSymbol[bareSymbol],
      {"deprecated", {"readonly"}}
    ,
    (* Session symbols (Echo, Print, In, Out, etc.) - real system functions *)
    isSessionSymbol[bareSymbol],
      {"function", {"readonly", "defaultLibrary"}}
    ,
    (* Undocumented symbols - exist in System` but have no public docs *)
    isUndocumentedSymbol[bareSymbol],
      {"type", {"defaultLibrary"}}
    ,
    (* Bad/commonly confused symbol names - likely a mistake *)
    isBadSymbol[bareSymbol],
      {"comment", {"error"}}
    ,
    (* Check if it's a context (ends with `) *)
    StringEndsQ[name, "`"],
      {"type", {}}
    ,
    (* Check if it starts with $ - likely a global variable *)
    StringStartsQ[name, "$"] || StringStartsQ[bareSymbol, "$"],
      {"variable", {}}
    ,
    True,
      (* Undefined symbol - mark as comment with error modifier *)
      {"comment", {"error"}}
  ]
]


semanticTokensURIMayOpenQ[uri_String] :=
  AnyTrue[
    Join[
      Replace[$PreExpandContentQueue, Except[_List] -> {}],
      Replace[$ContentQueue, Except[_List] -> {}]
    ],
    AssociationQ[#] &&
    MemberQ[{"textDocument/didOpen", "textDocument/didOpenFencepost"}, Lookup[#, "method", None]] &&
    Lookup[Lookup[Lookup[#, "params", <||>], "textDocument", <||>], "uri", None] === uri &
  ]


semanticTokensEntryReadyQ[entry_Association] :=
Module[{cst, ast},
  cst = Lookup[entry, "CST", Null];
  ast = Lookup[entry, "AST", Null];

  cst =!= Null && !FailureQ[cst] && ast =!= Null && !FailureQ[ast]
]


semanticTokensEntryWaitingForReindexQ[entry_] :=
  AssociationQ[entry] &&
  !semanticTokensEntryReadyQ[entry] &&
  Lookup[entry, "ScheduledJobs", {}] =!= {}


expandContent[content:KeyValuePattern["method" -> "textDocument/semanticTokens/full"], pos_] :=
Catch[
Module[{params, id, doc, uri, entry, res, supersededIDs, supersededFenceposts},

  log[1, "textDocument/semanticTokens/full: enter"];

  id = content["id"];
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  entry = Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]];

  If[!AssociationQ[entry] && !semanticTokensURIMayOpenQ[uri],
    log[0, "DBG-ST expand: CLOSED id=", id, " uri=", uri];
    Throw[{<| "method" -> "textDocument/semanticTokens/fullFencepost", "id" -> id, "params" -> params, "closed" -> True |>}]
  ];

  supersededIDs = LSPServer`Private`rememberPendingSemanticTokenRequest[uri, id];
  LSPServer`Private`dropQueuedSemanticTokenFenceposts[uri, supersededIDs];
  supersededFenceposts = LSPServer`Private`supersededSemanticTokenFencepostContents[uri, supersededIDs];

  If[!AssociationQ[entry],
    log[0, "DBG-ST expand: WAITING FOR DIDOPEN id=", id, " uri=", uri];
    log[1, "textDocument/semanticTokens/full: exit"];
    Throw[supersededFenceposts]
  ];

  If[semanticTokensEntryWaitingForReindexQ[entry],
    log[0, "DBG-ST expand: WAITING FOR REINDEX id=", id, " uri=", uri];
    log[1, "textDocument/semanticTokens/full: exit"];
    Throw[supersededFenceposts]
  ];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    If[$Debug2,
      log["canceled"]
    ];

    log[0, "DBG-ST expand: CANCELED id=", id];
    Throw[Join[supersededFenceposts, {<| "method" -> "textDocument/semanticTokens/fullFencepost", "id" -> id, "params" -> params, "stale" -> True |>}] ]
  ];

  log[0, "DBG-ST expand: id=", id, " uri=", uri];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],

    If[$Debug2,
      log["stale"]
    ];

    log[0, "DBG-ST expand: STALE id=", id, " uri=", uri];
    Throw[Join[supersededFenceposts, {<| "method" -> "textDocument/semanticTokens/fullFencepost", "id" -> id, "params" -> params, "stale" -> True |>}] ]
  ];

  res = Join[supersededFenceposts, {<| "method" -> "textDocument/semanticTokens/fullFencepost", "id" -> id, "params" -> params |> }];

  log[0, "DBG-ST expand: QUEUED FENCEPOST id=", id, " uri=", uri];
  log[1, "textDocument/semanticTokens/full: exit"];

  res
]]

(*
Compute the 1-based source {line, col} of the character at 1-based index `offset`
within rawStr (which includes surrounding quote characters), given the source
position of rawStr[[1]] (the opening quote).
Handles multi-line strings by counting literal newline characters.
*)
stringOffsetToSourcePos[rawStr_String, {startLine_Integer, startCol_Integer}, offset_Integer] :=
Module[{prefix, numNewlines, lastSegment},
  prefix = StringTake[rawStr, offset - 1];
  numNewlines = StringCount[prefix, "\n"];
  If[numNewlines == 0,
    {startLine, startCol + offset - 1}
    ,
    (* All preserves trailing empty string when prefix ends with \n *)
    lastSegment = Last[StringSplit[prefix, "\n", All]];
    {startLine + numNewlines, StringLength[lastSegment] + 1}
  ]
]

(*
Extract semantic tokens for WL expressions embedded via <* expr *> syntax.
rawStr is the CST representation including surrounding double-quote characters.
srcStart is the 1-based source position of rawStr[[1]] (the opening quote).
Supports both single-line and multi-line strings and expressions.
*)
stringArgEmbeddedTokens[
  LeafNode[String, rawStr_String, KeyValuePattern[Source -> {srcStart:{_Integer, _Integer}, _}]],
  scopedSources_
] :=
Catch[
Module[{blocks, allTokens},

  blocks = StringPosition[rawStr, "<*" ~~ Shortest[___] ~~ "*>"];

  If[blocks === {}, Throw[{}]];

  allTokens = Join @@ Map[
    Function[{block},
      Module[{exprStart, exprEnd, exprStr, exprCst, nodes, exprSourceStart},
        exprStart = block[[1]] + 2;  (* first char after "<*" *)
        exprEnd   = block[[2]] - 2;  (* last char before "*>" *)

        If[exprEnd < exprStart, Return[{}]];

        exprStr = StringTake[rawStr, {exprStart, exprEnd}];

        exprCst = Quiet[CodeConcreteParse[exprStr]];
        If[FailureQ[exprCst], Return[{}]];

        (* 1-based source position of exprStr[[1]] *)
        exprSourceStart = stringOffsetToSourcePos[rawStr, srcStart, exprStart];

        (* {"symbol", name, src} for symbols, {"string", Null, src} for string literals *)
        nodes = Join[
          Cases[exprCst,
            LeafNode[Symbol, name_String, KeyValuePattern[Source -> s_]] :> {"symbol", name, s},
            Infinity
          ],
          Cases[exprCst,
            LeafNode[String, _, KeyValuePattern[Source -> s_]] :> {"string", Null, s},
            Infinity
          ]
        ];

        Map[
          Function[{node},
            Module[{kind, name, nodeSrc, srcLineActual, srcColActual, tokenType, modifiers},
              {kind, name, nodeSrc} = node;

              (* Map parsed {line, col} to source {line, col} (both 1-based) *)
              srcLineActual = exprSourceStart[[1]] + nodeSrc[[1, 1]] - 1;
              srcColActual  = If[nodeSrc[[1, 1]] == 1,
                exprSourceStart[[2]] + nodeSrc[[1, 2]] - 1,
                nodeSrc[[1, 2]]  (* column resets on new lines *)
              ];

              If[KeyExistsQ[scopedSources, {srcLineActual, srcColActual}],
                Return[Nothing]
              ];

              If[kind === "symbol",
                {tokenType, modifiers} = classifyGlobalSymbol[name]
                ,
                tokenType = "string"; modifiers = {}
              ];

              {
                srcLineActual - 1,   (* 0-based line *)
                srcColActual - 1,    (* 0-based column *)
                nodeSrc[[2, 2]] - nodeSrc[[1, 2]],
                $SemanticTokenTypes[tokenType],
                If[modifiers === {},
                  0,
                  BitOr @@ BitShiftLeft[1, Lookup[$SemanticTokenModifiers, modifiers, 0]]
                ]
              }
            ]
          ],
          nodes
        ]
      ]
    ],
    blocks
  ];

  DeleteCases[allTokens, Nothing]
]]

stringArgEmbeddedTokens[_, _] := {}

(*
Find all string nodes in the CST that contain <* ... *> template syntax and
extract semantic tokens for the embedded WL expressions.
Scanning the CST directly avoids relying on the CST CallNode structure
(which differs from the AST: head is {LeafNode[...]}, body is GroupNode).
*)
stringTemplateEmbeddedTokens[cst_, scopedSources_] :=
Module[{templateStrings},
  templateStrings = Cases[cst,
    node:LeafNode[String, rawStr_String, _] /; StringContainsQ[rawStr, "<*"] :> node,
    Infinity
  ];
  Join @@ Map[stringArgEmbeddedTokens[#, scopedSources]&, templateStrings]
]


(*
Compute and cache semantic tokens for uri synchronously.
Reads entry from $OpenFilesMap; requires entry["CST"] and entry["AST"] to be present.
If ScopingData is not yet cached it is computed now.
Returns True if tokens were computed, False if the entry or parse data was missing.
Used by workspace/semanticTokens/refresh to pre-compute tokens for files whose
initial semanticTokens/full pipeline was cancelled before it completed.
*)
computeAndCacheSemanticTokens[uri_String] :=
Module[{entry, cst, ast, scopingData, localTokens,
  scopedSources, allSymbols, globalSymbolTokens, stringTemplateTokens,
  transformed, line, char, oldLine, oldChar, semanticTokens},

  entry = Lookup[$OpenFilesMap, uri, Null];
  If[!AssociationQ[entry], Return[False]];

  cst = Lookup[entry, "CST", Null];
  ast = Lookup[entry, "AST", Null];

  If[cst === Null || FailureQ[cst] || ast === Null || FailureQ[ast],
    Return[False]
  ];

  (* Compute ScopingData if not yet cached *)
  scopingData = Lookup[entry, "ScopingData", Null];
  If[scopingData === Null || FailureQ[scopingData],
    scopingData = Join[ScopingData[ast], extractMathScopingData[ast]];
    entry["ScopingData"] = scopingData;
  ];

  localTokens =
    Function[{source, scope, modifiers},
      {#[[1, 1]], #[[1, 2]], #[[2, 2]] - #[[1, 2]],
        $SemanticTokenTypes[
          Switch[scope,
            {___, "Module" | "Block" | "DynamicModule" | "Internal`InheritedBlock"},
              "variable",
            {___, "With"},
              "constant",
            {___, "Defined"},
              "function",
            _,
              "parameter"
          ]
        ],
        BitOr @@ BitShiftLeft[1, Lookup[$SemanticTokenModifiers, modifiers ~Join~ (
            Replace[scope,
              {"Module" | "DynamicModule" -> "Module",
               "Block" | "Internal`InheritedBlock" -> "Block",
               "With" -> "With",
               _ :> Sequence @@ {}},
              {1}]
          ), 0]]}&[source - 1]
    ] @@@ scopingData;

  scopedSources = Association[
    Table[{data[[1, 1, 1]], data[[1, 1, 2]]} -> True, {data, scopingData}]
  ];

  allSymbols = Cases[cst,
    LeafNode[Symbol, name_String, KeyValuePattern[Source -> src_]] :> {name, src},
    Infinity
  ];

  globalSymbolTokens = DeleteCases[
    Table[
      Module[{name, src, tokenType, modifiers, classification, startPos},
        {name, src} = sym;
        startPos = {src[[1, 1]], src[[1, 2]]};
        If[KeyExistsQ[scopedSources, startPos], Nothing,
          classification = classifyGlobalSymbol[name];
          tokenType = classification[[1]];
          modifiers = classification[[2]];
          {src[[1, 1]] - 1, src[[1, 2]] - 1, src[[2, 2]] - src[[1, 2]],
           $SemanticTokenTypes[tokenType],
           If[modifiers === {}, 0,
              BitOr @@ BitShiftLeft[1, Lookup[$SemanticTokenModifiers, modifiers, 0]]]}
        ]
      ],
      {sym, allSymbols}
    ],
    Nothing
  ];

  stringTemplateTokens = stringTemplateEmbeddedTokens[cst, scopedSources];

  transformed = Sort[Join[localTokens, globalSymbolTokens, stringTemplateTokens]];
  line = 0; char = 0;
  transformed = Function[{t},
    oldLine = line; oldChar = char;
    line = t[[1]]; char = t[[2]];
    {line - oldLine, If[oldLine == line, char - oldChar, char], t[[3]], t[[4]], t[[5]]}
  ] /@ transformed;
  transformed = Flatten[transformed];

  semanticTokens = transformed;
  entry["SemanticTokens"] = semanticTokens;
  $OpenFilesMap[uri] = entry;

  log[0, "DBG-ST computeAndCacheSemanticTokens: COMPUTED tokens=", Length[semanticTokens], " uri=", uri];
  True
]


handleContent[content:KeyValuePattern["method" -> "textDocument/semanticTokens/fullFencepost"]] :=
Catch[
Module[{id, params, doc, uri, entry, semanticTokens, scopingData, cst, allSymbols,
  scopedSources, globalSymbolTokens, stringTemplateTokens, localTokens, transformed,
  line, char, oldLine, oldChar},

  log[1, "textDocument/semanticTokens/fullFencepost: enter"];

  id = content["id"];

  (*
  Even if VS Code cancelled the request (startup burst), we compute and return real tokens.
  VS Code ignores the cancellation and applies the response if it arrives before it moves on.
  Just clear the stale cancel entry so handleContent["$/cancelRequest"] doesn't warn.
  *)
  If[Lookup[$CancelMap, id, False],
    $CancelMap[id] =.;
    If[$Debug2,
      log["cancel cleared; computing tokens anyway"]
    ]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  clearPending[] := LSPServer`Private`forgetPendingSemanticTokenRequest[uri, id];

  log[0, "DBG-ST fencepost: enter id=", id, " stale=", Lookup[content, "stale", False], " uri=", uri];

  If[TrueQ[Lookup[content, "superseded", False]],
    log[0, "DBG-ST fencepost: SUPERSEDED id=", id, " uri=", uri];
    clearPending[];
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  If[TrueQ[Lookup[content, "closed", False]],
    log[0, "DBG-ST fencepost: CLOSED id=", id, " uri=", uri];
    clearPending[];
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  (*
  Only abort on real document-change staleness (a didChange/didClose for the same URI is
  already queued). The content["stale"] flag comes from expand-time cancel detection and
  skips runScopingData, but CST/AST are still available from didOpenFencepost, so we can
  still compute at least global-symbol tokens.
  *)
  If[isStale[$ContentQueue, uri],

    If[$Debug2,
      log["stale"]
    ];

    log[0, "DBG-ST fencepost: DIDCHANGE-STALE id=", id, " uri=", uri];
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];

  If[entry === Null,
    If[!semanticTokensURIMayOpenQ[uri],
      log[0, "DBG-ST fencepost: NOT OPEN, returning null id=", id, " uri=", uri];
      clearPending[];
      Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
    ];

    log[0, "DBG-ST fencepost: WAITING FOR DIDOPEN id=", id, " uri=", uri];
    Throw[{}]
  ];

  semanticTokens = Lookup[entry, "SemanticTokens", Null];

  If[semanticTokens =!= Null,
    clearPending[];
    log[0, "DBG-ST fencepost: CACHE HIT id=", id, " tokens=", Length[semanticTokens], " uri=", uri];
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> <| "data" -> semanticTokens |> |>}]
  ];

  If[semanticTokensEntryWaitingForReindexQ[entry],
    log[0, "DBG-ST fencepost: WAITING FOR REINDEX id=", id, " uri=", uri];
    Throw[{}]
  ];

  If[LSPServer`SemanticTokens`computeAndCacheSemanticTokens[uri],
    entry = Lookup[$OpenFilesMap, uri, Null];
    semanticTokens = Lookup[entry, "SemanticTokens", Null];
    If[semanticTokens =!= Null,
      clearPending[];
      log[0, "DBG-ST fencepost: COMPUTED FROM ENTRY id=", id, " tokens=", Length[semanticTokens], " uri=", uri];
      Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> <| "data" -> semanticTokens |> |>}]
    ]
  ];

  scopingData = Lookup[entry, "ScopingData", Null];
  cst = Lookup[entry, "CST", Null];

  (*
  If no scoping data, still try to provide global symbol highlighting
  *)
  If[scopingData === Null || FailureQ[scopingData],
    scopingData = {}
  ];

  (*
  Transform local scoping data (from CodeParser`Scoping`)
  Related links: https://microsoft.github.io/language-server-protocol/specification#textDocument_semanticTokens
  *)
  localTokens =
    Function[{source, scope, modifiers},
      {#[[1, 1]], #[[1, 2]], #[[2, 2]] - #[[1, 2]],

        $SemanticTokenTypes[
          Switch[scope,
            {___, "Module" | "Block" | "DynamicModule" | "Internal`InheritedBlock"},
              "variable"
            ,
            {___, "With"},
              "constant"
            ,
            {___, "Defined"},
              "function"
            ,
            _,
              "parameter"
          ]
        ],

        BitOr @@ BitShiftLeft[1, Lookup[$SemanticTokenModifiers, modifiers ~Join~ (
            Replace[scope,
              {
                "Module" | "DynamicModule" -> "Module",
                "Block" | "Internal`InheritedBlock" -> "Block",
                "With" -> "With",
                _ :> Sequence @@ {}
              }
              ,
              {1}
            ]
          ), 0]]}&[source - 1]
    ] @@@ scopingData;

  (*
  Now add global symbol highlighting (paclet symbols, system symbols, undefined)
  Only if we have CST available
  *)
  globalSymbolTokens = {};
  stringTemplateTokens = {};

  If[cst =!= Null && !FailureQ[cst],
    (*
    Get all symbol sources that are already covered by local scoping
    Use start position (line, col) as key since that uniquely identifies a token position
    *)
    scopedSources = Association[
      Table[
        {data[[1, 1, 1]], data[[1, 1, 2]]} -> True,
        {data, scopingData}
      ]
    ];

    (*
    Find all symbols in the CST that aren't in local scope
    *)
    allSymbols = Cases[cst,
      LeafNode[Symbol, name_String, KeyValuePattern[Source -> src_]] :> {name, src},
      Infinity
    ];

    (*
    Filter to symbols not in local scoping data and classify them
    *)
    globalSymbolTokens = Table[
      Module[{name, src, tokenType, modifiers, classification, startPos},
        {name, src} = sym;
        startPos = {src[[1, 1]], src[[1, 2]]};

        (*
        Skip if this source position is already covered by local scoping
        *)
        If[KeyExistsQ[scopedSources, startPos],
          Nothing
          ,
          (*
          Classify the symbol
          *)
          classification = classifyGlobalSymbol[name];
          tokenType = classification[[1]];
          modifiers = classification[[2]];

          (*
          Create token: {line, char, length, tokenType, modifierBits}
          Convert to 0-based
          *)
          {
            src[[1, 1]] - 1,
            src[[1, 2]] - 1,
            src[[2, 2]] - src[[1, 2]],
            $SemanticTokenTypes[tokenType],
            If[modifiers === {},
              0,
              BitOr @@ BitShiftLeft[1, Lookup[$SemanticTokenModifiers, modifiers, 0]]
            ]
          }
        ]
      ],
      {sym, allSymbols}
    ];

    (*
    Remove Nothing entries
    *)
    globalSymbolTokens = DeleteCases[globalSymbolTokens, Nothing];

    stringTemplateTokens = stringTemplateEmbeddedTokens[cst, scopedSources];
  ];

  (*
  Combine local, global, and StringTemplate embedded tokens
  *)
  transformed = Join[localTokens, globalSymbolTokens, stringTemplateTokens];

  (*
  Relativize the tokens (LSP requires delta encoding)
  *)
  transformed = Sort[transformed];

  line = 0;
  char = 0;

  transformed = Function[{t},
    oldLine = line;
    oldChar = char;
    line = t[[1]];
    char = t[[2]];
    {line - oldLine, If[oldLine == line, char - oldChar, char], t[[3]], t[[4]], t[[5]]}
  ] /@ transformed;

  transformed = Flatten[transformed];

  semanticTokens = transformed;

  entry["SemanticTokens"] = semanticTokens;

  $OpenFilesMap[uri] = entry;

  clearPending[];
  log[0, "DBG-ST fencepost: COMPUTED id=", id, " tokens=", Length[semanticTokens], " uri=", uri];
  log[1, "textDocument/semanticTokens/fullFencepost: exit"];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> <| "data" -> semanticTokens |> |>}
]]


(*
Extract scoping data for math functions whose variables are not handled
by CodeParser`Scoping`ScopingData (e.g. Integrate, D, Solve, Limit, ...).

Returns {source, scope, modifiers} triples in the same format as ScopingData[],
using scope {"MathIterator"} which falls through to the "parameter" token type.
*)
extractMathScopingData[ast_] :=
Module[{bag},
  bag = Internal`Bag[];

  (* Mark ALL occurrences of varName within callNode as MathIterator scoped.
     This mirrors how ScopingData handles Module[{x}, f[x]] -- both the
     declaration site and every usage within the scope get the same token type. *)
  markVar[varName_String, callNode_] :=
    Scan[
      Internal`StuffBag[bag, {#[[3, Key[Source]]], {"MathIterator"}, {}}]&,
      Cases[callNode, LeafNode[Symbol, varName, _], Infinity]
    ];

  (* D derivative spec: x or {x, n} -- return list of var names *)
  dSpecVarNames[LeafNode[Symbol, vn_, _]] := {vn};
  dSpecVarNames[CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, vn_, _], _}, _]] := {vn};
  dSpecVarNames[_] := {};

  (* Integrate[f, {x, a, b}], NIntegrate, Series, Residue *)
  Cases[ast,
    call:CallNode[LeafNode[Symbol, "Integrate" | "NIntegrate" | "Series" | "Residue", _],
      {_, CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, vn_, _], ___}, _], ___}, _] :>
      markVar[vn, call],
    Infinity];

  (* D[f, spec1, spec2, ...] -- all specs contribute variables *)
  Cases[ast,
    call:CallNode[LeafNode[Symbol, "D", _], {_, specs__}, _] :>
      Scan[Scan[markVar[#, call]&, dSpecVarNames[#]]&, {specs}],
    Infinity];

  (* Solve[eqns, x], NSolve, Reduce, FindInstance -- bare symbol *)
  Cases[ast,
    call:CallNode[LeafNode[Symbol, "Solve" | "NSolve" | "Reduce" | "FindInstance", _],
      {_, LeafNode[Symbol, vn_, _], ___}, _] :>
      markVar[vn, call],
    Infinity];

  (* Solve[eqns, {x, y, ...}] -- list of vars *)
  Cases[ast,
    call:CallNode[LeafNode[Symbol, "Solve" | "NSolve" | "Reduce" | "FindInstance", _],
      {_, CallNode[LeafNode[Symbol, "List", _], vars_, _], ___}, _] :>
      Scan[If[MatchQ[#, LeafNode[Symbol, _, _]], markVar[#[[2]], call]]&, vars],
    Infinity];

  (* FindRoot[f, {x, x0}] or FindRoot[f, {x, x0, x1}] *)
  Cases[ast,
    call:CallNode[LeafNode[Symbol, "FindRoot", _],
      {_, CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, vn_, _], __}, _], ___}, _] :>
      markVar[vn, call],
    Infinity];

  (* DSolve/NDSolve[eqns, y, x] -- 3rd arg is independent variable *)
  Cases[ast,
    call:CallNode[LeafNode[Symbol, "DSolve" | "NDSolve", _],
      {_, _, LeafNode[Symbol, vn_, _], ___}, _] :>
      markVar[vn, call],
    Infinity];

  (* NDSolve[eqns, y, {x, x0, x1}] -- iterator form *)
  Cases[ast,
    call:CallNode[LeafNode[Symbol, "NDSolve", _],
      {_, _, CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, vn_, _], __}, _], ___}, _] :>
      markVar[vn, call],
    Infinity];

  (* Limit[f, x -> x0] *)
  Cases[ast,
    call:CallNode[LeafNode[Symbol, "Limit", _],
      {_, CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], {LeafNode[Symbol, vn_, _], _}, _], ___}, _] :>
      markVar[vn, call],
    Infinity];

  (* Deduplicate by start position in case of nested calls or overlapping matches *)
  DeleteDuplicatesBy[Internal`BagPart[bag, All], #[[1, 1]]&]
]


handleContent[content:KeyValuePattern["method" -> "textDocument/runScopingData"]] :=
Catch[
Module[{params, doc, uri, entry, ast, scopingData},

  log[1, "textDocument/runScopingData: enter"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  log[0, "DBG-ST runScopingData: uri=", uri];

  If[isStale[$ContentQueue, uri],

    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];

  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  scopingData = Lookup[entry, "ScopingData", Null];

  If[scopingData =!= Null,
    log[0, "DBG-ST runScopingData: ALREADY CACHED uri=", uri];
    Throw[{}]
  ];

  ast = entry["AST"];

  If[FailureQ[ast],
    Throw[{}]
  ];

  If[$Debug2,
    log["before ScopingData"]
  ];

  scopingData = ScopingData[ast];

  (* Merge in math function variable scoping (Integrate, D, Solve, Limit, etc.) *)
  scopingData = Join[scopingData, extractMathScopingData[ast]];

  log[2, "after ScopingData"];

  entry["ScopingData"] = scopingData;

  $OpenFilesMap[uri] = entry;

  log[0, "DBG-ST runScopingData: COMPUTED entries=", Length[scopingData], " uri=", uri];
  log[1, "textDocument/runScopingData: exit"];

  {}
]]


End[]

EndPackage[]
