BeginPackage["LSPServer`Diagnostics`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`PacletIndex`"]
Needs["LSPServer`Utils`"]
Needs["CodeInspector`"]
Needs["CodeInspector`SuppressedRegions`"] (* for SuppressedRegions *)
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Scoping`"] (* for scopingDataObject *)


expandContent[content:KeyValuePattern["method" -> "textDocument/runDiagnostics"], pos_] :=
Catch[
Module[{params, doc, uri},

  If[$Debug2,
    log["textDocument/runDiagnostics: enter expand"]
  ];
  
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];

  <| "method" -> #, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/suppressedRegions",
    "textDocument/runConcreteDiagnostics",
    "textDocument/aggregateParse",
    "textDocument/runAggregateDiagnostics",
    "textDocument/abstractParse",
    "textDocument/runAbstractDiagnostics",
    "textDocument/runScopingData", (* implemented in SemanticTokens.wl *)
    "textDocument/runScopingDiagnostics",
    "textDocument/runWorkspaceDiagnostics"
  }
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/suppressedRegions"]] :=
Catch[
Module[{params, doc, uri, entry, cst, suppressedRegions},

  If[$Debug2,
    log["textDocument/suppressedRegions: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

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

  suppressedRegions = Lookup[entry, "SuppressedRegions", Null];

  If[suppressedRegions =!= Null,
    Throw[{}]
  ];
  
  cst = entry["CST"];

  If[$Debug2,
    log["before SuppressedRegions"];
  ];

  suppressedRegions = SuppressedRegions[cst];

  If[$Debug2,
    log["after SuppressedRegions"]
  ];

  If[$Debug2,
    log["suppressedRegions: ", suppressedRegions]
  ];
  
  entry["SuppressedRegions"] = suppressedRegions;

  $OpenFilesMap[uri] = entry;

  {}
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runConcreteDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, cst, cstLints, suppressedRegions},

  If[$Debug2,
    log["textDocument/runConcreteDiagnostics: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

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

  cstLints = Lookup[entry, "CSTLints", Null];

  If[cstLints =!= Null,
    Throw[{}]
  ];
  
  cst = entry["CST"];

  suppressedRegions = entry["SuppressedRegions"];

  If[$Debug2,
    log["before CodeInspectCST"]
  ];

  cstLints = CodeInspectCST[cst, "AggregateRules" -> <||>, "AbstractRules" -> <||>, "SuppressedRegions" -> suppressedRegions];

  If[$Debug2,
    log["after CodeInspectCST"]
  ];

  If[!MatchQ[cstLints, _List],
    log["cstLints: NOT A LIST!!!"];
    Throw[{}]
  ];

  If[$Debug2,
    log["cstLints: ", #["Tag"]& /@ cstLints]
  ];

  entry["CSTLints"] = cstLints;

  $OpenFilesMap[uri] = entry;

  {}
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runAggregateDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, agg, aggLints, suppressedRegions},

  If[$Debug2,
    log["textDocument/runAggregateDiagnostics: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

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

  aggLints = Lookup[entry, "AggLints", Null];

  If[aggLints =!= Null,
    Throw[{}]
  ];

  agg = entry["Agg"];

  suppressedRegions = entry["SuppressedRegions"];

  If[$Debug2,
    log["before CodeInspectAgg"]
  ];

  aggLints = CodeInspectAgg[agg, "AbstractRules" -> <||>, "SuppressedRegions" -> suppressedRegions];

  If[$Debug2,
    log["after CodeInspectAgg"]
  ];

  If[!MatchQ[aggLints, _List],
    log["aggLints: NOT A LIST!!!"];
    Throw[{}]
  ];

  If[$Debug2,
    log["aggLints: ", #["Tag"]& /@ aggLints]
  ];

  entry["AggLints"] = aggLints;

  $OpenFilesMap[uri] = entry;

  {}
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runAbstractDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, ast, astLints, suppressedRegions},

  If[$Debug2,
    log["textDocument/runAbstractDiagnostics: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

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

  astLints = Lookup[entry, "ASTLints", Null];

  If[astLints =!= Null,
    Throw[{}]
  ];

  ast = entry["AST"];

  If[FailureQ[ast],
    Throw[{}]
  ];

  suppressedRegions = entry["SuppressedRegions"];

  If[$Debug2,
    log["before CodeInspectAST"]
  ];

  astLints = CodeInspectAST[ast, "SuppressedRegions" -> suppressedRegions];

  If[$Debug2,
    log["after CodeInspectAST"]
  ];

  If[!MatchQ[astLints, _List],
    log["astLints: NOT A LIST!!!"];
    Throw[{}]
  ];

  If[$Debug2,
    log["astLints: ", #["Tag"]& /@ astLints]
  ];

  entry["ASTLints"] = astLints;

  $OpenFilesMap[uri] = entry;

  {}
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runScopingDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, scopingLints, scopingData, filtered, suppressedRegions, isActive},

  If[$Debug2,
    log["textDocument/runScopingDiagnostics: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

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

  scopingLints = Lookup[entry, "ScopingLints", Null];

  If[scopingLints =!= Null,
    Throw[{}]
  ];

  scopingData = entry["ScopingData"];

  If[$Debug2,
    log["scopingData (up to 20): ", Take[scopingData, UpTo[20]]]
  ];

  (*
  Filter those that have non-empty modifiers
  *)
  filtered = Cases[scopingData, scopingDataObject[_, _, {_, ___}, _]];

  scopingLints = scopingDataObjectToLints /@ filtered;

  scopingLints = Flatten[scopingLints];

  (*
  Filter out suppressed
  *)
  suppressedRegions = entry["SuppressedRegions"];

  isActive = makeIsActiveFunc[suppressedRegions];

  scopingLints = Select[scopingLints, isActive];
  
  (*
  If $SemanticTokens, then only keep:
  errors

  These will be semantic highlighted AND shown in diagnostics
  Everything else will just be semantic highlighted


  If NOT $SemanticTokens, then only keep:
  errors
  unused variables

  Everything else, such as shadowed and unused parameters is a bit too noisy
  *)
  If[$SemanticTokens,
    scopingLints =
      Cases[scopingLints, InspectionObject[_, _, "Warning" | "Error" | "Fatal", _]]
    ,
    scopingLints =
      Cases[scopingLints,
        InspectionObject[_, _, "Warning" | "Error" | "Fatal", _] |
          InspectionObject["UnusedVariable", _, "Scoping", _]]
  ];

  If[$Debug2,
    log["scopingLints: ", #["Tag"]& /@ scopingLints]
  ];

  entry["ScopingLints"] = scopingLints;

  $OpenFilesMap[uri] = entry;

  {}
]]


(*
Run workspace-wide diagnostics
This checks for:
1. Undefined symbols that might be defined elsewhere in the paclet
2. Context loading errors (using symbols from unloaded contexts)
*)
handleContent[content:KeyValuePattern["method" -> "textDocument/runWorkspaceDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, cst, workspaceLints, symbolRefs, undefined,
  suppressedRegions, isActive, pacletSymbols, systemSymbols, localSymbols,
  scopingData, locallyDefinedSymbols, contextErrors, contextLints},

  If[$Debug2,
    log["textDocument/runWorkspaceDiagnostics: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

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

  workspaceLints = Lookup[entry, "WorkspaceLints", Null];

  If[workspaceLints =!= Null,
    Throw[{}]
  ];

  (*
  Skip workspace diagnostics if paclet index is not initialized
  *)
  If[$WorkspaceRootPath === None,
    entry["WorkspaceLints"] = {};
    $OpenFilesMap[uri] = entry;
    Throw[{}]
  ];

  cst = entry["CST"];

  If[FailureQ[cst],
    Throw[{}]
  ];

  (*
  Get all symbol references in the file
  *)
  symbolRefs = Cases[cst, 
    LeafNode[Symbol, name_String, KeyValuePattern[Source -> src_]] :> <|"name" -> name, "source" -> src|>,
    Infinity
  ];

  (*
  Get system symbols
  *)
  systemSymbols = Join[
    WolframLanguageSyntax`Generate`$builtinFunctions,
    WolframLanguageSyntax`Generate`$constants
  ];

  (*
  Get paclet symbols
  *)
  pacletSymbols = GetPacletSymbols[];

  (*
  Get locally defined symbols from scoping data
  *)
  scopingData = Lookup[entry, "ScopingData", {}];
  locallyDefinedSymbols = DeleteDuplicates[#[[4]]& /@ scopingData];

  (*
  Find undefined symbols - those not in system, paclet, or local scope
  *)
  undefined = Select[symbolRefs, 
    Function[{ref},
      Module[{name},
        name = ref["name"];
        (*
        Symbol is undefined if:
        - Not a system symbol
        - Not defined in the paclet
        - Not locally scoped in this file
        - Not a common pattern variable (single letters, or ending in _)
        - Not a context path (contains `)
        *)
        And[
          !MemberQ[systemSymbols, name],
          !MemberQ[pacletSymbols, name],
          !MemberQ[locallyDefinedSymbols, name],
          !StringMatchQ[name, LetterCharacter],  (* Single letter variables *)
          !StringContainsQ[name, "`"],  (* Context paths *)
          !StringMatchQ[name, "$" ~~ LetterCharacter],  (* $ prefixed single letters *)
          StringLength[name] > 1  (* Multi-character symbols *)
        ]
      ]
    ]
  ];

  (*
  Create lints for undefined symbols
  *)
  workspaceLints = Table[
    InspectionObject[
      "UndefinedSymbol",
      "Symbol \"" <> ref["name"] <> "\" is not defined in the paclet or System context",
      "Remark",
      <|
        Source -> ref["source"],
        ConfidenceLevel -> 0.5,  (* Lower confidence since we might miss some definitions *)
        "Argument" -> ref["name"]
      |>
    ],
    {ref, Take[undefined, UpTo[50]]}  (* Limit to avoid overwhelming with warnings *)
  ];

  (*
  Check for context loading errors (using symbols from unloaded contexts)
  *)
  contextErrors = GetContextLoadErrors[uri];
  
  contextLints = Table[
    InspectionObject[
      "UnloadedContext",
      "Context \"" <> err["context"] <> "\" is used but not loaded. " <>
        "Add Needs[\"" <> err["context"] <> "\"] before using " <> err["fullName"] <> ".",
      "Warning",
      <|
        Source -> err["source"],
        ConfidenceLevel -> 0.85,
        "Argument" -> err["context"]
      |>
    ],
    {err, Take[contextErrors, UpTo[20]]}
  ];
  
  workspaceLints = Join[workspaceLints, contextLints];

  (*
  Filter out suppressed lints
  *)
  suppressedRegions = Lookup[entry, "SuppressedRegions", {}];
  isActive = makeIsActiveFunc[suppressedRegions];
  workspaceLints = Select[workspaceLints, isActive];

  If[$Debug2,
    log["workspaceLints: ", Length[workspaceLints], " issues (", 
        Length[undefined], " undefined symbols, ", 
        Length[contextErrors], " context errors)"]
  ];

  entry["WorkspaceLints"] = workspaceLints;

  $OpenFilesMap[uri] = entry;

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/clearDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry},

  If[$Debug2,
    log["textDocument/clearDiagnostics: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

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
  
  entry["CSTLints"] =.;

  entry["AggLints"] =.;
  
  entry["ASTLints"] =.;

  entry["ScopingLints"] =.;

  entry["WorkspaceLints"] =.;

  $OpenFilesMap[uri] = entry;

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/publishDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, lints, lintsWithConfidence, cstLints, aggLints, astLints, scopingLints, workspaceLints, diagnostics},

  If[$Debug2,
    log["textDocument/publishDiagnostics: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];
  
  entry = Lookup[$OpenFilesMap, uri, Null];

  (*
  Possibly cleared
  *)
  If[entry === Null,
    Throw[{<| "jsonrpc" -> "2.0",
              "method" -> "textDocument/publishDiagnostics",
              "params" -> <| "uri" -> uri,
                              "diagnostics" -> {} |> |>}]
  ];

  (*
  Possibly cleared
  *)
  cstLints = Lookup[entry, "CSTLints", {}];

  (*
  Possibly cleared
  *)
  aggLints = Lookup[entry, "AggLints", {}];

  (*
  Possibly cleared
  *)
  astLints = Lookup[entry, "ASTLints", {}];

  (*
  Possibly cleared
  *)
  scopingLints = Lookup[entry, "ScopingLints", {}];

  (*
  Workspace-wide lints (undefined symbols, etc.)
  *)
  workspaceLints = Lookup[entry, "WorkspaceLints", {}];

  lints = cstLints ~Join~ aggLints ~Join~ astLints ~Join~ scopingLints ~Join~ workspaceLints;

  If[$Debug2,
    log["lints: ", #["Tag"]& /@ lints]
  ];


  lintsWithConfidence = Cases[lints, InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> _]]];

  lints = Cases[lintsWithConfidence, InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> _?(GreaterEqualThan[$ConfidenceLevel])]]];

  (*

  Disable shadow filtering for now

  Below is quadratic time

  shadowing = Select[lints, Function[lint, AnyTrue[lints, shadows[lint, #]&]]];

  lints = Complement[lints, shadowing];
  *)

  
  (*
  Make sure to sort lints before taking

  Sort by severity, then sort by Source

  severityToInteger maps "Remark" -> 1 and "Fatal" -> 4, so make sure to negate that
  *)
  lints = SortBy[lints, {-severityToInteger[#[[3]]]&, #[[4, Key[Source]]]&}];

  lints = Take[lints, UpTo[CodeInspector`Summarize`$DefaultLintLimit]];

  If[$Debug2,
    log["lints: ", #["Tag"]& /@ lints]
  ];

  diagnostics = lintToDiagnostics /@ lints;

  diagnostics = Flatten[diagnostics];

  {<| "jsonrpc" -> "2.0",
      "method" -> "textDocument/publishDiagnostics",
      "params" -> <| "uri" -> uri,
                      "diagnostics" -> diagnostics |> |>}
]]




(*
returns a function lint -> True|False
*)
makeIsActiveFunc[suppressedRegions_] :=
  Function[{lint},
    AllTrue[suppressedRegions,
      Function[{region},
        !SourceMemberQ[region[[1;;2]], lint] ||
          AllTrue[region[[3]],
            Function[{suppressed}, isTagActive[lint, suppressed]]
          ]
      ]
    ]
  ]


isTagActive[InspectionObject[tag1_, _, _, KeyValuePattern["Argument" -> arg1_]], {tag2_, arg2_}] :=
  !(tag1 === tag2 && arg1 === arg2)

(*
The lint has an Argument, but there is no argument in the suppressed
*)
isTagActive[InspectionObject[_, _, _, KeyValuePattern["Argument" -> _]], {_}] :=
  True

isTagActive[InspectionObject[tag1_, _, _, _], {tag2_, _}] :=
  !(tag1 === tag2)

isTagActive[InspectionObject[tag1_, _, _, _], {tag2_}] :=
  !(tag1 === tag2)


End[]

EndPackage[]
