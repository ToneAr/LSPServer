BeginPackage["LSPServer`Diagnostics`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`PacletIndex`"]
Needs["LSPServer`IgnorePatterns`"]
Needs["LSPServer`Utils`"]
Needs["CodeInspector`"]
Needs["CodeInspector`SuppressedRegions`"]
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Scoping`"]

(* Load pre-generated builtin pattern map into this package's private context.
   Note: PacletIndex.wl also loads this file into its own private context so that
   inferPatternFromRHS and resolveCallReturnPattern can access the data.  Each
   package needs its own copy because they live in separate private contexts. *)
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
  Convert a single input-spec string from $BuiltinPatterns into a WL pattern
  expression suitable for MatchQ against a representative sample value.

  Spec string vocabulary:
    "Type..."  - BlankSequence[Symbol["Type"]]    (1+ expressions with head Type)
    "Type*"    - BlankNullSequence[Symbol["Type"]] (0+ expressions with head Type)
    "..."      - BlankSequence[]                   (1+ untyped)
    "*"        - BlankNullSequence[]               (0+ untyped)
    "_?Pred"   - ToExpression["_?Pred"]            (PatternTest -- starts with "_")
    "Type"     - Blank[Symbol["Type"]]             (exactly one typed arg)
    None       - Blank[]                            (exactly one untyped arg)
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

  For fixed-arity overloads: exact length match + element-wise MatchQ.
  For variadic overloads (last spec ends with "..." or "*"):
    - at least Length[specs]-1 args required
    - fixed prefix args matched normally
    - each tail arg matched against the element type (Blank[T])
  Missing["Unknown"] args always match (unresolvable types pass through).
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


expandContent[content:KeyValuePattern["method" -> "textDocument/runDiagnostics"], pos_] :=
Catch[
Module[{params, doc, uri, res},

  log[1, "textDocument/runDiagnostics: enter expand"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],

    log[2, "stale"];

    Throw[{}]
  ];

  res = <| "method" -> #, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/suppressedRegions",
    "textDocument/parseIgnoreComments",
    "textDocument/runConcreteDiagnostics",
    "textDocument/aggregateParse",
    "textDocument/runAggregateDiagnostics",
    "textDocument/abstractParse",
    "textDocument/runAbstractDiagnostics",
    "textDocument/runScopingData",
    "textDocument/runScopingDiagnostics",
    "textDocument/runWorkspaceDiagnostics"
  };

  log[1, "textDocument/runDiagnostics: exit"];

  res
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/suppressedRegions"]] :=
Catch[
Module[{params, doc, uri, entry, cst, suppressedRegions},

  log[1, "textDocument/suppressedRegions: enter"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],

    log[2, "stale"];

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

  log[2, "before SuppressedRegions"];

  suppressedRegions = SuppressedRegions[cst];

  log["after SuppressedRegions"];

  entry["SuppressedRegions"] = suppressedRegions;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/suppressedRegions: exit"];

  {}
]]

(*
Parse wl-disable/wl-enable comments for custom ignore patterns
*)
handleContent[content:KeyValuePattern["method" -> "textDocument/parseIgnoreComments"]] :=
Catch[
Module[{params, doc, uri, entry, cst, ignoreData},

  If[$Debug2,
    log["textDocument/parseIgnoreComments: enter"]
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

  (* Check if already parsed *)
  If[Lookup[entry, "IgnoreData", Null] =!= Null,
    Throw[{}]
  ];

  cst = entry["CST"];

  If[$Debug2,
    log["before ParseIgnoreComments"]
  ];

  ignoreData = UpdateIgnoreData[uri, cst];

  If[$Debug2,
    log["after ParseIgnoreComments"];
    log["ignoreData: ", ignoreData]
  ];

  entry["IgnoreData"] = ignoreData;

  $OpenFilesMap[uri] = entry;

  {}
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runConcreteDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, cst, cstLints, suppressedRegions},

  log[1, "textDocument/runConcreteDiagnostics: enter"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],

    log[2, "stale"];

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


  suppressedRegions = Lookup[entry, "SuppressedRegions", Null];

  If[suppressedRegions == Null,
    suppressedRegions = SuppressedRegions[cst];
    entry["SuppressedRegions"] = suppressedRegions;
  ];

  log[2, "before CodeInspectCST"];

  cstLints = CodeInspectCST[cst, "AggregateRules" -> <||>, "AbstractRules" -> <||>, "SuppressedRegions" -> suppressedRegions];

  log[2, "after CodeInspectCST"];

  If[!MatchQ[cstLints, _List],
    log["cstLints: NOT A LIST!!!"];
    Throw[{}]
  ];

  log[2, "cstLints: ", #["Tag"]& /@ cstLints];

  entry["CSTLints"] = cstLints;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/runConcreteDiagnostics: exit"];

  {}
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runAggregateDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, agg, aggLints, suppressedRegions},

  log[1, "textDocument/runAggregateDiagnostics: Enter"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],

    log[2, "stale"];

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

  log[2, "before CodeInspectAgg"];

  aggLints = CodeInspectAgg[agg, "AbstractRules" -> <||>, "SuppressedRegions" -> suppressedRegions];

  log[2, "after CodeInspectAgg"];

  If[!MatchQ[aggLints, _List],
    log[2, "aggLints: NOT A LIST!!!"];
    Throw[{}]
  ];

  log[2, "aggLints: ", #["Tag"]& /@ aggLints];

  entry["AggLints"] = aggLints;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/runAggregateDiagnostics: Exit"];

  {}
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runAbstractDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, ast, cst, astLints, suppressedRegions},

  log[1, "textDocument/runAbstractDiagnostics: enter"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],

    log[2, "stale"];

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

  suppressedRegions = Lookup[entry, "SuppressedRegions", Null];

  If[suppressedRegions == Null,
    cst = entry["CST"];
    suppressedRegions = SuppressedRegions[cst];
    entry["SuppressedRegions"] = suppressedRegions;
  ];


  log[2, "before CodeInspectAST"];

  astLints = CodeInspectAST[ast, "SuppressedRegions" -> suppressedRegions];

  log[2, "after CodeInspectAST"];

  If[!MatchQ[astLints, _List],
    log[2, "astLints: NOT A LIST!!!"];
    Throw[{}]
  ];

  log[2, "astLints: ", #["Tag"]& /@ astLints];

  entry["ASTLints"] = astLints;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/runAbstractDiagnostics: exit"];

  {}
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runScopingDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, cst, scopingLints, scopingData, filtered, suppressedRegions, isActive},

  log[1, "textDocument/runScopingDiagnostics: enter"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],

    log[2, "stale"];

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

  log[2, "scopingData (up to 20): ", Take[scopingData, UpTo[20]]];

  (*
  Filter those that have non-empty modifiers
  *)
  filtered = Cases[scopingData, scopingDataObject[_, _, {_, ___}, _]];

  scopingLints = scopingDataObjectToLints /@ filtered;

  scopingLints = Flatten[scopingLints];

  (*
  Filter out suppressed
  *)

  suppressedRegions = Lookup[entry, "SuppressedRegions", Null];

  If[suppressedRegions == Null,
    cst = entry["CST"];
    suppressedRegions = SuppressedRegions[cst];
    entry["SuppressedRegions"] = suppressedRegions;
  ];

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

  log[2, "scopingLints: ", #["Tag"]& /@ scopingLints];

  entry["ScopingLints"] = scopingLints;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/runScopingDiagnostics: exit"];

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
  scopingData, locallyDefinedSymbols, contextErrors, contextLints,
  depSymbols, depSymbolsSet},

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
  Start with an empty lint list; file-local checks run unconditionally
  (they only need the AST / PacletIndex, not $WorkspaceRootPath).
  Workspace-wide checks (undefined symbols, context loading) are added
  only when a workspace root has been set.
  *)
  workspaceLints = {};

  cst = entry["CST"];

  (*
  Workspace-wide checks: undefined symbols and context loading errors.
  Only run when a workspace root has been set and the CST is available.
  *)
  If[$WorkspaceRootPath =!= None && !FailureQ[cst],

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
    WolframLanguageSyntax`Generate`$constants,
    If[ListQ[WolframLanguageSyntax`Generate`$options], WolframLanguageSyntax`Generate`$options, {}]
  ];

  (*
  Get paclet symbols
  *)
  pacletSymbols = GetPacletSymbols[];

  (*
  Get symbols from dependency contexts (BeginPackage/Needs) that are loaded in the kernel.
  These are NOT false-positive undefined — they're provided by a loaded package.
  *)
  depSymbols = LSPServer`PacletIndex`GetLoadedDependencySymbols[uri];
  depSymbolsSet = Association[Thread[depSymbols -> True]];

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
          !KeyExistsQ[depSymbolsSet, name],
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
  workspaceLints = Join[workspaceLints, Table[
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
  ]];

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

  workspaceLints = Join[workspaceLints, contextLints]

  ]; (* end If[$WorkspaceRootPath =!= None] *)

  (*
  Check for function overloads missing doc-comments.

  For each symbol that has at least one "function" definition with a DocComment,
  we flag any other "function" definitions for that symbol in this file that
  lack a DocComment.  This allows users who adopt doc-comments to be reminded
  to document every overload.

  We only warn if at least one overload in the file already has a DocComment,
  so we don't spam projects that haven't opted in at all.
  *)
  Module[{fileDefsRaw, fileDefsBySymbol, docCommentLints},

    (*
    Pull all function definitions that belong to this file from the index.
    *)
    fileDefsRaw = Flatten[
      Function[{symName},
        Module[{defs},
          defs = GetSymbolDefinitions[symName];
          Select[defs, #["uri"] === uri && #["kind"] === "function" &] /.
            d_Association :> Append[d, "symbolName" -> symName]
        ]
      ] /@ GetPacletSymbols[],
      1
    ];

    (*
    Group by symbol name.
    *)
    fileDefsBySymbol = GroupBy[fileDefsRaw, #["symbolName"]&];

    docCommentLints = Flatten[
      KeyValueMap[
        Function[{symName, defs},
          Module[{hasAnyDocComment, missing},
            (*
            Only warn when at least one overload in this file is documented.
            *)
            hasAnyDocComment = AnyTrue[defs, AssociationQ[#["DocComment"]] &];
            If[!hasAnyDocComment,
              Return[{}]
            ];
            (*
            Flag every overload that is missing a doc-comment.
            *)
            missing = Select[defs, !AssociationQ[#["DocComment"]] &];
            InspectionObject[
              "MissingDocComment",
              "Function \"" <> symName <> "\" overload is missing a doc-comment. " <>
                "Add a (* Return: _Pattern *) comment (or (* Description: ... * Return: ... *)) immediately before this definition.",
              "Warning",
              <|
                Source -> #["source"],
                ConfidenceLevel -> 0.9,
                "Argument" -> symName
              |>
            ]& /@ missing
          ]
        ],
        fileDefsBySymbol
      ],
      1
    ];

    workspaceLints = Join[workspaceLints, docCommentLints]
  ];

  (*
  Check function call sites against doc-comment Param: input patterns.

  For every function call f[arg1, ..., argN] in the file where:
    - f has at least one definition in the PacletIndex with a DocComment that carries
      non-empty InputPatterns, AND
    - every such documented overload has the same arity as the call, AND
    - every documented overload's InputPatterns reject at least one literal argument,
  we emit a "DocCommentInputMismatch" warning pointing at the offending argument.

  We only fire when ALL documented overloads declare InputPatterns and none of them
  can accept the arguments; if any overload lacks InputPatterns we assume it is a
  general catch-all and suppress the warning.
  *)
  Module[{ast, defLHSSources, callNodes, inputMismatchLints, localDefMap, localVarMap,
          branchBodyRanges, findEnclosingBranch,
          extractVarConstraintsFromCond, switchPatternToSample,
          condEntriesForBranch, condEntriesForBranchFalse,
          returnMismatchLints, patternToSampleValue, inferRHSSample, defRHSByLine},

    ast = entry["AST"];

    If[!FailureQ[ast],

      (*
      Collect source ranges of all definition LHS call nodes so we can exclude
      them from call-site checking.
      *)
      defLHSSources = Association[
        Cases[ast,
          CallNode[
            LeafNode[Symbol, "Set" | "SetDelayed" | "UpSet" | "UpSetDelayed" | "TagSet" | "TagSetDelayed", _],
            {lhs:CallNode[_, _, KeyValuePattern[Source -> lhsSrc_]], __},
            _] :> lhsSrc -> True,
          Infinity
        ]
      ];

      (*
      Build a local map of funcName -> list of InputPatterns lists, derived
      directly from definition LHS nodes in this file's AST.
      This works without a populated PacletIndex and handles the common case
      of calling a function defined in the same file.
      Each entry is a list of type-name strings (or None for untyped blanks).
      *)
      localDefMap = GroupBy[
        Cases[ast,
          CallNode[
            LeafNode[Symbol, "Set" | "SetDelayed", _],
            {lhs:CallNode[LeafNode[Symbol, funcName_String, _], args_List, _], _},
            _] :> <|
              "name" -> funcName,
              "InputPatterns" -> LSPServer`PacletIndex`ExtractLHSInputPatterns[lhs],
              (* Flag variadic overloads so arity checking skips them *)
              "Variadic" -> AnyTrue[args,
                MatchQ[#,
                  CallNode[LeafNode[Symbol, "Pattern", _],
                    {_, CallNode[LeafNode[Symbol, "BlankSequence" | "BlankNullSequence", _], _, _]}, _] |
                  CallNode[LeafNode[Symbol, "BlankSequence" | "BlankNullSequence", _], _, _] |
                  CallNode[LeafNode[Symbol, "Optional", _], _, _]
                ] &
              ]
            |>,
          Infinity
        ],
        #["name"] &
      ];

      (*
      Helper: infer a representative WL sample value from a literal AST node,
      recursing into List constructors.  No variable lookup - only handles literal
      leaves and structural list/assoc literals.  Used by localVarMap construction
      BEFORE inferArgSampleValue is defined (which adds Symbol lookup on top).
      *)
      inferLiteralNodeValue = Function[{node},
        Which[
          MatchQ[node, LeafNode[Integer,   _, _]], 0,
          MatchQ[node, LeafNode[String,    _, _]], "",
          MatchQ[node, LeafNode[Real,      _, _]], 0.,
          MatchQ[node, LeafNode[Rational,  _, _]], 1/2,
          MatchQ[node, CallNode[LeafNode[Symbol, "Association", _], _List, _]],
            Module[{pairs},
              pairs = Cases[node[[2]],
                CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _],
                  {LeafNode[String, keyStr_String, _], valNode_}, _] :>
                  (StringTake[keyStr, {2, -2}] -> inferLiteralNodeValue[valNode])
              ];
              If[Length[pairs] === 0 || AnyTrue[pairs[[All, 2]], # === Missing["Unknown"] &],
                Missing["Unknown"],
                Association[pairs]
              ]
            ],
          MatchQ[node, CallNode[LeafNode[Symbol, "List", _], _List, _]],
            Module[{elems = inferLiteralNodeValue /@ node[[2]]},
              If[AnyTrue[elems, # === Missing["Unknown"] &],
                Missing["Unknown"],
                elems
              ]
            ],
          True, Missing["Unknown"]
        ]
      ];

      (*
      Helper: convert a WL return-type pattern (e.g. _Integer) to a representative
      sample value.  Used when inferring variable types from function return annotations.
      Returns Missing["Unknown"] for unrecognized or parametric patterns.
      *)
      patternToSampleValue = Function[{pat},
        Which[
          pat === _?BooleanQ,                       True,
          pat === _Integer,                          0,
          pat === _String,                           "",
          pat === _Real,                             0.,
          pat === _Rational,                         1/2,
          pat === _Complex,                          Complex[0, 1],
          pat === _List,                             {},
          pat === _Association,                      <||>,
          (* _?SomePredicate (not BooleanQ) - typed but non-boolean representative *)
          MatchQ[pat, PatternTest[Blank[], _]],      0,
          True, Missing["Unknown"]
        ]
      ];

      (*
      Helper: given a call-node RHS expression, attempt to infer a representative
      sample value by looking up the callee's return type.
      Checks $BuiltinPatterns first (System` functions), then $PacletIndex for
      user-defined functions whose doc-comment Return: annotations all agree on type.
      Only handles fixed return types; passthrough (_argN) and unknown yields Missing["Unknown"].
      *)
      inferCallNodeReturnSampleValue = Function[{node},
        If[MatchQ[node, CallNode[LeafNode[Symbol, _String, _], _List, _]],
          With[{fname = node[[1, 2]]},
            Which[
              (* Builtin with a known fixed return type *)
              KeyExistsQ[$BuiltinPatterns, fname],
                Switch[$BuiltinPatterns[fname][[1, 2]],
                  "_?BooleanQ", True,          (* known boolean return *)
                  "_Integer",   0,
                  "_String",    "",
                  "_Real",      0.,
                  "_Rational",  1/2,
                  "_List",      {},
                  "_Association", <||>,
                  None,         Missing["Unknown"],  (* no return type info *)
                  _String,      0  (* any other known return type string -> non-boolean *)
                ],
              (* User-defined: use DocComment ReturnPattern first,
                 then fall back to automatically InferredReturnPattern. *)
              KeyExistsQ[$PacletIndex["Symbols"], fname] &&
                Length[$PacletIndex["Symbols", fname, "Definitions"]] > 0,
                Module[{defs, retPats, irps},
                  defs = $PacletIndex["Symbols", fname, "Definitions"];
                  retPats = DeleteMissing[
                    Map[Function[d,
                      Lookup[Lookup[d, "DocComment", <||>], "ReturnPattern", Missing["Unknown"]]
                    ], defs]
                  ];
                  Which[
                    Length[retPats] > 0 && Length[DeleteDuplicates[retPats]] === 1,
                      patternToSampleValue[retPats[[1]]],
                    (* Fall back: all definitions agree on an InferredReturnPattern *)
                    True,
                      irps = DeleteCases[
                        Lookup[defs, "InferredReturnPattern", None],
                        None | _Missing
                      ];
                      If[Length[irps] > 0 && Length[DeleteDuplicates[irps]] === 1,
                        patternToSampleValue[irps[[1]]],
                        Missing["Unknown"]
                      ]
                  ]
                ],
              True, Missing["Unknown"]
            ]
          ],
          Missing["Unknown"]
        ]
      ];

      (*
      Collect source ranges of every branch-body argument of If / Which / Switch.
      Each entry is {{startLine, startCol}, {endLine, endCol}}.
      These ranges are used to scope variable assignments to their enclosing branch so
      that a "myVar = 42" inside the True-clause of an If is not treated as evidence
      that myVar is an Integer when checking calls in the False-clause or after the If.
      *)
      branchBodyRanges = Flatten[
        Cases[ast,
          CallNode[LeafNode[Symbol, ifSym_String, _], args_List, _] /;
            MemberQ[{"If", "Which", "Switch"}, ifSym] :>
            Module[{idxs},
              (* Branch-value arg positions:
                 If:     2, 3, 4   (skip condition at position 1)
                 Which:  2, 4, 6   (value args at even positions)
                 Switch: 3, 5, 7   (value args at odd positions >= 3) *)
              idxs = Switch[ifSym,
                "If",     Range[2, Length[args]],
                "Which",  Range[2, Length[args], 2],
                "Switch", Range[3, Length[args], 2],
                _,        {}
              ];
              Map[
                Function[i,
                  With[{src = Quiet[args[[i, 3, Key[Source]]]]},
                    If[MatchQ[src, {{_Integer, _Integer}, {_Integer, _Integer}}],
                      src, Nothing]
                  ]
                ],
                Select[idxs, # <= Length[args] &]
              ]
            ],
          Infinity
        ],
        1
      ];

      (*
      Given a line number, return the source range of the innermost If/Which/Switch
      branch body containing that line, or None if the line is at top scope.
      "Innermost" = smallest range (by line span) that still contains the line.
      *)
      findEnclosingBranch = Function[{line},
        Module[{enclosing},
          enclosing = Select[branchBodyRanges,
            Function[r, r[[1, 1]] <= line <= r[[2, 1]]]];
          If[enclosing === {}, None,
            MinimalBy[enclosing, Function[r, r[[2, 1]] - r[[1, 1]]]][[1]]
          ]
        ]
      ];

      (*
      Extract variable type constraints implied by a condition node being True.
      Used to inject narrowed types into If/Which branch bodies based on their guard.
      Returns a list of {varName, sampleValue} pairs.
      Examples:
        StringQ[x]         -> {"x" -> ""}
        x == 3             -> {"x" -> 3}
        And[StringQ[x], IntegerQ[y]] -> {"x" -> "", "y" -> 0}
      Only positive (true-branch) constraints are extracted; negations are skipped
      except that Not[pred] / !pred are handled by inverting the branch target.
      *)
      extractVarConstraintsFromCond = Function[{condNode},
        Which[
          (* Predicate test: StringQ[x], IntegerQ[x], ListQ[x], etc. *)
          MatchQ[condNode, CallNode[LeafNode[Symbol, _String, _],
                                   {LeafNode[Symbol, _String, _]}, _]],
            Module[{testFn = condNode[[1, 2]], varName = condNode[[2, 1, 2]], sv},
              sv = Switch[testFn,
                "StringQ" | "Internal`SymbolNameQ",        "",
                "IntegerQ" | "MachineIntegerQ" |
                  "Internal`NonNegativeIntegerQ",           0,
                "NumberQ" | "NumericQ" | "InexactNumberQ", 0.,
                "ListQ" | "VectorQ" | "MatrixQ",           {},
                "AssociationQ",                            <||>,
                _,                                         Missing["Unknown"]
              ];
              If[sv === Missing["Unknown"], {}, {{varName, sv}}]
            ],

          (* Equality: x == literal  or  x === literal *)
          MatchQ[condNode, CallNode[LeafNode[Symbol, "Equal" | "SameQ", _],
                                   {LeafNode[Symbol, _String, _], _}, _]],
            Module[{varName = condNode[[2, 1, 2]],
                    lv = inferLiteralNodeValue[condNode[[2, 2]]]},
              If[lv === Missing["Unknown"], {}, {{varName, lv}}]
            ],

          (* Equality: literal == x  or  literal === x *)
          MatchQ[condNode, CallNode[LeafNode[Symbol, "Equal" | "SameQ", _],
                                   {_, LeafNode[Symbol, _String, _]}, _]],
            Module[{varName = condNode[[2, 2, 2]],
                    lv = inferLiteralNodeValue[condNode[[2, 1]]]},
              If[lv === Missing["Unknown"], {}, {{varName, lv}}]
            ],

          (* Head[x] === TypeName: narrow by head identity *)
          MatchQ[condNode, CallNode[LeafNode[Symbol, "SameQ", _],
                                   {CallNode[LeafNode[Symbol, "Head", _],
                                             {LeafNode[Symbol, _String, _]}, _],
                                    LeafNode[Symbol, _String, _]}, _]],
            Module[{varName = condNode[[2, 1, 2, 1, 2]],
                    headName = condNode[[2, 2, 2]], sv},
              sv = Switch[headName,
                "Integer",     0,
                "Real",        0.,
                "String",      "",
                "List",        {},
                "Association", <||>,
                _,             Missing["Unknown"]
              ];
              If[sv === Missing["Unknown"], {}, {{varName, sv}}]
            ],

          (* And[c1, c2, ...]: union of constraints from all conjuncts *)
          MatchQ[condNode, CallNode[LeafNode[Symbol, "And", _], _List, _]],
            Flatten[extractVarConstraintsFromCond /@ condNode[[2]], 1],

          True, {}
        ]
      ];

      (*
      Map a Switch pattern AST node to a representative sample value for the
      variable being matched.  Returns Missing["Unknown"] for unrecognised patterns.
      *)
      switchPatternToSample = Function[{patNode},
        Which[
          (* _TypeName blank: _Integer, _String, _Real, etc. *)
          MatchQ[patNode, CallNode[LeafNode[Symbol, "Blank", _],
                                  {LeafNode[Symbol, _String, _]}, _]],
            Switch[patNode[[2, 1, 2]],
              "String" | "Symbol", "",
              "Integer",            0,
              "Real",               0.,
              "List",               {},
              "Association",        <||>,
              _,                    Missing["Unknown"]
            ],

          (* Named pattern x_TypeName *)
          MatchQ[patNode, CallNode[LeafNode[Symbol, "Pattern", _],
                                  {_, CallNode[LeafNode[Symbol, "Blank", _],
                                              {LeafNode[Symbol, _String, _]}, _]}, _]],
            Switch[patNode[[2, 2, 2, 1, 2]],
              "String" | "Symbol", "",
              "Integer",            0,
              "Real",               0.,
              "List",               {},
              "Association",        <||>,
              _,                    Missing["Unknown"]
            ],

          (* Literal used as a pattern: 3, "hello", 1.5, 1/2 *)
          MatchQ[patNode, LeafNode[Integer | String | Real | Rational, _, _]],
            inferLiteralNodeValue[patNode],

          (* Catch-all blanks _ / __ / ___ or PatternTest _?f:
             the variable's type remains unknown in this branch. *)
          True, Missing["Unknown"]
        ]
      ];

      (*
      Given a condition AST node and a branch body source range, build a list of
      {varName, branchStartLine, sampleValue, branchSrc} entries.  These act as
      implicit "virtual assignments" at the very start of the branch, narrowing the
      inferred type of variables that appear in the condition.
      *)
      condEntriesForBranch = Function[{condNode, branchSrc},
        If[!MatchQ[branchSrc, {{_Integer, _Integer}, {_Integer, _Integer}}],
          {},
          Map[
            Function[pair, {pair[[1]], branchSrc[[1, 1]], pair[[2]], branchSrc}],
            extractVarConstraintsFromCond[condNode]
          ]
        ]
      ];

      (*
      Compute condition-narrowing entries for the FALSE (else) branch.
      Three cases yield useful narrowing:
        1. Not[pred[x]]  ->  false branch has pred[x]=True  ->  x gets positive type
        2. x == literal  ->  false branch has x != literal  ->  x gets Except[literal]
           (Except[v] is handled specially in argMatchesPattern: only definitively
            excluded from patterns that are structurally equal to v.)
        3. TypeQ[x]      ->  false branch has !TypeQ[x]     ->  x gets Except[Blank[Type]]
      *)
      condEntriesForBranchFalse = Function[{condNode, branchSrc},
        If[!MatchQ[branchSrc, {{_Integer, _Integer}, {_Integer, _Integer}}],
          {},
          Which[
            (* Not[innerCond]: false branch satisfies innerCond positively *)
            MatchQ[condNode, CallNode[LeafNode[Symbol, "Not", _], {_}, _]],
              condEntriesForBranch[condNode[[2, 1]], branchSrc],

            (* x == literal  or  x === literal: false branch has x != literal *)
            MatchQ[condNode, CallNode[LeafNode[Symbol, "Equal" | "SameQ", _],
                                     {LeafNode[Symbol, _String, _], _}, _]],
              Module[{varName = condNode[[2, 1, 2]],
                      lv = inferLiteralNodeValue[condNode[[2, 2]]]},
                If[lv === Missing["Unknown"], {},
                  {{varName, branchSrc[[1, 1]], Except[lv], branchSrc}}]
              ],

            (* literal == x  or  literal === x *)
            MatchQ[condNode, CallNode[LeafNode[Symbol, "Equal" | "SameQ", _],
                                     {_, LeafNode[Symbol, _String, _]}, _]],
              Module[{varName = condNode[[2, 2, 2]],
                      lv = inferLiteralNodeValue[condNode[[2, 1]]]},
                If[lv === Missing["Unknown"], {},
                  {{varName, branchSrc[[1, 1]], Except[lv], branchSrc}}]
              ],

            (* TypeQ[x] predicate: false branch means the type does NOT hold *)
            MatchQ[condNode, CallNode[LeafNode[Symbol, _String, _],
                                     {LeafNode[Symbol, _String, _]}, _]],
              Module[{testFn = condNode[[1, 2]], varName = condNode[[2, 1, 2]], negPat},
                negPat = Switch[testFn,
                  "StringQ" | "Internal`SymbolNameQ",                 Blank[String],
                  "IntegerQ" | "MachineIntegerQ" |
                    "Internal`NonNegativeIntegerQ",                   Blank[Integer],
                  "NumberQ" | "NumericQ" | "InexactNumberQ",          Blank[Number],
                  "ListQ"   | "VectorQ"  | "MatrixQ",                 Blank[List],
                  "AssociationQ",                                     Blank[Association],
                  _,                                                  Missing["Unknown"]
                ];
                If[negPat === Missing["Unknown"], {},
                  {{varName, branchSrc[[1, 1]], Except[negPat], branchSrc}}]
              ],

            True, {}
          ]
        ]
      ];

      (*
      Build a position-aware map of varName -> sorted list of {assignLine, sampleValue, branchRange}.
      Covers literal assignments (myIntVar = 42) and call-result assignments
      where the callee's return type is statically known (myLen = StringLength[s]).
      Each entry's branchRange is the source range {{r1,c1},{r2,c2}} of the innermost
      If/Which/Switch branch body containing the assignment, or None for top-scope
      assignments.  Branch-scoped entries are only used when the variable usage is
      inside the same branch; top-scope entries are always available.
      Additionally, "convergence entries" are synthesised when ALL branches of an
      If/Which/Switch assign the same variable to values of the same head: in that case
      a top-scope entry is emitted just after the conditional, letting code that follows
      it see the variable's type.
      e.g. myListVar = {1,2,3}  =>  <|"myListVar" -> {{1, {0,0,0}, None}}|>
      e.g. myLen = StringLength["hi"]  =>  <|"myLen" -> {{1, 0, None}}|>  (Integer)
      *)
      localVarMap =
        Module[{rawEntries, convergenceEntries, condEntries, paramEntries,
                iterEntries, mapParamEntries, allEntries},
          rawEntries = Cases[ast,
            CallNode[
              LeafNode[Symbol, "Set", _],
              {LeafNode[Symbol, varName_String, _], rhs_},
              meta_] :>
              Module[{litVal = inferLiteralNodeValue[rhs], sampleVal, assignLine, assignBranch},
                sampleVal = If[litVal =!= Missing["Unknown"],
                  litVal,
                  inferCallNodeReturnSampleValue[rhs]
                ];
                (* Always record the assignment, even when sampleVal is Missing["Unknown"].
                   This ensures that a reassignment whose return type cannot be inferred
                   clears any previously known type: the lookup always takes the last
                   entry before the usage, and Missing["Unknown"] causes the check to pass. *)
                assignLine = meta[[Key[Source], 1, 1]];
                assignBranch = findEnclosingBranch[assignLine];
                {varName, assignLine, sampleVal, assignBranch}
              ],
            Infinity
          ];

          (* Convergence: for each If/Which/Switch, when ALL branches assign the
             same variable to values of the same head (same WL type), emit one
             top-scope entry placed at the line after the conditional ends.  This
             allows code following the conditional to see the variable's type even
             though every individual branch assignment is branch-scoped. *)
          convergenceEntries = Flatten[
            Cases[ast,
              CallNode[LeafNode[Symbol, ifSym_String, _], args_List, ifMeta_] /;
                MemberQ[{"If", "Which", "Switch"}, ifSym] :>
                Module[{branchIdxs, branchSrcs, afterLine, perBranchMaps, commonVars, cEntries},
                  branchIdxs = Switch[ifSym,
                    "If",     Range[2, Length[args]],
                    "Which",  Range[2, Length[args], 2],
                    "Switch", Range[3, Length[args], 2],
                    _,        {}
                  ];
                  branchIdxs = Select[branchIdxs, # <= Length[args] &];
                  If[Length[branchIdxs] < 2,
                    Nothing,
                    branchSrcs = Map[Function[i, Quiet[args[[i, 3, Key[Source]]]]], branchIdxs];
                    If[!AllTrue[branchSrcs,
                         MatchQ[#, {{_Integer, _Integer}, {_Integer, _Integer}}] &],
                      Nothing,
                      afterLine = Quiet[ifMeta[[Key[Source], 2, 1]]] + 1;
                      If[!IntegerQ[afterLine], Nothing,
                        (* For each branch, collect its assignments from rawEntries *)
                        perBranchMaps = Map[Function[bSrc,
                          GroupBy[
                            Select[rawEntries, Function[e, e[[4]] === bSrc]],
                            First,       (* group by varName *)
                            Function[es, Last[SortBy[es, #[[2]] &]][[3]]]  (* last sampleVal per var *)
                          ]
                        ], branchSrcs];
                        commonVars = If[Length[perBranchMaps] > 0,
                          Intersection @@ (Keys /@ perBranchMaps), {}];
                        cEntries = Map[Function[v,
                          Module[{branchVals = (Lookup[#, v, Missing["Unknown"]] & /@ perBranchMaps)},
                            If[!MemberQ[branchVals, Missing["Unknown"]] &&
                               Length[DeleteDuplicates[Head /@ branchVals]] === 1,
                              (* All branches agree on Head (same type) *)
                              {v, afterLine, branchVals[[1]], None},
                              Nothing
                            ]
                          ]
                        ], commonVars];
                        Sequence @@ Select[cEntries, ListQ]
                      ]
                    ]
                  ]
                ],
              Infinity
            ],
            1
          ];

          (* Condition-narrowing entries: for each If/Which/Switch, analyse the guard
             expression(s) to extract variable type constraints and inject them as
             virtual assignments at the start of each branch body.  This lets e.g.
               If[StringQ[x], f[x], ...]  →  x is seen as String inside the true branch
               If[x == 3, f[x], ...]      →  x is seen as integer 3 inside the true branch
               Switch[x, _Integer, f[x]]  →  x is seen as Integer inside that branch

             For If[cond, t, f]:
               - True branch (t): apply positive constraints from cond.
               - False branch (f): apply positive constraints from cond if cond is
                 Not[pred] or !pred (invert target), so If[!StringQ[x], ..., f[x]]
                 correctly sees x as String in the false branch.
             For Which[c1, v1, c2, v2, ...]:
               - Each branch vi gets positive constraints from ci.
             For Switch[expr, p1, v1, p2, v2, ...]:
               - Each branch vi gets a constraint on expr (if expr is a Symbol)
                 derived from pattern pi (_Integer, _String, literal, etc.).
          *)
          condEntries = Flatten[
            Cases[ast,
              CallNode[LeafNode[Symbol, ifSym_String, _], args_List, _] /;
                MemberQ[{"If", "Which", "Switch"}, ifSym] :>
                Switch[ifSym,

                  "If",
                    If[Length[args] < 2, {},
                      Join[
                        (* True branch: positive constraints from condition *)
                        condEntriesForBranch[
                          args[[1]],
                          Quiet[args[[2, 3, Key[Source]]]]
                        ],
                        (* False branch: handled by condEntriesForBranchFalse, which
                           covers Not[pred] -> positive pred constraints, and
                           x == literal -> Except[literal] for the variable. *)
                        If[Length[args] >= 3,
                          condEntriesForBranchFalse[
                            args[[1]],
                            Quiet[args[[3, 3, Key[Source]]]]
                          ],
                          {}
                        ]
                      ]
                    ],

                  "Which",
                    Flatten[
                      Table[
                        If[i + 1 <= Length[args],
                          condEntriesForBranch[
                            args[[i]],
                            Quiet[args[[i + 1, 3, Key[Source]]]]
                          ],
                          {}
                        ],
                        {i, 1, Length[args] - 1, 2}
                      ],
                      1
                    ],

                  "Switch",
                    If[Length[args] < 3 ||
                       !MatchQ[args[[1]], LeafNode[Symbol, _String, _]],
                      {},
                      Module[{varName = args[[1, 2]]},
                        Flatten[
                          Table[
                            If[i + 1 <= Length[args],
                              Module[{sv = switchPatternToSample[args[[i]]],
                                      branchSrc = Quiet[args[[i + 1, 3, Key[Source]]]]},
                                If[sv === Missing["Unknown"] ||
                                   !MatchQ[branchSrc, {{_Integer, _Integer}, {_Integer, _Integer}}],
                                  {},
                                  {{varName, branchSrc[[1, 1]], sv, branchSrc}}
                                ]
                              ],
                              {}
                            ],
                            {i, 2, Length[args] - 1, 2}
                          ],
                          1
                        ]
                      ]
                    ],

                  _, {}
                ],
              Infinity
            ],
            1
          ];

          (* Parameter entries: for each function definition f[x_Integer, y_String] := ...,
             inject one entry per patterned argument into the localVarMap so that
             inferArgSampleValue can resolve x/y inside the function body. Each entry
             is scoped to the definition's source range (used as assignBranch) so that
             a parameter does not bleed across unrelated definitions with the same name. *)
          paramEntries = Flatten[
            Cases[ast,
              CallNode[
                LeafNode[Symbol, "SetDelayed" | "Set", _],
                {CallNode[_, args_List, _], _},
                KeyValuePattern[Source -> defSrc_]] :>
                Cases[args,
                  CallNode[LeafNode[Symbol, "Pattern", _],
                    {LeafNode[Symbol, varName_String, _], blankNode_},
                    _] :>
                    Module[{sv = patternNodeToSample[blankNode]},
                      If[sv === Missing["Unknown"],
                        Nothing,
                        {varName, defSrc[[1, 1]], sv, defSrc}
                      ]
                    ],
                  Infinity],
              Infinity],
            1
          ];

          (* Iterator-variable entries: Table/Do/Sum/Product/Array {i, lo, hi} → i is Integer;
             {i, listExpr} → i gets element type of listExpr.
             Entries are scoped to the whole iterator-call range. *)
          iterEntries = Flatten[
            Cases[ast,
              CallNode[LeafNode[Symbol, "Table"|"Do"|"Sum"|"Product"|"Array", _], args_List,
                KeyValuePattern[Source -> iterSrc_]] :>
                Flatten[Cases[Rest[args],
                  spec_CallNode :>
                    If[!MatchQ[spec, CallNode[LeafNode[Symbol, "List", _],
                                              {LeafNode[Symbol, _, _], __}, _]],
                      Nothing,
                      Module[{varName = spec[[2, 1, 2]], specArgs = spec[[2]], sv},
                        sv = If[Length[specArgs] == 2 &&
                                MatchQ[specArgs[[2]], CallNode[LeafNode[Symbol, "List", _], _, _]],
                          inferListElementSample[specArgs[[2]]],   (* {i, listExpr} *)
                          0                                         (* {i, lo} / {i, lo, hi} / … *)
                        ];
                        If[sv === Missing["Unknown"], Nothing,
                          {varName, iterSrc[[1, 1]], sv, iterSrc}]
                      ]
                    ],
                  1], 1],
              Infinity],
            1
          ];

          (* Map-parameter entries: Map/Scan/Select/Pick[Function[{a,...}, body], list]
             Inject one entry per parameter with the inferred element type of the list.
             Entries are scoped to the Function's source range. *)
          mapParamEntries = Flatten[
            Cases[ast,
              CallNode[LeafNode[Symbol, mapFn_String, _], args_List, _] /;
                MemberQ[{"Map", "Scan", "Select", "Pick", "MapIndexed", "MapThread"}, mapFn] :>
                Module[{func, list, funcSrc, params, sv},
                  If[Length[args] < 2, Nothing,
                    {func, list} = Switch[mapFn,
                      "Select" | "Pick", {args[[2]], args[[1]]},
                      _,                 {args[[1]], args[[2]]}
                    ];
                    If[!MatchQ[func, CallNode[LeafNode[Symbol, "Function", _],
                                              {CallNode[LeafNode[Symbol, "List", _], _List, _], _}, _]],
                      Nothing,
                      funcSrc = Quiet[func[[3, Key[Source]]]];
                      If[!MatchQ[funcSrc, {{_Integer, _Integer}, {_Integer, _Integer}}], Nothing,
                        params = func[[2, 1, 2]];
                        sv = inferListElementSample[list];
                        If[sv === Missing["Unknown"], Nothing,
                          Cases[params, LeafNode[Symbol, varName_String, _] :>
                            {varName, funcSrc[[1, 1]], sv, funcSrc}, 1]
                        ]
                      ]
                    ]
                  ]
                ],
              Infinity],
            1
          ];

          allEntries = Join[condEntries, rawEntries, convergenceEntries,
                            paramEntries, iterEntries, mapParamEntries];
          GroupBy[
            allEntries,
            First,
            Function[entries, SortBy[Map[Function[e, {e[[2]], e[[3]], e[[4]]}], entries], First]]
          ]
        ];

      (*
      Find call nodes that appear in non-definition positions.
      *)
      callNodes = Cases[ast,
        node:CallNode[
          LeafNode[Symbol, funcName_String, _],
          args_List,
          KeyValuePattern[Source -> callSrc_]] /;
            !KeyExistsQ[defLHSSources, callSrc] :> node,
        Infinity
      ];

      (*
      Helper: convert a Blank/BlankSequence pattern node to a representative sample value.
      Used to infer default parameter types from function LHS patterns like x_Integer.
      *)
      patternNodeToSample = Function[{blankNode},
        Switch[blankNode,
          CallNode[LeafNode[Symbol, "Blank", _], {LeafNode[Symbol, "Integer", _]}, _],    0,
          CallNode[LeafNode[Symbol, "Blank", _], {LeafNode[Symbol, "String", _]}, _],     "",
          CallNode[LeafNode[Symbol, "Blank", _], {LeafNode[Symbol, "Real", _]}, _],       0.,
          CallNode[LeafNode[Symbol, "Blank", _], {LeafNode[Symbol, "Rational", _]}, _],   1/2,
          CallNode[LeafNode[Symbol, "Blank", _], {LeafNode[Symbol, "Complex", _]}, _],    Complex[0, 0],
          CallNode[LeafNode[Symbol, "Blank", _], {LeafNode[Symbol, "List", _]}, _],       {},
          CallNode[LeafNode[Symbol, "Blank", _], {LeafNode[Symbol, "Association", _]}, _], <||>,
          CallNode[LeafNode[Symbol, "Blank", _], {LeafNode[Symbol, "Symbol", _]}, _],     Unique[],
          _, Missing["Unknown"]
        ]
      ];

      (*
      Helper: infer a representative sample value for elements of a list-valued expression.
      Used to type iterator variables (Table, Do, …) and Function parameters inside Map/Scan/etc.
        {1, 2, 3}      -> 0          (all integers)
        {"a", "b"}     -> ""         (all strings)
        Range[n]       -> 0          (always integers)
        mixed / unknown -> Missing["Unknown"]
      *)
      inferListElementSample = Function[{listNode},
        Which[
          MatchQ[listNode, CallNode[LeafNode[Symbol, "List", _], {_, ___}, _]],
            Module[{samples = DeleteDuplicates[
                DeleteCases[inferLiteralNodeValue /@ listNode[[2]], _Missing]]},
              If[Length[samples] === 1, samples[[1]], Missing["Unknown"]]
            ],
          MatchQ[listNode, CallNode[LeafNode[Symbol, "Range", _], {__}, _]],
            0,
          True, Missing["Unknown"]
        ]
      ];

      (*
      Helper: infer a representative sample value for a call-site argument node.
      Recurses into List constructors for structural shape inference.
      Also resolves locally-defined variables whose literal assignment was recorded.
      Returns Missing["Unknown"] when the argument is not statically determinable.
      *)
      inferArgSampleValue = Function[{argNode},
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
                  (StringTake[keyStr, {2, -2}] -> inferArgSampleValue[valNode])
              ];
              If[Length[pairs] === 0 || AnyTrue[pairs[[All, 2]], # === Missing["Unknown"] &],
                Missing["Unknown"],
                Association[pairs]
              ]
            ],
          (* List: recurse so {1., 3.} yields {0., 0.} which MatchQ tests against {_Real, _Real} *)
          MatchQ[argNode, CallNode[LeafNode[Symbol, "List", _], _List, _]],
            Module[{elems = inferArgSampleValue /@ argNode[[2]]},
              If[AnyTrue[elems, # === Missing["Unknown"] &],
                Missing["Unknown"],
                elems
              ]
            ],
          (* Symbol whose sample value is known from a local assignment;
             pick the most recent definition textually at or before this usage line,
             respecting branch scoping: branch-scoped assignments are only visible to
             usages that are inside that same branch body (including usages nested
             deeper inside inner-branches of that body); top-scope (None) assignments
             are always visible. *)
          MatchQ[argNode, LeafNode[Symbol, _String, _]] && KeyExistsQ[localVarMap, argNode[[2]]],
            Module[{varName = argNode[[2]], entries, argLine, valid},
              entries = localVarMap[varName];  (* sorted {{line, sampleVal, branchRange}, ...} *)
              argLine = argNode[[3, Key[Source], 1, 1]];
              valid = Select[entries, Function[e,
                e[[1]] <= argLine &&
                (* Valid if:
                   (a) top-scope assignment (None - no branch restriction), OR
                   (b) usage line falls within the assignment's branch body range,
                       meaning the usage is inside (or nested inside) that branch *)
                (e[[3]] === None || (
                  MatchQ[e[[3]], {{_Integer, _Integer}, {_Integer, _Integer}}] &&
                  e[[3, 1, 1]] <= argLine <= e[[3, 2, 1]]
                ))
              ]];
              If[Length[valid] > 0, Last[valid][[2]], Missing["Unknown"]]
            ],
          True, Missing["Unknown"]
        ]
      ];

      (*
      Helper: does an argument sample value satisfy a declared input pattern expression?
      patExpr is a WL pattern like _Integer, {_Real, _Real}, or Blank[] (untyped).
      Blank[] accepts anything; structured patterns like {_Real, _Real} are checked
      recursively by MatchQ, matching both type and shape.
      Returns True when compatible or when the argument is not statically determinable.
      *)
      argMatchesPattern = Function[{argSample, patExpr},
        argSample === Missing["Unknown"] ||   (* non-literal: can't check, assume OK *)
        If[Head[argSample] === Except,
          argSample[[1]] =!= patExpr,         (* Except[v]: only excluded from a pattern
                                                 that is structurally identical to v itself.
                                                 Broad type patterns (_Integer etc.) are
                                                 assumed to match conservatively. *)
          MatchQ[argSample, patExpr]          (* Normal concrete sample: direct match *)
        ]
      ];

      (*
      For each call node, look up the callee's definitions and check InputPatterns.
      *)
      inputMismatchLints = Flatten[
        Function[{callNode},
          Catch[
          Module[{funcName, callArgs, callSrc, allDefs, localEntries,
            argSamples, mismatchIdx, badArg, badArgSrc, symptomatic},

            funcName = callNode[[1, 2]];
            callArgs = callNode[[2]];
            callSrc  = callNode[[3, Key[Source]]];

            (*
            Structural argument-count validation for Which and Switch.
            These functions have variadic alternating argument structures that the generic
            InputPatterns machinery cannot express, so they get dedicated early checks here.
            Valid calls return {} immediately (skipping the generic $BuiltinPatterns check
            which would produce false positives for these functions).
            *)
            If[funcName === "Which",
              Throw[Which[
                OddQ[Length[callArgs]],
                  {InspectionObject[
                    "WhichOddArgCount",
                    "Which requires an even number of (test, value) argument pairs but received " <>
                      ToString[Length[callArgs]] <>
                      If[Length[callArgs] === 1, " argument", " arguments"] <> ".",
                    "Warning",
                    <|Source -> callSrc, ConfidenceLevel -> 0.95, "Argument" -> "Which"|>
                  ]},
                True,
                  (* Even arg count is valid structure; check each test arg for non-Boolean type.
                     Test positions are 1, 3, 5, ...
                     - Known non-boolean type (Integer, String, Real, etc.): high-confidence warning.
                     - Completely unknown return type (function not in builtins, not in PacletIndex,
                       or not inferable): lower-confidence warning. Which tests must be boolean, and
                       an arbitrary expression whose type cannot be verified is suspicious. *)
                  Flatten[Map[
                    Function[{i},
                      With[{testArg = callArgs[[i]]},
                        With[{sample =
                          With[{s = inferArgSampleValue[testArg]},
                            If[s === Missing["Unknown"] &&
                                 MatchQ[testArg, CallNode[LeafNode[Symbol, _String, _], _, _]],
                              inferCallNodeReturnSampleValue[testArg],
                              s
                            ]
                          ]
                        },
                          Which[
                            sample =!= Missing["Unknown"] && !BooleanQ[sample],
                              {InspectionObject[
                                "WhichNonBooleanTest",
                                "Test argument " <> ToString[i] <> " of Which is not a Boolean" <>
                                  " expression (got " <> ToString[Head[sample]] <> ").",
                                "Error",
                                <|Source -> Quiet[testArg[[3, Key[Source]]]],
                                  ConfidenceLevel -> 0.9, "Argument" -> "Which"|>
                              ]},
                            sample === Missing["Unknown"] &&
                              MatchQ[testArg, CallNode[LeafNode[Symbol, _String, _], _, _]],
                              {InspectionObject[
                                "WhichUnknownTest",
                                "Test argument " <> ToString[i] <> " of Which has unknown return type;" <>
                                  " Which tests should be Boolean expressions.",
                                "Warning",
                                <|Source -> Quiet[testArg[[3, Key[Source]]]],
                                  ConfidenceLevel -> 0.75, "Argument" -> "Which"|>
                              ]},
                            True, {}
                          ]
                        ]
                      ]
                    ],
                    Range[1, Length[callArgs], 2]
                  ]]
              ]]
            ];
            If[funcName === "Switch",
              Throw[Which[
                Length[callArgs] < 3,
                  {InspectionObject[
                    "SwitchInsufficientArgs",
                    "Switch requires at least 3 arguments: Switch[expr, pattern, value, \[Ellipsis]]" <>
                      " but received " <> ToString[Length[callArgs]] <>
                      If[Length[callArgs] === 1, " argument", " arguments"] <> ".",
                    "Warning",
                    <|Source -> callSrc, ConfidenceLevel -> 0.95, "Argument" -> "Switch"|>
                  ]},
                EvenQ[Length[callArgs]],
                  {InspectionObject[
                    "SwitchEvenArgCount",
                    "Switch[expr, pattern, value, \[Ellipsis]] requires an odd total argument count" <>
                      " (1 dispatch expression followed by pattern\[Dash]value pairs)" <>
                      " but received " <> ToString[Length[callArgs]] <> " arguments.",
                    "Warning",
                    <|Source -> callSrc, ConfidenceLevel -> 0.95, "Argument" -> "Switch"|>
                  ]},
                True,
                  {}
              ]]
            ];

            (*
            Build the definition list: prefer local (same-file) definitions so
            the check works even when $WorkspaceRootPath is None and the
            PacletIndex has not been populated.  If the function is not defined
            locally, fall back to the PacletIndex (cross-file / cross-paclet).
            *)
            localEntries = Lookup[localDefMap, funcName, {}];
            allDefs = If[Length[localEntries] > 0,
              localEntries,
              Module[{pacletDefs},
                pacletDefs = If[KeyExistsQ[$PacletIndex["Symbols"], funcName],
                  $PacletIndex["Symbols", funcName, "Definitions"],
                  {}
                ];
                If[Length[pacletDefs] > 0,
                  pacletDefs,
                  (* PacletIndex has no definitions (symbol only referenced, not defined).
                     Fall back to pre-generated builtin patterns for System` functions. *)
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
                ]
              ]
            ];

            (*
            Only check functions that have at least one typed parameter across
            their definitions.  Functions with only untyped catch-alls (x_) can
            accept anything so no mismatch is possible.
            *)
            If[!AnyTrue[allDefs,
              AnyTrue[Lookup[#, "InputPatterns", {}], (# =!= Blank[]) &] &],
              Throw[{}]
            ];

            (*
            Check arity: warn when no overload accepts the call\'s argument count.
            Variadic overloads (BlankSequence / BlankNullSequence / Optional) are
            exempt because their accepted arity is not fixed.
            Arity checking is only performed for locally-defined functions since
            PacletIndex and builtin entries lack variadic metadata.
            *)
            If[!AnyTrue[allDefs,
              Lookup[#, "Variadic", False] ||
              Length[Lookup[#, "InputPatterns", {}]] === Length[callArgs] &],
              If[Length[localEntries] > 0,
                Throw[{InspectionObject[
                  "DocCommentArityMismatch",
                  "\"" <> funcName <> "\" called with " <> ToString[Length[callArgs]] <>
                    If[Length[callArgs] === 1, " argument", " arguments"] <>
                    ", but no definition accepts this arity.",
                  "Error",
                  <|
                    Source -> callSrc,
                    ConfidenceLevel -> 0.85,
                    "Argument" -> funcName
                  |>
                ]}],
                Throw[{}]
              ]
            ];

            (*
            Pre-compute a representative sample value for every argument.
            *)
            argSamples = inferArgSampleValue /@ callArgs;

            (*
            Determine if at least one definition is compatible with the call.
            An overload is compatible when:
              - its InputPatterns length matches the number of arguments, AND
              - every argument satisfies the corresponding pattern string
                (None patterns always match - they are untyped or complex).
            *)
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

            If[!symptomatic, Throw[{}]];

            (*
            Find the first argument position rejected by ALL definitions
            so we can point the warning at the offending token.
            *)
            mismatchIdx = Catch[
              Do[
                Module[{s = argSamples[[i]]},
                  If[s =!= Missing["Unknown"] &&
                    AllTrue[allDefs,
                      Function[{d},
                        Module[{pats = Lookup[d, "InputPatterns", {}]},
                          Length[pats] >= i && !argMatchesPattern[s, pats[[i]]]
                        ]
                      ]
                    ],
                    Throw[i]
                  ]
                ],
                {i, Length[callArgs]}
              ];
              None
            ];

            badArg    = If[IntegerQ[mismatchIdx], callArgs[[mismatchIdx]], Null];
            badArgSrc = If[badArg =!= Null && AssociationQ[badArg[[3]]] &&
                            KeyExistsQ[badArg[[3]], Source],
              badArg[[3, Key[Source]]],
              callSrc
            ];

            Module[{expectedStr, gotStr},
              expectedStr = StringRiffle[
                DeleteDuplicates[
                  Cases[
                    Function[{d},
                      If[IntegerQ[mismatchIdx],
                        With[{pats = Lookup[d, "InputPatterns", {}]},
                          If[Length[pats] >= mismatchIdx &&
                             pats[[mismatchIdx]] =!= Blank[],
                            ToString[pats[[mismatchIdx]], InputForm],
                            Nothing
                          ]
                        ],
                        Nothing
                      ]
                    ] /@ allDefs,
                    _String
                  ]
                ],
                " | "
              ];
              gotStr = If[IntegerQ[mismatchIdx] && argSamples[[mismatchIdx]] =!= Missing["Unknown"],
                SymbolName[Head[argSamples[[mismatchIdx]]]],
                "unknown"
              ];
              {
                InspectionObject[
                  "DocCommentInputMismatch",
                  "Argument " <> If[IntegerQ[mismatchIdx], ToString[mismatchIdx], ""] <>
                    " of \"" <> funcName <> "\" does not match any declared input pattern." <>
                    If[StringLength[expectedStr] > 0,
                      " Expected " <> expectedStr <> ", got a " <> gotStr <> ".",
                      ""
                    ],
                  "Error",
                  <|
                    Source -> badArgSrc,
                    ConfidenceLevel -> 0.9,
                    "Argument" -> funcName
                  |>
                ]
              }
            ]
          ]
          ]  (* end Catch *)
        ] /@ callNodes,
        1
      ];

      workspaceLints = Join[workspaceLints, inputMismatchLints];

      (*
      Check function definition bodies against their DocComment Return: patterns.

      For each function definition in this file that declares  Return: <pattern>  in
      its doc-comment, infer the return type of the function body (RHS) and warn when
      the inferred type provably does not match the declared pattern.

      Inference handles:
        - Literal RHS values (Integer, Real, String, True/False, {}, <||>)
        - Direct function calls whose callee has a known ReturnPattern in the
          PacletIndex or $BuiltinPatterns
      For all other RHS forms (If/Which, complex expressions, unknown callees)
      Missing["Unknown"] is returned and no warning is emitted - no false positives.
      *)

      (* Convert a Blank-typed pattern expression to a representative concrete value.
         Returns Missing["Unknown"] for patterns that can't be sampled this way.
         NOTE: Uses === (structural equality) NOT MatchQ, because MatchQ[Blank[X], Blank[]]
         is always True (Blank[] matches anything) — SameQ checks the expression itself. *)
      patternToSampleValue = Function[{pat},
        Which[
          pat === Blank[],            Missing["Unknown"],
          pat === Blank[Integer],     0,
          pat === Blank[String],      "",
          pat === Blank[Real],        0.,
          pat === Blank[List],        {},
          pat === Blank[Association], <||>,
          True,                       Missing["Unknown"]
        ]
      ];

      (* Infer a representative sample value for the return type of a function body
         RHS node.  Returns a concrete WL value (usable with argMatchesPattern) or
         Missing["Unknown"] when the type cannot be statically determined. *)
      inferRHSSample = Function[{rhsNode},
        Which[
          MatchQ[rhsNode, LeafNode[Integer, _, _]],                     0,
          MatchQ[rhsNode, LeafNode[Real,    _, _]],                     0.,
          MatchQ[rhsNode, LeafNode[String,  _, _]],                     "",
          MatchQ[rhsNode, LeafNode[Symbol, "True" | "False", _]],       True,
          MatchQ[rhsNode, CallNode[LeafNode[Symbol, "List", _], {}, _]],        {},
          MatchQ[rhsNode, CallNode[LeafNode[Symbol, "Association", _], {}, _]], <||>,
          (* Direct function call: look up callee's declared ReturnPattern *)
          MatchQ[rhsNode, CallNode[LeafNode[Symbol, _, _], _, _]],
            Module[{callHead, calleeDefs, matchedRetPat},
              callHead = rhsNode[[1, 2]];
              (* PacletIndex first (user-defined / same-paclet functions) *)
              calleeDefs = Lookup[
                Lookup[$PacletIndex["Symbols"], callHead, <||>],
                "Definitions", {}
              ];
              (* Fall back to pre-generated builtin patterns for System` functions *)
              If[Length[calleeDefs] === 0 && KeyExistsQ[$BuiltinPatterns, callHead],
                calleeDefs = Map[
                  Function[{overload},
                    <|
                      "InputPatterns" -> Map[
                        Function[{s}, If[StringQ[s], Blank[Symbol[s]], Blank[]]],
                        overload[[1]]
                      ],
                      "DocComment" -> If[StringQ[overload[[2]]] &&
                          !StringMatchQ[overload[[2]], "_[" ~~ DigitCharacter.. ~~ "]" ~~ ___],
                        <|"ReturnPattern"       -> ToExpression[overload[[2]]],
                          "ReturnPatternString" -> overload[[2]]|>,
                        None
                      ]
                    |>
                  ],
                  $BuiltinPatterns[callHead]
                ]
              ];
              (* Pick first overload with a concrete return type:
                 1st pass: explicit DocComment Return: annotation (highest precedence)
                 2nd pass: automatically inferred InferredReturnPattern (fallback) *)
              matchedRetPat = Catch[
                Scan[
                  Function[{def},
                    With[{dc = Lookup[def, "DocComment", None]},
                      If[AssociationQ[dc] && !MatchQ[dc["ReturnPattern"], None | _Missing],
                        Throw[dc["ReturnPattern"]]
                      ]
                    ]
                  ],
                  calleeDefs
                ];
                Scan[
                  Function[{def},
                    With[{irp = Lookup[def, "InferredReturnPattern", None]},
                      If[!MatchQ[irp, None | _Missing], Throw[irp]]
                    ]
                  ],
                  calleeDefs
                ];
                None
              ];
              If[matchedRetPat === None,
                Missing["Unknown"],
                patternToSampleValue[matchedRetPat]
              ]
            ],
          True, Missing["Unknown"]
        ]
      ];

      (* Map: definition start line -> RHS AST node, for all Set/SetDelayed in this file *)
      defRHSByLine = Association[
        Cases[ast,
          CallNode[
            LeafNode[Symbol, "Set" | "SetDelayed", _],
            {_, rhs_},
            KeyValuePattern[Source -> {{defStartLine_Integer, _}, _}]
          ] :> defStartLine -> rhs,
          Infinity
        ]
      ];

      returnMismatchLints = Flatten[
        Function[{symName},
          Module[{defs},
            defs = GetSymbolDefinitions[symName];
            defs = Select[defs,
              #["uri"] === uri && #["kind"] === "function" &&
              AssociationQ[#["DocComment"]] &&
              !MatchQ[#["DocComment"]["ReturnPattern"], None | _Missing] &
            ];
            Flatten[
              Function[{def},
                Module[{defLine, declaredRetPat, rhsNode, inferredSample, sampleTypeStr},
                  defLine        = def["source"][[1, 1]];
                  declaredRetPat = def["DocComment"]["ReturnPattern"];
                  rhsNode        = Lookup[defRHSByLine, defLine, Missing["NoRHS"]];
                  (* Compute inferred sample; Missing if RHS not found or not inferrable *)
                  inferredSample = If[MissingQ[rhsNode],
                    Missing["Unknown"],
                    inferRHSSample[rhsNode]
                  ];
                  (* Emit warning only when we can determine the type and it mismatches *)
                  If[inferredSample === Missing["Unknown"] ||
                      argMatchesPattern[inferredSample, declaredRetPat],
                    {},
                    {
                      sampleTypeStr = Which[
                        BooleanQ[inferredSample],     "Boolean",
                        IntegerQ[inferredSample],     "Integer",
                        StringQ[inferredSample],      "String",
                        RealQ[inferredSample],        "Real",
                        ListQ[inferredSample],        "List",
                        AssociationQ[inferredSample], "Association",
                        True,                          ToString[Head[inferredSample]]
                      ];
                      InspectionObject[
                        "DocCommentReturnMismatch",
                        "Return type of \"" <> symName <>
                          "\" does not match declared Return: pattern. Expected " <>
                          ToString[declaredRetPat] <> ", but the body evaluates to a " <>
                          sampleTypeStr <> ".",
                        "Error",
                        <|
                          Source -> rhsNode[[3, Key[Source]]],
                          ConfidenceLevel -> 0.85,
                          "Argument" -> symName
                        |>
                      ]
                    }
                  ]
                ]
              ] /@ defs,
              1
            ]
          ]
        ] /@ GetPacletSymbols[],
        1
      ];

      (*
      DeclaredTypeMismatch: body return type does not match the DeclaredType annotation
      from an .ipwl file. DeclaredType is stored on "declaration" kind entries; we pair
      it with the "function" kind entries for the same symbol in the same file.
      *)
      Module[{declaredTypeLints},
        declaredTypeLints = Flatten[
          Function[{symName},
            Module[{allDefs, declEntry, declaredRetPat, funcDefs},
              allDefs = GetSymbolDefinitions[symName];
              (* Find the declaration entry with a non-parametric DeclaredType for this file *)
              declEntry = SelectFirst[allDefs,
                #["uri"] === uri && #["kind"] === "declaration" &&
                !MatchQ[Lookup[#, "DeclaredType", None], None | _Missing] &&
                (* Skip parametric — use Head check, not Blank[ParametricRef[_]] *)
                !(Head[Lookup[#, "DeclaredType", None]] === Blank &&
                  Length[Lookup[#, "DeclaredType", None]] === 1 &&
                  Head[Lookup[#, "DeclaredType", None][[1]]] === LSPServer`TypeWL`ParametricRef) &,
                None
              ];
              If[declEntry === None, Return[{}]];
              declaredRetPat = declEntry["DeclaredType"];

              (* Get the function (body) definitions for this symbol in this file *)
              funcDefs = Select[allDefs, #["uri"] === uri && #["kind"] === "function" &];

              Flatten[
                Function[{def},
                  Module[{defLine, rhsNode, inferredSample, sampleTypeStr},
                    defLine        = def["source"][[1, 1]];
                    rhsNode        = Lookup[defRHSByLine, defLine, Missing["NoRHS"]];
                    inferredSample = If[MissingQ[rhsNode],
                      Missing["Unknown"],
                      inferRHSSample[rhsNode]
                    ];
                    If[inferredSample === Missing["Unknown"] ||
                        argMatchesPattern[inferredSample, declaredRetPat],
                      {},
                      {
                        sampleTypeStr = Which[
                          BooleanQ[inferredSample],      "Boolean",
                          IntegerQ[inferredSample],      "Integer",
                          StringQ[inferredSample],       "String",
                          MatchQ[inferredSample, _Real], "Real",
                          ListQ[inferredSample],         "List",
                          AssociationQ[inferredSample],  "Association",
                          True,                           ToString[Head[inferredSample]]
                        ];
                        InspectionObject[
                          "DeclaredTypeMismatch",
                          "Return type of \"" <> symName <>
                            "\" does not match declared type. Expected " <>
                            Quiet[ToString[declaredRetPat, InputForm], {ToString::shdw}] <>
                            ", but the body evaluates to a " <> sampleTypeStr <> ".",
                          "Error",
                          <|
                            Source -> rhsNode[[3, Key[Source]]],
                            ConfidenceLevel -> 0.85,
                            "Argument" -> symName
                          |>
                        ]
                      }
                    ]
                  ]
                ] /@ funcDefs,
                1
              ]
            ]
          ] /@ GetPacletSymbols[],
          1
        ];
        workspaceLints = Join[workspaceLints, declaredTypeLints]
      ];

      workspaceLints = Join[workspaceLints, returnMismatchLints]
    ]
  ];

  (*
  IPWLSyntaxError / IPWLUnresolvedSymbol: file-level diagnostics from the
  pre-processor pass. Only for .ipwl files.
  *)
  If[TrueQ[Lookup[Lookup[$PacletIndex["Files"], uri, <||>], "IsIPWL", False]],
    Module[{ipwlSrc, preprocessResult, annotations, ipwlLints},
      ipwlSrc = Quiet[entry["Text"]];
      If[StringQ[ipwlSrc],
        preprocessResult = LSPServer`TypeWL`PreprocessIPWL[ipwlSrc];
        annotations      = preprocessResult[[2]];

        (* IPWLSyntaxError entries from the pre-processor *)
        ipwlLints = Map[
          Function[{ann},
            InspectionObject[
              "IPWLSyntaxError",
              ann["message"],
              "Error",
              <| Source -> {{ann["line"], 1}, {ann["line"], 1}},
                 ConfidenceLevel -> 1.0 |>
            ]
          ],
          Select[annotations, #["kind"] === "IPWLSyntaxError" &]
        ];

        (* IPWLUnresolvedSymbol: declared but not defined in any indexed .wl file *)
        ipwlLints = Join[ipwlLints, Flatten[Map[
          Function[{ann},
            If[ann["kind"] =!= "DeclaredType", Return[{}]];
            Module[{sym = ann["symbol"], allDefs},
              allDefs = Lookup[
                Lookup[LSPServer`PacletIndex`$PacletIndex["Symbols"], sym, <||>],
                "Definitions", {}
              ];
              (* Has any non-declaration definition in a non-IPWL file? *)
              If[NoneTrue[allDefs,
                  !TrueQ[Lookup[Lookup[LSPServer`PacletIndex`$PacletIndex["Files"], #["uri"], <||>], "IsIPWL", False]] &&
                  #["kind"] === "function" &],
                {InspectionObject[
                  "IPWLUnresolvedSymbol",
                  "Symbol \"" <> sym <> "\" is declared in .ipwl but not defined in any .wl file.",
                  "Warning",
                  <| Source -> {{ann["line"], 1}, {ann["line"], 1}},
                     ConfidenceLevel -> 0.7 |>
                ]},
                {}
              ]
            ]
          ],
          Select[annotations, #["kind"] === "DeclaredType" &]
        ], 1]];

        workspaceLints = Join[workspaceLints, ipwlLints]
      ]
    ]
  ];

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

  log[1, "textDocument/clearDiagnostics: enter"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],

    log[2, "stale"];

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

  entry["IgnoreData"] =.;

  (* Also clear from the global ignore data map *)
  ClearIgnoreData[uri];

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/clearDiagnostics: exit"];

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/publishDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, lints, lintsWithConfidence, cstLints, aggLints, astLints,
  scopingLints, workspaceLints, diagnostics, ignoreData, beforeIgnoreCount},

  log[1, "textDocument/publishDiagnostics: enter"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],

    log[2, "stale"];

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
    log["lints before filtering: ", Length[lints]]
  ];


  lintsWithConfidence = Cases[lints, InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> _]]];

  lints = Cases[lintsWithConfidence, InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> _?(GreaterEqualThan[$ConfidenceLevel])]]];

  (*
  Filter out lints that are ignored by wl-disable comments
  *)
  ignoreData = Lookup[entry, "IgnoreData", GetIgnoreData[uri]];
  beforeIgnoreCount = Length[lints];
  lints = Select[lints, !ShouldIgnoreDiagnostic[#, ignoreData]&];

  If[$Debug2,
    log["lints after ignore filtering: ", Length[lints], " (", beforeIgnoreCount - Length[lints], " ignored)"]
  ];

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
    log["lints final: ", #["Tag"]& /@ lints]
  ];

  diagnostics = lintToDiagnostics /@ lints;

  diagnostics = Flatten[diagnostics];

  res = {<| "jsonrpc" -> "2.0",
      "method" -> "textDocument/publishDiagnostics",
      "params" -> <| "uri" -> uri,
                      "diagnostics" -> diagnostics |> |>};

  log[1, "textDocument/publishDiagnostics: exit"];

  res
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
