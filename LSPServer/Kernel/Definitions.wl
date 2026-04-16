BeginPackage["LSPServer`Definitions`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`PacletIndex`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


validDefinitionSourceQ[src_] :=
  MatchQ[src, {{_Integer?Positive, _Integer?Positive}, {_Integer?Positive, _Integer?Positive}}]


definitionLocation[uri_, src_] /; StringQ[uri] && validDefinitionSourceQ[src] :=
Module[{adjustedSource},
  adjustedSource = Map[Max[#, 0] &, src - 1, {2}];
  <|
    "uri" -> uri,
    "range" -> <|
      "start" -> <| "line" -> adjustedSource[[1, 1]], "character" -> adjustedSource[[1, 2]] |>,
      "end" -> <| "line" -> adjustedSource[[2, 1]], "character" -> adjustedSource[[2, 2]] |>
    |>
  |>
]


definitionLocation[uri_, src_] := Nothing


definitionLocation[def_Association] :=
  definitionLocation[Lookup[def, "uri", None], Lookup[def, "source", None]]


definitionLocation[_] := Nothing


(*
Find the definition location for an external symbol (from a loaded paclet).
Returns a list of location associations, or {} if not found.
    pacletDefinitions = LSPServer`PacletIndex`GetVisibleSymbolDefinitions[uri, symbolName, preferredContext];
This searches:
1. The paclet's Kernel directory for the symbol definition
2. Uses the symbol's context to find the right paclet
*)
findExternalSymbolDefinition[symbolName_String] :=
Module[{bareSymbol, symContext, pacletName, pacletObj, pacletDir, kernelFiles,
        locations, text, cst, ast, defs, uri},

  (*
  Get the bare symbol name and its context
  *)
  If[StringContainsQ[symbolName, "`"],
    bareSymbol = Last[StringSplit[symbolName, "`"]];
    symContext = StringJoin[Riffle[Most[StringSplit[symbolName, "`"]], "`"]] <> "`",
    (* Try to get context from the symbol itself *)
    bareSymbol = symbolName;
    symContext = Quiet[
      Check[Context[symbolName], None, {Context::notfound}],
      {Context::notfound}
    ]
  ];

  If[!StringQ[symContext] || symContext === "Global`" || symContext === "System`",
    Return[{}]
  ];

  (*
  Extract paclet name from context (first part before `)
  *)
  pacletName = First[StringSplit[symContext, "`"], None];

  If[pacletName === None,
    Return[{}]
  ];

  (*
  Find the paclet location
  *)
  pacletObj = Quiet[PacletObject[pacletName]];

  If[!MatchQ[pacletObj, _PacletObject] || FailureQ[pacletObj],
    Return[{}]
  ];

  pacletDir = pacletObj["Location"];

  If[!StringQ[pacletDir] || !DirectoryQ[pacletDir],
    Return[{}]
  ];

  If[$Debug2,
    log["findExternalSymbolDefinition: searching in ", pacletDir, " for ", bareSymbol]
  ];

  (*
  Search for .wl and .m files in the paclet's Kernel directory
  *)
  kernelFiles = Join[
    FileNames["*.wl", FileNameJoin[{pacletDir, "Kernel"}], Infinity],
    FileNames["*.m", FileNameJoin[{pacletDir, "Kernel"}], Infinity]
  ];

  (*
  Also check the root of the paclet for .wl/.m files
  *)
  kernelFiles = Join[kernelFiles,
    FileNames["*.wl", pacletDir],
    FileNames["*.m", pacletDir]
  ];

  If[Length[kernelFiles] == 0,
    Return[{}]
  ];

  locations = {};

  (*
  Search each file for the symbol definition
  *)
  Do[
    text = Quiet[Import[file, "Text"]];

    If[!StringQ[text],
      Continue[]
    ];

    (*
    Quick check - does the file contain the symbol name?
    *)
    If[!StringContainsQ[text, bareSymbol],
      Continue[]
    ];

    (*
    Parse and search for definitions
    *)
    cst = Quiet[CodeConcreteParse[text]];

    If[FailureQ[cst],
      Continue[]
    ];

    ast = Quiet[CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[cst]]];

    If[FailureQ[ast],
      Continue[]
    ];

    (*
    Find definitions of this symbol
    *)
    defs = Cases[ast,
      node_[_, _, KeyValuePattern["Definitions" -> defList_List, Source -> src_]] /;
        AnyTrue[defList, MatchQ[#, LeafNode[Symbol, s_String, _] /;
          StringMatchQ[s, (__ ~~ "`" ~~ bareSymbol) | bareSymbol]] &] :>
        src,
      Infinity
    ];

    (*
    Also look for usage message definitions (symbol::usage = ...)
    *)
    defs = Join[defs, Cases[ast,
      CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _],
        {CallNode[LeafNode[Symbol, "MessageName", _],
          {LeafNode[Symbol, s_String, _], LeafNode[String, "\"usage\"", _], ___}, _], _},
        KeyValuePattern[Source -> src_]] /;
        StringMatchQ[s, (__ ~~ "`" ~~ bareSymbol) | bareSymbol] :> src,
      Infinity
    ]];

    If[Length[defs] > 0,
      uri = "file://" <> file;

      locations = Join[
        locations,
        DeleteCases[definitionLocation[uri, #] & /@ defs, Nothing]
      ]
    ],
    {file, kernelFiles}
  ];

  locations
]


expandContent[content:KeyValuePattern["method" -> "textDocument/definition"], pos_] :=
Catch[
Module[{params, id, doc, uri, res},

  log[1, "textDocument/definition: enter expand"];

  id = content["id"];
  params = content["params"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "canceled"];

    Throw[{<| "method" -> "textDocument/definitionFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],

    log[2, "stale"];

    Throw[{<| "method" -> "textDocument/definitionFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  res = <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/aggregateParse",
    "textDocument/abstractParse",
    "textDocument/definitionFencepost"
  };

  log[1, "textDocument/definition: exit"];

  res
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/definitionFencepost"]] :=
Catch[
Module[{id, params, doc, uri, ast, position, locations, line, char, cases, sym, namePat, srcs, entry,
  symbolName, rawSymbolName, preferredContext, pacletDefinitions, localLocations, pacletLocations},

  log[1, "textDocument/definitionFencepost: enter"];

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "canceled"];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[Lookup[content, "stale", False] || isStale[$ContentQueue, uri],

    log[2, "stale"];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  position = params["position"];
  line = position["line"];
  char = position["character"];

  (*
  convert from 0-based to 1-based
  *)
  line+=1;
  char+=1;

  entry = Lookup[$OpenFilesMap, uri, Null];

  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  ast = Lookup[entry, "AST", Null];

  If[ast === Null || MissingQ[ast] || FailureQ[ast],
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  (*
  Find the name of the symbol at the position
  *)
  cases = Cases[ast, LeafNode[Symbol, _, KeyValuePattern[Source -> src_ /; SourceMemberQ[src, {line, char}]]], Infinity];

  If[cases == {},
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}]
  ];

  sym = cases[[1]];

  rawSymbolName = sym["String"];
  preferredContext = If[StringContainsQ[rawSymbolName, "`"],
    StringJoin[Riffle[Most[StringSplit[rawSymbolName, "`"]], "`"]] <> "`",
    None
  ];
  symbolName = rawSymbolName;

  (*
  Remove contexts for pattern matching
  *)
  namePat = StringReplace[symbolName, __ ~~ "`" -> ""];

  (*
  Definitions may be specified with or without context
  *)
  namePat = (__ ~~ "`" ~~ namePat) | namePat;

  (*
  First, look for definitions in the current file (local definitions)
  *)
  cases = Flatten[Cases[ast, _[_, _, KeyValuePattern["Definitions" -> defs_ /; AnyTrue[defs, StringMatchQ[#[[2]], namePat]&]]], Infinity]];

  srcs = #[[3, Key[Source]]]& /@ cases;

  localLocations = DeleteCases[definitionLocation[uri, #] & /@ srcs, Nothing];

  (*
  Then, look for definitions in the paclet index (cross-file definitions)
  This includes definitions from other files in the workspace
  *)
  pacletLocations = {};

  If[StringQ[$WorkspaceRootPath],
    (*
    Get the bare symbol name (without context)
    *)
    symbolName = StringReplace[symbolName, __ ~~ "`" -> ""];

    (*
    Search paclet index for definitions
    *)
    pacletDefinitions = LSPServer`PacletIndex`GetVisibleSymbolDefinitions[uri, symbolName, preferredContext];

    If[$Debug2,
      log["paclet definitions for ", symbolName, ": ", Length[pacletDefinitions]]
    ];

    (*
    Convert paclet index definitions to LSP locations
    Exclude definitions from the current file (already in localLocations)
    *)
    pacletLocations = DeleteCases[
      definitionLocation /@ Select[pacletDefinitions, Lookup[#, "uri", None] =!= uri &],
      Nothing
    ]
  ];

  (*
  Combine local and paclet-wide locations, with local definitions first
  *)
  locations = Join[localLocations, pacletLocations];

  (*
  If no definitions found in workspace, search external paclets
  *)
  If[Length[locations] == 0,
    Module[{externalLocations},
      If[$Debug2,
        log["searching external paclets for: ", symbolName]
      ];

      externalLocations = findExternalSymbolDefinition[symbolName];

      If[Length[externalLocations] > 0,
        If[$Debug2,
          log["found ", Length[externalLocations], " external definitions"]
        ];
        locations = externalLocations
      ]
    ]
  ];

  (*
  Remove duplicates (same location from different sources)
  *)
  locations = DeleteDuplicatesBy[locations, {#["uri"], #["range"]["start"]["line"], #["range"]["start"]["character"]}&];

  If[$Debug2,
    log["definition locations: ", Length[locations]]
  ];

  log[1, "textDocument/definitionFencepost: exit"];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> locations |>}
]]


End[]

EndPackage[]
