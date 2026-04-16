BeginPackage["LSPServer`References`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`PacletIndex`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


expandContent[content:KeyValuePattern["method" -> "textDocument/references"], pos_] :=
Catch[
Module[{params, id, doc, uri, res},

  log[1, "textDocument/references: enter expand"];

  id = content["id"];
  params = content["params"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "canceled"];

    Throw[{<| "method" -> "textDocument/referencesFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],

    log[2, "stale"];

    Throw[{<| "method" -> "textDocument/referencesFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  res = <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/referencesFencepost"
  };

  log[1, "textDocument/references: exit"];

  res
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/referencesFencepost"]] :=
Catch[
Module[{id, params, doc, uri, cst, pos, line, char, cases, sym, name, srcs, entry, locations,
  localLocations, pacletReferences, pacletLocations, includeDeclaration, context},

  log[1, "textDocument/referencesFencepost: enter"];

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "$CancelMap: ", $CancelMap];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[Lookup[content, "stale", False] || isStale[$ContentQueue, uri],

    log[2, "stale"];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  pos = params["position"];
  line = pos["line"];
  char = pos["character"];

  (*
  Check if we should include the declaration
  *)
  context = Lookup[params, "context", <||>];
  includeDeclaration = Lookup[context, "includeDeclaration", True];

  (*
  convert from 0-based to 1-based
  *)
  line+=1;
  char+=1;

  entry = Lookup[$OpenFilesMap, uri, Null];

  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  cst = Lookup[entry, "CST", Null];

  If[cst === Null || MissingQ[cst],
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  If[FailureQ[cst],
    Throw[cst]
  ];

  (*
  Find the name of the symbol at the position
  *)
  cases = Cases[cst, LeafNode[Symbol, _, KeyValuePattern[Source -> src_ /; SourceMemberQ[src, {line, char}]]], Infinity];

  If[cases == {},
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}]
  ];

  sym = cases[[1]];

  name = sym["String"];

  (*
  Remove context prefix for matching
  *)
  name = StringReplace[name, __ ~~ "`" -> ""];

  (*
  Find references in current file
  *)
  cases = Cases[cst, LeafNode[Symbol, n_ /; StringReplace[n, __ ~~ "`" -> ""] === name, _], Infinity];

  srcs = #[[3, Key[Source]]]& /@ cases;

  localLocations =
    Function[{src}, <|
      "uri" -> uri,
      "range" -> <|
        "start" -> <| "line" -> #[[1, 1]],"character" -> #[[1, 2]] |>,
        "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |>
      |>
    |>&[Map[Max[#, 0]&, src-1, {2}]]] /@ srcs;

  log[1, "textDocument/referencesFencepost: exit"];

  (*
  Find references across the paclet (workspace)
  *)
  pacletLocations = {};

  If[StringQ[$WorkspaceRootPath],
    (*
    Get references from paclet index
    *)
    pacletReferences = GetSymbolReferences[name];

    If[$Debug2,
      log["paclet references for ", name, ": ", Length[pacletReferences]]
    ];

    (*
    Convert to LSP locations, excluding current file (already handled above)
    *)
    pacletLocations = Table[
      <|
        "uri" -> ref["uri"],
        "range" -> <|
          "start" -> <| "line" -> ref["source"][[1, 1]] - 1, "character" -> ref["source"][[1, 2]] - 1 |>,
          "end" -> <| "line" -> ref["source"][[2, 1]] - 1, "character" -> ref["source"][[2, 2]] - 1 |>
        |>
      |>,
      {ref, Select[pacletReferences, #["uri"] =!= uri &]}
    ];

    (*
    Optionally include definitions as references
    *)
    If[includeDeclaration,
      Module[{pacletDefs, defLocations},
        pacletDefs = GetSymbolDefinitions[name];
        defLocations = Table[
          <|
            "uri" -> def["uri"],
            "range" -> <|
              "start" -> <| "line" -> def["source"][[1, 1]] - 1, "character" -> def["source"][[1, 2]] - 1 |>,
              "end" -> <| "line" -> def["source"][[2, 1]] - 1, "character" -> def["source"][[2, 2]] - 1 |>
            |>
          |>,
          {def, Select[pacletDefs, #["uri"] =!= uri &]}
        ];
        pacletLocations = Join[pacletLocations, defLocations]
      ]
    ]
  ];

  (*
  Combine local and paclet-wide references
  *)
  locations = Join[localLocations, pacletLocations];

  (*
  Remove duplicates
  *)
  locations = DeleteDuplicatesBy[locations, {#["uri"], #["range"]["start"]["line"], #["range"]["start"]["character"]}&];

  If[$Debug2,
    log["total references: ", Length[locations]]
  ];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> locations |>}
]]

End[]

EndPackage[]
