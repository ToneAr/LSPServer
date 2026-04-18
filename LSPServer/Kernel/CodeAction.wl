BeginPackage["LSPServer`CodeAction`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["LSPServer`IgnorePatterns`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


diagnosticRuleTag[diagnostic_Association] :=
Module[{code, parts},
  code = Lookup[diagnostic, "code", Missing["NotAvailable"]];

  If[!StringQ[code],
    Return[None]
  ];

  parts = StringSplit[code, "\[VeryThinSpace]\:25bb\[VeryThinSpace]", 2];
  First[parts]
]


diagnosticSourceRange[diagnostic_Association] :=
Module[{range, start, end},
  range = Lookup[diagnostic, "range", Missing["NotAvailable"]];

  If[!AssociationQ[range],
    Return[Missing["NotAvailable"]]
  ];

  start = Lookup[range, "start", Missing["NotAvailable"]];
  end = Lookup[range, "end", Missing["NotAvailable"]];

  If[!AssociationQ[start] || !AssociationQ[end],
    Return[Missing["NotAvailable"]]
  ];

  {
    {Lookup[start, "line", -1] + 1, Lookup[start, "character", -1] + 1},
    {Lookup[end, "line", -1] + 1, Lookup[end, "character", -1] + 1}
  }
]


diagnosticSupportsIgnoreQuickFixQ[diagnostic_Association] :=
Module[{tagStr},
  tagStr = diagnosticRuleTag[diagnostic];

  StringQ[tagStr] &&
    tagStr =!= "" &&
    Lookup[diagnostic, "source", Missing["NotAvailable"]] === "wolfram lint"
]


candidateIgnoreQuickFixDiagnostics[params_, lints_List, ignoreData_] :=
Module[{contextDiagnostics, fallbackDiagnostics},
  contextDiagnostics = Replace[
    Lookup[Lookup[params, "context", <||>], "diagnostics", {}],
    Except[_List] -> {}
  ];

  If[contextDiagnostics === {},
    fallbackDiagnostics = Flatten[
      lintToDiagnostics /@ Select[lints, !ShouldIgnoreDiagnostic[#, ignoreData] &],
      1
    ];
    contextDiagnostics = fallbackDiagnostics
  ];

  DeleteDuplicatesBy[
    Select[contextDiagnostics, diagnosticSupportsIgnoreQuickFixQ],
    {
      Lookup[#, "code", Missing["NotAvailable"]],
      Lookup[#, "range", Missing["NotAvailable"]],
      Lookup[#, "source", Missing["NotAvailable"]]
    } &
  ]
]


expandContent[content:KeyValuePattern["method" -> "textDocument/codeAction"], pos_] :=
Catch[
Module[{params, id, doc, uri, res},


  log[1, "textDocument/codeAction: enter expand"];


  id = content["id"];
  params = content["params"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "canceled"];

    Throw[{<| "method" -> "textDocument/codeActionFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],

    log[2, "stale"];

    Throw[{<| "method" -> "textDocument/codeActionFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  res = <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/suppressedRegions",
    "textDocument/parseIgnoreComments",
    "textDocument/runConcreteDiagnostics",
    "textDocument/aggregateParse",
    "textDocument/runAggregateDiagnostics",
    "textDocument/abstractParse",
    "textDocument/runAbstractDiagnostics",
    "textDocument/codeActionFencepost"
  };

  log[1, "textDocument/codeAction: exit"];

  res
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/codeActionFencepost"]] :=
Catch[
Module[{id, params, doc, uri, actions, range, lints, lspAction, lspActions, edit, diagnostics,
  command, label, actionData, actionSrc, replacementNode, insertionNode, replacementText, lintsWithConfidence,
  shadowing, insertionText, cursor, entry, text, cst, agg, ast, cstLints, aggLints, astLints,
  scopingLints, workspaceLints, ignoreData},


  log[1, "textDocument/codeActionFencepost: enter"];


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

  range = params["range"];

  cursor = { { range["start"]["line"], range["start"]["character"] },
             { range["end"]["line"], range["end"]["character"] } };

  (* convert from 0-based to 1-based *)
  cursor+=1;

  log[2, "cursor: ", ToString[cursor]];

  entry = Lookup[$OpenFilesMap, uri, Null];

  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  cstLints = Replace[Lookup[entry, "CSTLints", {}], Except[_List] -> {}];
  aggLints = Replace[Lookup[entry, "AggLints", {}], Except[_List] -> {}];
  astLints = Replace[Lookup[entry, "ASTLints", {}], Except[_List] -> {}];
  scopingLints = Replace[Lookup[entry, "ScopingLints", {}], Except[_List] -> {}];
  workspaceLints = Replace[Lookup[entry, "WorkspaceLints", {}], Except[_List] -> {}];

  lints = cstLints ~Join~ aggLints ~Join~ astLints ~Join~ scopingLints ~Join~ workspaceLints;

  log[2, "lints: ", stringLineTake[StringTake[ToString[lints, InputForm], UpTo[1000]], UpTo[20]]];
  log[2, "...\n"];

  lintsWithConfidence = Cases[lints, InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> _]]];

  lints = Cases[lintsWithConfidence, InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> _?(GreaterEqualThan[$ConfidenceLevel])]]];


  (*

  Disable shadow filtering for now

  Below is quadratic time

  shadowing = Select[lints, Function[lint, AnyTrue[lints, shadows[lint, #]&]]];

  If[$Debug2,
   Write[$Messages, "shadowing: " //OutputForm, ToString[shadowing, InputForm] //OutputForm];
  ];

  lints = Complement[lints, shadowing];
  *)

  (*
  Make sure to sort lints before taking

  Sort by severity, then sort by Source

  severityToInteger maps "Remark" -> 1 and "Fatal" -> 4, so make sure to negate that
  *)
  lints = SortBy[lints, {-severityToInteger[#[[3]]]&, #[[4, Key[Source]]]&}];

  lints = Take[lints, UpTo[CodeInspector`Summarize`$DefaultLintLimit]];


  lspActions = {};

  Do[

    diagnostics = lintToDiagnostics[lint];

    log[2, "diagnostics (up to 20): ", ToString[Take[diagnostics, UpTo[20]]]];

    (*
    Need to filter the actions that match the cursor
    *)
    actions = Cases[lint, CodeAction[_, _, _], Infinity];

    log[2, "actions (up to 20): ", ToString[Take[actions, UpTo[20]]]];

    (*
    Need to filter the actions that match the cursor
    *)
    actions = Cases[actions, CodeAction[_, _, KeyValuePattern[Source -> src_ /; SourceMemberIntersectingQ[src, cursor]]]];

    log[2, "actions (up to 20): ", ToString[Take[actions, UpTo[20]]]];

    Do[

      label = action[[1]];

      label = plainify[label];

      command = action[[2]];
      actionData = action[[3]];

      actionSrc = actionData[Source];

      Switch[command,

        InsertNode,

        insertionNode = actionData["InsertionNode"];

        log[2, "insertionNode: ", ToString[insertionNode]];

        (*
        For inserting, don't use the [start, end) range, only use [start, start)
        *)
        edit = (<| "changes"-> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                            "end" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |> |>,
                                              "newText" -> ToSourceCharacterString[insertionNode] |> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <| "title"  -> label,
                       "kind"  -> "quickfix",
                       "edit"  -> edit,
                       "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction];

        ,

        InsertText,

        insertionText = actionData["InsertionText"];

        log[2, "insertionText: ", ToString[insertionText]];

        (*
        For inserting, don't use the [start, end) range, only use [start, start)
        *)
        edit = (<| "changes" -> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                               "end" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |> |>,
                                               "newText" -> insertionText|> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <| "title" -> label,
                       "kind" -> "quickfix",
                       "edit" -> edit,
                       "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction];

        ,

        DeleteNode,

        edit = (<| "changes"-> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                            "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>,
                                              "newText" -> "" |> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <| "title" -> label,
                       "kind" -> "quickfix",
                       "edit" -> edit,
                       "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction]

        ,

        ReplaceNode,

        replacementNode = actionData["ReplacementNode"];

        log[2, "replacementNode: ", ToString[replacementNode]];

        edit = (<| "changes"-> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                            "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>,
                                              "newText" -> ToSourceCharacterString[replacementNode] |> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <|  "title" -> label,
                        "kind" -> "quickfix",
                        "edit" -> edit,
                        "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction]

        ,

        ReplaceText,

        replacementText = actionData["ReplacementText"];

        log[2, "replacementText: ", ToString[replacementText]];

        edit = (<| "changes"-> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                            "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>,
                                              "newText" -> replacementText |> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <| "title" -> label,
                       "kind" -> "quickfix",
                       "edit" -> edit,
                       "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction]

        ,

        DeleteText,

        edit = (<| "changes"-> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                            "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>,
                                              "newText" -> ""|> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <| "title" -> label,
                       "kind" -> "quickfix",
                       "edit" -> edit,
                       "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction]

        ,

        _,

        log[2, "UNSUPPORTED COMMAND: ", command];

      ]

      ,
      {action, actions}
    ]

    ,
    {lint, lints}
  ];

  (*
  Add "Disable rule" quick fixes for each lint that intersects the cursor.
  These offer to insert wl-disable-line or wl-disable-next-line comments.
  *)
  text = Lookup[entry, "Text", ""];
  ignoreData = Lookup[entry, "IgnoreData", GetIgnoreData[uri]];

  Module[{textLines, diagnostic, diagnosticSrc, diagnosticLine0,
          disableLineEdit, disableNextLineEdit, disableLineText, disableNextLineText,
          lineEndChar, tagStr, ignoreDiagnostics},

    textLines = StringSplit[text, {"\r\n", "\n", "\r"}, All];

    ignoreDiagnostics = candidateIgnoreQuickFixDiagnostics[params, lints, ignoreData];

    Do[
      tagStr = diagnosticRuleTag[diagnostic];
      diagnosticSrc = diagnosticSourceRange[diagnostic];

      If[!MatchQ[diagnosticSrc, {{_Integer, _Integer}, {_Integer, _Integer}}],
        Continue[]
      ];

      (* Check if this lint intersects the cursor (1-based) *)
      If[!SourceMemberIntersectingQ[diagnosticSrc, cursor],
        Continue[]
      ];

      diagnosticLine0 = diagnosticSrc[[1, 1]] - 1;

      If[diagnosticLine0 < 0,
        Continue[]
      ];

      (* --- Action 1: Disable for this line --- *)
      (* Insert " (* wl-disable-line RuleName *)" at end of the line *)
      If[diagnosticLine0 < Length[textLines],
        lineEndChar = StringLength[textLines[[diagnosticLine0 + 1]]];
        disableLineText = " (* wl-disable-line " <> tagStr <> " *)";

        disableLineEdit = <| "changes" -> <| uri -> {
          <| "range" -> <| "start" -> <| "line" -> diagnosticLine0, "character" -> lineEndChar |>,
                           "end" -> <| "line" -> diagnosticLine0, "character" -> lineEndChar |> |>,
             "newText" -> disableLineText |>
        } |> |>;

        AppendTo[lspActions, <|
          "title" -> "Disable \"" <> tagStr <> "\" for this line",
          "kind" -> "quickfix",
          "edit" -> disableLineEdit,
          "diagnostics" -> {diagnostic}
        |>]
      ];

      (* --- Action 2: Disable for next line --- *)
      (* Insert "(* wl-disable-next-line RuleName *)\n" before the diagnostic's line *)
      disableNextLineText = "(* wl-disable-next-line " <> tagStr <> " *)\n";

      disableNextLineEdit = <| "changes" -> <| uri -> {
        <| "range" -> <| "start" -> <| "line" -> diagnosticLine0, "character" -> 0 |>,
                         "end" -> <| "line" -> diagnosticLine0, "character" -> 0 |> |>,
           "newText" -> disableNextLineText |>
      } |> |>;

      AppendTo[lspActions, <|
        "title" -> "Disable \"" <> tagStr <> "\" for next line",
        "kind" -> "quickfix",
        "edit" -> disableNextLineEdit,
        "diagnostics" -> {diagnostic}
      |>],

      {diagnostic, ignoreDiagnostics}
    ]
  ];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> lspActions |>}
]]

End[]

EndPackage[]
