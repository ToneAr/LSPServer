BeginPackage["LSPServer`CodeAction`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["LSPServer`IgnorePatterns`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


expandContent[content:KeyValuePattern["method" -> "textDocument/codeAction"], pos_] :=
Catch[
Module[{params, id, doc, uri},

  If[$Debug2,
    log["textDocument/codeAction: enter expand"]
  ];

  id = content["id"];
  params = content["params"];
  
  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    If[$Debug2,
      log["canceled"]
    ];
    
    Throw[{<| "method" -> "textDocument/codeActionFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    If[$Debug2,
      log["stale"]
    ];

    Throw[{<| "method" -> "textDocument/codeActionFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/suppressedRegions",
    "textDocument/parseIgnoreComments",
    "textDocument/runConcreteDiagnostics",
    "textDocument/aggregateParse",
    "textDocument/runAggregateDiagnostics",
    "textDocument/abstractParse",
    "textDocument/runAbstractDiagnostics",
    "textDocument/codeActionFencepost"
  }
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/codeActionFencepost"]] :=
Catch[
Module[{id, params, doc, uri, actions, range, lints, lspAction, lspActions, edit, diagnostics,
  command, label, actionData, actionSrc, replacementNode, insertionNode, replacementText, lintsWithConfidence,
  shadowing, insertionText, cursor, entry, text, cst, agg, ast, cstLints, aggLints, astLints},
  
  If[$Debug2,
    log["textDocument/codeActionFencepost: enter"]
  ];

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    If[$Debug2,
      log["canceled"]
    ];
    
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];
  
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[Lookup[content, "stale", False] || isStale[$ContentQueue, uri],
    
    If[$Debug2,
      log["stale"]
    ];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  range = params["range"];

  cursor = { { range["start"]["line"], range["start"]["character"] },
             { range["end"]["line"], range["end"]["character"] } };

  (* convert from 0-based to 1-based *)
  cursor+=1;

  If[$Debug2,
    log["cursor: ", ToString[cursor]]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];
  
  cstLints = entry["CSTLints"];

  (*
  Might get something like FileTooLarge
  *)
  If[FailureQ[cstLints],
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}]
  ];
  
  aggLints = entry["AggLints"];

  astLints = entry["ASTLints"];

  lints = cstLints ~Join~ aggLints ~Join~ astLints;

  If[$Debug2,
    log["lints: ", stringLineTake[StringTake[ToString[lints, InputForm], UpTo[1000]], UpTo[20]]];
    log["...\n"]
  ];

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

    If[$Debug2,
      log["diagnostics (up to 20): ", ToString[Take[diagnostics, UpTo[20]]]]
    ];

    (*
    Need to filter the actions that match the cursor
    *)
    actions = Cases[lint, CodeAction[_, _, _], Infinity];

    If[$Debug2,
      log["actions (up to 20): ", ToString[Take[actions, UpTo[20]]]]
    ];

    (*
    Need to filter the actions that match the cursor
    *)
    actions = Cases[actions, CodeAction[_, _, KeyValuePattern[Source -> src_ /; SourceMemberIntersectingQ[src, cursor]]]];

    If[$Debug2,
      log["actions (up to 20): ", ToString[Take[actions, UpTo[20]]]]
    ];

    Do[

      label = action[[1]];

      label = plainify[label];

      command = action[[2]];
      actionData = action[[3]];

      actionSrc = actionData[Source];

      Switch[command,

        InsertNode,

        insertionNode = actionData["InsertionNode"];

        If[$Debug2,
          log["insertionNode: ", ToString[insertionNode]]
        ];

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

        If[$Debug2,
          log["insertionText: ", ToString[insertionText]];
        ];

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

        If[$Debug2,
          log["replacementNode: ", ToString[replacementNode]]
        ];

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

        If[$Debug2,
          log["replacementText: ", ToString[replacementText]]
        ];

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

        If[$Debug,
          log["UNSUPPORTED COMMAND: ", command]
        ]

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
  
  Module[{textLines, lintTag, lintData, lintSrc, lintLine0, lineText, 
          disableLineEdit, disableNextLineEdit, disableLineText, disableNextLineText,
          lineEndChar, lintDiagnostics, ignoreData, tagStr},
    
    textLines = StringSplit[text, {"\r\n", "\n", "\r"}, All];
    
    (* Get ignore data to skip already-suppressed lints *)
    ignoreData = Lookup[entry, "IgnoreData", GetIgnoreData[uri]];
    
    Do[
      lintTag = lint[[1]];
      tagStr = If[StringQ[lintTag], lintTag, SymbolName[lintTag]];
      lintData = lint[[4]];
      lintSrc = Lookup[lintData, Source, Lookup[lintData, CodeParser`Source, {{1, 1}, {1, 1}}]];
      
      (* Check if this lint intersects the cursor (1-based) *)
      If[!SourceMemberIntersectingQ[lintSrc, cursor],
        Continue[]
      ];
      
      (* Skip already-suppressed lints *)
      If[ShouldIgnoreDiagnostic[lint, ignoreData],
        Continue[]
      ];
      
      (* 0-based line number for LSP *)
      lintLine0 = lintSrc[[1, 1]] - 1;
      
      lintDiagnostics = lintToDiagnostics[lint];
      
      (* --- Action 1: Disable for this line --- *)
      (* Insert " (* wl-disable-line RuleName *)" at end of the line *)
      If[lintLine0 < Length[textLines],
        lineText = textLines[[lintLine0 + 1]];
        lineEndChar = StringLength[lineText];
        disableLineText = " (* wl-disable-line " <> tagStr <> " *)";
        
        disableLineEdit = <| "changes" -> <| uri -> {
          <| "range" -> <| "start" -> <| "line" -> lintLine0, "character" -> lineEndChar |>,
                           "end" -> <| "line" -> lintLine0, "character" -> lineEndChar |> |>,
             "newText" -> disableLineText |>
        } |> |>;
        
        AppendTo[lspActions, <|
          "title" -> "Disable \"" <> tagStr <> "\" for this line",
          "kind" -> "quickfix",
          "edit" -> disableLineEdit,
          "diagnostics" -> lintDiagnostics
        |>]
      ];
      
      (* --- Action 2: Disable for next line --- *)
      (* Insert "(* wl-disable-next-line RuleName *)\n" before the diagnostic's line *)
      disableNextLineText = "(* wl-disable-next-line " <> tagStr <> " *)\n";
      
      disableNextLineEdit = <| "changes" -> <| uri -> {
        <| "range" -> <| "start" -> <| "line" -> lintLine0, "character" -> 0 |>,
                         "end" -> <| "line" -> lintLine0, "character" -> 0 |> |>,
           "newText" -> disableNextLineText |>
      } |> |>;
      
      AppendTo[lspActions, <|
        "title" -> "Disable \"" <> tagStr <> "\" for next line",
        "kind" -> "quickfix",
        "edit" -> disableNextLineEdit,
        "diagnostics" -> lintDiagnostics
      |>],
      
      {lint, lints}
    ]
  ];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> lspActions |>}
]]

End[]

EndPackage[]
