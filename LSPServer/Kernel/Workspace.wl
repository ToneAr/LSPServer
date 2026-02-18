(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>|>:: *)

BeginPackage["LSPServer`Workspace`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


handleContent[content:KeyValuePattern["method" -> "workspace/executeCommand"]] :=
Catch[
Module[{params, id, command, res},

  log[1, "workspace/executeCommand: enter"];

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "$CancelMap: ", $CancelMap];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];
  
  params = content["params"];
  command = params["command"];

  Switch[command,
    (*
    toggle_inlay_hints toggles $InlayHints on/off.
    Returns the new state so the client can display a notification.
    Also sends workspace/inlayHint/refresh to prompt the client to re-request hints.
    *)
    "toggle_inlay_hints",
      $InlayHints = !TrueQ[$InlayHints];
      
      If[$Debug,
        log["toggle_inlay_hints: $InlayHints -> ", $InlayHints]
      ];
      
      (*
      Send workspace/inlayHint/refresh to notify the client that
      inlay hints have changed and should be re-requested.
      The client will call textDocument/inlayHint again for visible files.
      *)
      {
        <| "jsonrpc" -> "2.0", "id" -> id, "result" -> <|"inlayHints" -> $InlayHints|> |>,
        <| "method" -> "workspace/inlayHint/refresh" |>
      }
    ,
    (*
    enable_bracket_matcher_debug_mode is an undocumented debug command
    *)
    "enable_bracket_matcher_debug_mode",
      $DebugBracketMatcher = True;
      (*
      TODO: trigger bracket matcher here
      *)
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
    ,
    (*
    disable_bracket_matcher_debug_mode is an undocumented debug command
    *)
    "disable_bracket_matcher_debug_mode",
      $DebugBracketMatcher = False;
      (*
      TODO: trigger bracket matcher here
      *)
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
    ,
    (*
    enable_bracket_matcher_design_colors is an undocumented debug command
    *)
    "enable_bracket_matcher_design_colors",
      $BracketMatcherUseDesignColors = True;
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
    ,
    (*
    disable_bracket_matcher_design_colors is an undocumented debug command
    *)
    "disable_bracket_matcher_design_colors",
      $BracketMatcherUseDesignColors = False;
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
    ,
    (*
    enable_bracket_matcher_display_insertion_text is an undocumented debug command
    *)
    "enable_bracket_matcher_display_insertion_text",
      $BracketMatcherDisplayInsertionText = True;
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
    ,
    (*
    disable_bracket_matcher_display_insertion_text is an undocumented debug command
    *)
    "disable_bracket_matcher_display_insertion_text",
      $BracketMatcherDisplayInsertionText = False;
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
    ,
    (*
    roundtrip_responsiveness_test is an undocumented, debug command
    *)
    "roundtrip_responsiveness_test",

            log[1, "roundtrip_responsiveness_test:> \n\n"];
            log[1, DateString[Now, {"Year", "-", "Month", "-", "Day", "_", "Hour24", "-", "Minute", "-", "Second", "-", "Millisecond"}]];

            {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>, <| "method" -> "roundTripTest" |>}
          ,
          (*
          ping_pong_responsiveness_test is an undocumented, debug command
          *)
          "ping_pong_responsiveness_test",

            log[1, "ping_pong_responsiveness_test:> \n\n"];

            {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>, <| "method" -> "pingPongTest" |>}    
          ,
          (*
          payload_responsiveness_test is an undocumented, debug command
          *)
          "payload_responsiveness_test",

            log[1, "payload_responsiveness_test:> \n\n"];

            {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>, <| "method" -> "payloadTest", "payload" -> StringJoin@Flatten@Table[CharacterRange["a", "z"], 100000] |>}
          ,
          _,
            
            log[1, "UNSUPPORTED COMMAND: ", command];
            
            {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
        ];

  log[1, "workspace/executeCommand: exit"];

  res
]]


handleContent[content:KeyValuePattern["method" -> "workspace/didChangeWatchedFiles"]] :=
Module[{},

  log[1, "workspace/didChangeWatchedFiles: enter"];
  log[1, "workspace/didChangeWatchedFiles: exit"];

  {}
]

handleContent[content:KeyValuePattern["method" -> "workspace/didChangeConfiguration"]] :=
Module[{params, settings},

  log[1, "workspace/didChangeConfiguration: enter"];
  log[1, "workspace/didChangeConfiguration: exit"];

  params = content["params"];
  settings = Lookup[params, "settings", Null];
  
  If[AssociationQ[settings],
    (*
    Handle wolfram/wolframLSP settings section
    Clients may nest under "wolfram", "wolframLSP", or send at top level
    *)
    settings = Replace[settings, {
      KeyValuePattern["wolfram" -> s_Association] :> s,
      KeyValuePattern["wolframLSP" -> s_Association] :> s,
      other_ :> other
    }];
    
    If[KeyExistsQ[settings, "inlayHints"],
      $InlayHints = TrueQ[settings["inlayHints"]];
      
      If[$Debug2,
        log["didChangeConfiguration: $InlayHints -> ", $InlayHints]
      ]
    ]
  ];

  {}
]


(*
Handle workspace/symbol search request
*)
handleContent[content:KeyValuePattern["method" -> "workspace/symbol"]] :=
Catch[
Module[{id, params, query, symbols, results, symbolKindMap},

  If[$Debug2,
    log["workspace/symbol: enter"]
  ];

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    If[$Debug2,
      log["canceled"]
    ];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}]
  ];
  
  params = content["params"];
  query = Lookup[params, "query", ""];

  If[$Debug2,
    log["workspace/symbol query: ", query]
  ];

  (*
  Search workspace symbols
  *)
  symbols = SearchWorkspaceSymbols[query];

  (*
  Map internal kind to LSP SymbolKind
  *)
  symbolKindMap = <|
    "function" -> 12,  (* Function *)
    "constant" -> 14,  (* Constant *)
    "option" -> 7,     (* Property *)
    "attribute" -> 7,  (* Property *)
    "unknown" -> 13    (* Variable *)
  |>;

  (*
  Convert to LSP format
  *)
  results = Table[
    <|
      "name" -> sym["name"],
      "kind" -> Lookup[symbolKindMap, sym["kind"], 13],
      "location" -> sym["location"],
      "containerName" -> sym["containerName"]
    |>,
    {sym, Take[symbols, UpTo[100]]}
  ];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> results |>}
]]


(*
Handle workspace folder changes
*)
handleContent[content:KeyValuePattern["method" -> "workspace/didChangeWorkspaceFolders"]] :=
Module[{params, event, added, removed},

  If[$Debug2,
    log["workspace/didChangeWorkspaceFolders: enter"]
  ];

  params = content["params"];
  event = params["event"];
  added = Lookup[event, "added", {}];
  removed = Lookup[event, "removed", {}];

  (*
  Handle removed folders - remove files from index
  *)
  Scan[
    Function[{folder},
      Module[{folderPath, filesToRemove},
        folderPath = normalizeURI[folder["uri"]];
        filesToRemove = Select[Keys[$PacletIndex["Files"]], StringStartsQ[#, "file://" <> folderPath]&];
        Scan[RemoveFileFromIndex, filesToRemove]
      ]
    ],
    removed
  ];

  (*
  Handle added folders - scan and index files
  *)
  Scan[
    Function[{folder},
      Module[{folderPath, files},
        folderPath = normalizeURI[folder["uri"]];
        (*
        If this is the first workspace folder, set it as root
        *)
        If[$WorkspaceRootPath === None,
          $WorkspaceRootPath = folderPath
        ];
        (*
        Index files in the new folder
        *)
        files = FileNames[{"*.wl", "*.m", "*.wls"}, folderPath, Infinity];
        Scan[indexFile, files]
      ]
    ],
    added
  ];

  {}
]


End[]

EndPackage[]
