(* ::Package:: *)

BeginPackage["LSPServer`Completion`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`PacletIndex`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


(*
LSP Completion Item Kinds
*)
$CompletionItemKind = <|
  "Text" -> 1,
  "Method" -> 2,
  "Function" -> 3,
  "Constructor" -> 4,
  "Field" -> 5,
  "Variable" -> 6,
  "Class" -> 7,
  "Interface" -> 8,
  "Module" -> 9,
  "Property" -> 10,
  "Unit" -> 11,
  "Value" -> 12,
  "Enum" -> 13,
  "Keyword" -> 14,
  "Snippet" -> 15,
  "Color" -> 16,
  "File" -> 17,
  "Reference" -> 18,
  "Folder" -> 19,
  "EnumMember" -> 20,
  "Constant" -> 21,
  "Struct" -> 22,
  "Event" -> 23,
  "Operator" -> 24,
  "TypeParameter" -> 25
|>


(*
Handle textDocument/completion request
*)
expandContent[content:KeyValuePattern["method" -> "textDocument/completion"], pos_] :=
Catch[
Module[{params, id, doc, uri},

  If[$Debug2,
    log["textDocument/completion: enter expand"]
  ];

  id = content["id"];
  params = content["params"];
  
  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    If[$Debug2,
      log["canceled"]
    ];
    
    Throw[{<| "method" -> "textDocument/completionFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    If[$Debug2,
      log["stale"]
    ];

    Throw[{<| "method" -> "textDocument/completionFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
    "textDocument/completionFencepost"
  }
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/completionFencepost"]] :=
Catch[
Module[{id, params, doc, uri, position, entry, text, line, char, prefix,
  completions, systemCompletions, pacletCompletions, optionCompletions,
  contextCompletions, externalCompletions, result},

  If[$Debug2,
    log["textDocument/completionFencepost: enter"]
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

  position = params["position"];
  line = position["line"];
  char = position["character"];

  (*
  Convert from 0-based to 1-based
  *)
  line += 1;
  char += 1;

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  text = entry["Text"];

  (*
  Get the prefix at the cursor position
  *)
  prefix = getCompletionPrefix[text, line, char];

  If[$Debug2,
    log["completion prefix: ", prefix]
  ];

  (*
  If prefix is empty or too short, return empty list
  *)
  If[StringLength[prefix] < 1,
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> <| "isIncomplete" -> False, "items" -> {} |> |>}]
  ];

  (*
  Collect completions from different sources
  *)
  systemCompletions = getSystemSymbolCompletions[prefix];
  pacletCompletions = getPacletSymbolCompletions[prefix];
  optionCompletions = getOptionCompletions[text, line, char, prefix];
  contextCompletions = getContextCompletions[prefix];
  externalCompletions = getExternalSymbolCompletions[prefix];

  (*
  Combine and deduplicate
  Priority order: paclet symbols, external package symbols, system symbols, options, contexts
  *)
  completions = Join[pacletCompletions, externalCompletions, systemCompletions, optionCompletions, contextCompletions];
  completions = DeleteDuplicatesBy[completions, #["label"]&];

  (*
  Limit the number of results
  *)
  completions = Take[completions, UpTo[100]];

  result = <|
    "isIncomplete" -> Length[completions] >= 100,
    "items" -> completions
  |>;

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> result |>}
]]


(*
Get the identifier prefix at the cursor position
*)
getCompletionPrefix[text_String, line_Integer, char_Integer] :=
Module[{lines, currentLine, beforeCursor, prefix},
  
  lines = StringSplit[text, {"\r\n", "\n", "\r"}, All];
  
  If[line > Length[lines],
    Return[""]
  ];
  
  currentLine = lines[[line]];
  
  If[char > StringLength[currentLine] + 1,
    Return[""]
  ];
  
  beforeCursor = StringTake[currentLine, char - 1];
  
  (*
  Extract the identifier being typed
  Wolfram identifiers can contain letters, digits, $ and `
  *)
  prefix = StringCases[beforeCursor, 
    RegularExpression["[a-zA-Z$`][a-zA-Z0-9$`]*$"] :> "$0"
  ];
  
  If[prefix === {},
    "",
    Last[prefix]
  ]
]


(*
Cached set of constants for O(1) lookup
*)
$constantsSet := $constantsSet = Association[Thread[WolframLanguageSyntax`Generate`$constants -> True]]

(*
Cached combined list of all system symbols
*)
$allSystemSymbols := $allSystemSymbols = Join[
  WolframLanguageSyntax`Generate`$builtinFunctions,
  WolframLanguageSyntax`Generate`$constants
]

(*
Get completions from system symbols
*)
getSystemSymbolCompletions[prefix_String] :=
Module[{matching, completions},
  
  (*
  Filter by prefix - use cached combined list
  *)
  matching = Select[
    $allSystemSymbols,
    StringStartsQ[#, prefix, IgnoreCase -> True]&
  ];
  
  (*
  Sort by relevance (exact prefix match first, then shorter names)
  *)
  matching = SortBy[matching, {
    !StringStartsQ[#, prefix]&,  (* Exact case match first *)
    StringLength[#]&              (* Shorter names first *)
  }];
  
  (*
  Create completion items - use cached constant set for O(1) lookup
  *)
  completions = Table[
    Module[{kind, isConstant},
      isConstant = KeyExistsQ[$constantsSet, sym];
      kind = If[isConstant, 
        $CompletionItemKind["Constant"], 
        $CompletionItemKind["Function"]
      ];
      
      <|
        "label" -> sym,
        "kind" -> kind,
        "detail" -> If[isConstant, "System Constant", "System Function"],
        "sortText" -> "1_" <> sym  (* System symbols sort after paclet symbols *)
      |>
    ],
    {sym, Take[matching, UpTo[50]]}
  ];
  
  completions
]


(*
Get completions from paclet symbols
*)
getPacletSymbolCompletions[prefix_String] :=
Module[{pacletSymbols, completions},
  
  pacletSymbols = GetSymbolsForCompletion[prefix];
  
  completions = Table[
    Module[{kind, detail, item},
      kind = Switch[sym["kind"],
        "function", $CompletionItemKind["Function"],
        "constant", $CompletionItemKind["Constant"],
        "option", $CompletionItemKind["Property"],
        "attribute", $CompletionItemKind["Property"],
        _, $CompletionItemKind["Variable"]
      ];
      
      detail = If[sym["usage"] =!= None,
        StringTake[sym["usage"], UpTo[100]],
        "Paclet Symbol"
      ];
      
      item = <|
        "label" -> sym["name"],
        "kind" -> kind,
        "detail" -> detail,
        "sortText" -> "0_" <> sym["name"]  (* Paclet symbols sort first *)
      |>;
      
      (* Only add documentation if usage is available *)
      If[sym["usage"] =!= None,
        item["documentation"] = <| "kind" -> "markdown", "value" -> sym["usage"] |>
      ];
      
      item
    ],
    {sym, Take[pacletSymbols, UpTo[50]]}
  ];
  
  completions
]


(*
Get option completions when inside a function call
*)
getOptionCompletions[text_String, line_Integer, char_Integer, prefix_String] :=
Module[{options, matching, completions},
  
  (*
  Get all known options from the loaded data
  *)
  options = WolframLanguageSyntax`Generate`$options;
  
  (*
  Filter by prefix
  *)
  matching = Select[options, StringStartsQ[#, prefix, IgnoreCase -> True]&];
  
  matching = SortBy[matching, StringLength];
  
  completions = Table[
    <|
      "label" -> opt,
      "kind" -> $CompletionItemKind["Property"],
      "detail" -> "Option",
      "sortText" -> "2_" <> opt,
      "insertText" -> opt <> " -> "
    |>,
    {opt, Take[matching, UpTo[20]]}
  ];
  
  completions
]


(*
Get context completions (for `name patterns)
*)
getContextCompletions[prefix_String] :=
Module[{contexts, matching, completions},
  
  (*
  If prefix contains `, it might be a context-qualified symbol
  *)
  If[!StringContainsQ[prefix, "`"],
    Return[{}]
  ];
  
  (*
  Get contexts from the paclet index
  *)
  contexts = Keys[$PacletIndex["Contexts"]];
  
  (*
  Also get system contexts
  *)
  contexts = Join[contexts, {"System`", "Global`", "Developer`", "Internal`"}];
  
  (*
  Filter by prefix
  *)
  matching = Select[contexts, StringStartsQ[#, prefix, IgnoreCase -> True]&];
  
  completions = Table[
    <|
      "label" -> ctx,
      "kind" -> $CompletionItemKind["Module"],
      "detail" -> "Context",
      "sortText" -> "3_" <> ctx
    |>,
    {ctx, Take[matching, UpTo[20]]}
  ];
  
  completions
]


(*
Get completions from external loaded packages (dependencies)
*)
getExternalSymbolCompletions[prefix_String] :=
Module[{deps, allExternalSymbols, matching, completions},
  
  (*
  Get dependency contexts that have been loaded
  *)
  deps = GetDependencyContexts[];
  
  If[Length[deps] == 0,
    Return[{}]
  ];
  
  (*
  Get symbols from each dependency context
  *)
  allExternalSymbols = Flatten[
    Table[
      Quiet[
        Names[ctx <> "*"],
        {Names::notfound}
      ],
      {ctx, deps}
    ]
  ];
  
  (*
  Extract just the symbol names (without context prefix) for matching
  *)
  matching = Select[allExternalSymbols, 
    Function[{fullName},
      Module[{bareSymbol},
        bareSymbol = Last[StringSplit[fullName, "`"]];
        StringStartsQ[bareSymbol, prefix, IgnoreCase -> True]
      ]
    ]
  ];
  
  (*
  Sort by relevance
  *)
  matching = SortBy[matching, {
    !StringStartsQ[Last[StringSplit[#, "`"]], prefix]&,  (* Exact case match first *)
    StringLength[#]&  (* Shorter names first *)
  }];
  
  (*
  Create completion items
  *)
  completions = Table[
    Module[{bareSymbol, symContext, usage, detail, item},
      bareSymbol = Last[StringSplit[fullName, "`"]];
      symContext = StringJoin[Riffle[Most[StringSplit[fullName, "`"]], "`"]] <> "`";
      
      (*
      Try to get usage message
      *)
      usage = Quiet[
        Check[
          ToExpression[fullName <> "::usage"],
          None,
          {MessageName::noinfo}
        ],
        {MessageName::noinfo, ToExpression::notstrbox}
      ];
      
      (*
      Clean up usage for display - remove newlines and truncate
      *)
      detail = If[StringQ[usage],
        StringTake[
          StringReplace[usage, {"\n" -> " ", "\r" -> " ", RegularExpression["\\s+"] -> " "}],
          UpTo[80]
        ],
        symContext
      ];
      
      item = <|
        "label" -> bareSymbol,
        "kind" -> $CompletionItemKind["Function"],
        "detail" -> detail,
        "sortText" -> "1_" <> bareSymbol,  (* Sort after paclet symbols but with system symbols *)
        "labelDetails" -> <| "description" -> symContext |>
      |>;
      
      (*
      Add full documentation if usage is available
      *)
      If[StringQ[usage],
        item["documentation"] = <| "kind" -> "markdown", "value" -> usage |>
      ];
      
      item
    ],
    {fullName, Take[matching, UpTo[30]]}
  ];
  
  completions
]


(*
Handle completion item resolve for additional details
*)
handleContent[content:KeyValuePattern["method" -> "completionItem/resolve"]] :=
Catch[
Module[{id, params, label, documentation, result},

  If[$Debug2,
    log["completionItem/resolve: enter"]
  ];

  id = content["id"];
  params = content["params"];
  label = params["label"];

  (*
  Try to get documentation for the symbol
  *)
  documentation = getSymbolDocumentation[label];

  result = params;
  
  If[documentation =!= None,
    result["documentation"] = <| "kind" -> "markdown", "value" -> documentation |>
  ];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> result |>}
]]


(*
Get documentation for a symbol
*)
getSymbolDocumentation[symbolName_String] :=
Module[{usage, pacletUsages},
  
  (*
  First try system symbol usage
  *)
  If[NameQ[symbolName] && Context[symbolName] === "System`",
    usage = Quiet[ToExpression[symbolName <> "::usage"]];
    If[StringQ[usage],
      Return[usage]
    ]
  ];
  
  (*
  Then try paclet index
  *)
  If[KeyExistsQ[$PacletIndex["Symbols"], symbolName],
    pacletUsages = $PacletIndex["Symbols", symbolName, "Usages"];
    If[Length[pacletUsages] > 0,
      Return[pacletUsages[[1]]]
    ]
  ];
  
  None
]


End[]

EndPackage[]
