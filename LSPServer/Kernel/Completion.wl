(* ::Package:: *)

BeginPackage["LSPServer`Completion`"]

(* Load CodeParser before entering Private context so that
   LeafNode, BinaryNode, GroupNode etc. are recognized as CodeParser symbols *)
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`PacletIndex`"]
Needs["LSPServer`Utils`"]


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
  contextCompletions, externalCompletions, kernelCtxCompletions,
  keyCompletions, cst, result, keyContext},

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
  cst = Lookup[entry, "CST", Null];

  (*
  Check if we're in an association key context (e.g., assoc[" or assoc[Key[")
  *)
  keyContext = getAssociationKeyContext[text, line, char];
  
  (*
  Debug: log to file for diagnostics
  *)
  Quiet[
    Module[{debugStream, linesDbg, beforeCursorDbg},
      debugStream = OpenAppend["/tmp/lsp-assoc-debug.log"];
      linesDbg = StringSplit[text, {"\r\n", "\n", "\r"}, All];
      beforeCursorDbg = If[line <= Length[linesDbg] && char - 1 <= StringLength[linesDbg[[line]]],
        StringTake[linesDbg[[line]], char - 1], "OUT_OF_BOUNDS"];
      WriteString[debugStream, 
        "--- completion ---\n" <>
        "uri=" <> ToString[uri] <> "\n" <>
        "line=" <> ToString[line] <> " char=" <> ToString[char] <> 
        " cst=" <> If[cst === Null, "Null", "ok"] <>
        " before=" <> ToString[InputForm[beforeCursorDbg]] <>
        " ctx=" <> ToString[keyContext] <> "\n"
      ];
      Close[debugStream];
    ]
  ];

  If[keyContext =!= None,
    (*
    We're completing an association key.
    Pass line/char (0-based for LSP) and the full line text so the
    completion builder can create a textEdit that handles existing quotes.
    *)
    Module[{lspLine0, lspChar0, fullLine, linesForEdit},
      lspLine0 = line - 1;  (* back to 0-based for LSP *)
      lspChar0 = char - 1;
      linesForEdit = StringSplit[text, {"\r\n", "\n", "\r"}, All];
      fullLine = If[line <= Length[linesForEdit], linesForEdit[[line]], ""];
      
      keyCompletions = getAssociationKeyCompletions[cst, text, keyContext, lspLine0, lspChar0, fullLine];
    ];
    
    (*
    Debug: log completions
    *)
    Quiet[
      Module[{debugStream, keys},
        debugStream = OpenAppend["/tmp/lsp-assoc-debug.log"];
        keys = extractAssociationKeys[cst, text];
        WriteString[debugStream, 
          "keyPath=" <> ToString[keyContext["keyPath"]] <> "\n" <>
          "extractedKeys=" <> ToString[keys] <> "\n" <>
          "completionCount=" <> ToString[Length[keyCompletions]] <> "\n" <>
          "completionLabels=" <> ToString[Map[#["label"]&, keyCompletions]] <> "\n\n"
        ];
        Close[debugStream];
      ]
    ];
    
    result = <|
      "isIncomplete" -> False,
      "items" -> keyCompletions
    |>;
    
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> result |>}]
  ];

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
  kernelCtxCompletions = getKernelContextSymbolCompletions[prefix];

  (*
  Combine and deduplicate
  Priority order: paclet symbols, external package symbols, kernel context symbols,
  system symbols, options, contexts
  *)
  completions = Join[pacletCompletions, externalCompletions, kernelCtxCompletions, systemCompletions, optionCompletions, contextCompletions];
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
Module[{contexts, kernelContexts, matching, completions},
  
  (*
  If prefix contains `, it might be a context-qualified symbol
  *)
  If[!StringContainsQ[prefix, "`"],
    Return[{}]
  ];
  
  (*
  Get contexts from the paclet index (workspace-defined)
  *)
  contexts = Keys[$PacletIndex["Contexts"]];
  
  (*
  Get all kernel-known contexts using Contexts[].
  This dynamically discovers all available contexts (Internal`, Developer`,
  Compile`, JLink`, etc.) instead of relying on a hardcoded list.
  *)
  kernelContexts = GetKernelContextsCached[];
  
  contexts = DeleteDuplicates[Join[contexts, kernelContexts]];
  
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
Get symbol completions from kernel-known contexts using Names[].
This provides completions for context-qualified prefixes like Internal`Bag, Developer`To, etc.
without requiring the user to add Needs[] for these built-in contexts.

Only activates when the prefix contains a backtick AND what comes after the last backtick
is a partial symbol name (or empty, to list all symbols in the context).
*)
getKernelContextSymbolCompletions[prefix_String] :=
Module[{contexts, ctxPart, symbolPrefix, allNames, matching, completions},
  
  (* Only activate for context-qualified prefixes *)
  If[!StringContainsQ[prefix, "`"],
    Return[{}]
  ];
  
  (*
  Split prefix into context part and symbol prefix.
  E.g. "Internal`Bag" -> context="Internal`", symbolPrefix="Bag"
       "Internal`"    -> context="Internal`", symbolPrefix=""
  *)
  ctxPart = StringJoin[
    Riffle[Most[StringSplit[prefix, "`", All]], "`"]
  ] <> "`";
  symbolPrefix = Last[StringSplit[prefix, "`", All]];
  
  (*
  Check if this context is known to the kernel.
  Only provide completions for contexts that actually exist.
  *)
  contexts = GetKernelContextsCached[];
  
  If[!MemberQ[contexts, ctxPart],
    Return[{}]
  ];
  
  (*
  Use Names[] to get all symbols in this context matching the prefix.
  Names[] does not trigger loading or warnings — it only inspects existing symbols.
  *)
  allNames = Quiet[
    Names[ctxPart <> symbolPrefix <> "*"],
    {Names::notfound}
  ];
  If[!ListQ[allNames], allNames = {}];
  
  (*
  Sort by relevance: exact case prefix match first, then shorter names first
  *)
  matching = SortBy[allNames, {
    !StringStartsQ[#, prefix]&,
    StringLength[#]&
  }];
  
  (*
  Create completion items.
  The label is the fully-qualified name (e.g. "Internal`Bag").
  *)
  completions = Table[
    Module[{bareSymbol, usage, detail},
      bareSymbol = Last[StringSplit[fullName, "`"]];
      
      (*
      Try to get usage message without triggering evaluation
      *)
      usage = Quiet[
        Check[
          ToExpression[fullName <> "::usage"],
          None,
          {MessageName::noinfo}
        ],
        {MessageName::noinfo, ToExpression::notstrbox}
      ];
      
      detail = If[StringQ[usage],
        StringTake[
          StringReplace[usage, {"\n" -> " ", "\r" -> " ", RegularExpression["\\s+"] -> " "}],
          UpTo[80]
        ],
        ctxPart
      ];
      
      <|
        "label" -> fullName,
        "kind" -> $CompletionItemKind["Function"],
        "detail" -> detail,
        "sortText" -> "1_" <> bareSymbol,
        "filterText" -> fullName,
        "labelDetails" -> <| "description" -> ctxPart |>
      |>
    ],
    {fullName, Take[matching, UpTo[50]]}
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
========================================
Association Key Completion
========================================
*)

(*
Parse multi-key Part access: assoc["a", "b", " 
Scans backwards from commaPos (position of the comma) to extract the
preceding string keys and the variable name.
Returns None or <| "variable" -> ..., "keyPath" -> {...} |>
*)
parseCommaKeyPath[beforeCursor_String, commaPos_Integer] :=
Module[{chars, scanPos, keysReversed, keyStr, startKey, endKey, 
        varEnd, varStart, varName},
  
  chars = Characters[beforeCursor];
  keysReversed = {};
  scanPos = commaPos - 1;  (* move before the comma *)
  
  (*
  Scan backwards, collecting "key", integer, All, Span groups separated by commas
  until we hit the opening [ or [[
  *)
  While[scanPos > 0,
    (* Skip whitespace *)
    While[scanPos > 0 && MemberQ[{" ", "\t"}, chars[[scanPos]]],
      scanPos--
    ];
    If[scanPos < 1, Return[None]];
    
    If[chars[[scanPos]] === "\"",
      (* String key: scan backwards through "key" *)
      scanPos--;  (* past closing " *)
      
      (* Read the key backwards to the opening quote *)
      endKey = scanPos;
      While[scanPos > 0 && chars[[scanPos]] =!= "\"",
        scanPos--
      ];
      If[scanPos < 1, Return[None]];
      startKey = scanPos;
      keyStr = StringJoin[chars[[startKey + 1 ;; endKey]]];
      AppendTo[keysReversed, keyStr];
      scanPos--,  (* past opening " *)
      
      (* Not a quote. Could be integer, All, Span (;;), or the opening bracket *)
      If[chars[[scanPos]] === "[",
        (* Opening bracket — done scanning keys *)
        scanPos--;
        If[scanPos >= 1 && chars[[scanPos]] === "[",
          scanPos--
        ];
        Break[]
      ];
      
      (* Try to read a non-string expression (integer, All, 1;;3, etc.) backwards *)
      Module[{exprEnd, exprStart, exprStr},
        exprEnd = scanPos;
        exprStart = exprEnd;
        (* Scan backwards collecting digits, letters, ;, -, $, spaces *)
        While[exprStart > 0 && 
          (LetterQ[chars[[exprStart]]] || DigitQ[chars[[exprStart]]] || 
           MemberQ[{";", "-", "$", " "}, chars[[exprStart]]]),
          exprStart--
        ];
        exprStart++;
        If[exprStart > exprEnd, Return[None]];
        exprStr = StringReplace[StringJoin[chars[[exprStart ;; exprEnd]]],
          RegularExpression["^\\s+|\\s+$"] -> ""];
        If[exprStr === "", Return[None]];
        AppendTo[keysReversed, exprStr];
        scanPos = exprStart - 1
      ]
    ];
    
    (* Skip whitespace *)
    While[scanPos > 0 && MemberQ[{" ", "\t"}, chars[[scanPos]]],
      scanPos--
    ];
    If[scanPos < 1, Return[None]];
    
    (* Expect either , (another key before this) or [ (opening bracket) *)
    If[chars[[scanPos]] === ",",
      scanPos--;  (* skip comma, continue to next key *)
      Continue[]
    ];
    
    If[chars[[scanPos]] === "[",
      scanPos--;  (* skip the [ *)
      (* Also skip a second [ if present (Part syntax) *)
      If[scanPos >= 1 && chars[[scanPos]] === "[",
        scanPos--
      ];
      Break[]
    ];
    
    (* Unexpected character *)
    Return[None]
  ];
  
  (* What remains should be the variable name *)
  (* Skip whitespace *)
  While[scanPos > 0 && MemberQ[{" ", "\t"}, chars[[scanPos]]],
    scanPos--
  ];
  If[scanPos < 1, Return[None]];
  
  varEnd = scanPos;
  varStart = varEnd;
  While[varStart > 0 && (LetterQ[chars[[varStart]]] || DigitQ[chars[[varStart]]] || chars[[varStart]] === "$"),
    varStart--
  ];
  varStart++;
  If[varStart > varEnd, Return[None]];
  
  varName = StringJoin[chars[[varStart ;; varEnd]]];
  If[varName === "" || varName === "Key", Return[None]];
  
  <|
    "variable" -> varName,
    "keyPath" -> Reverse[keysReversed]
  |>
]


(*
Check if the cursor is in an association key context.

Scans backwards from the cursor through the beforeCursor string, tracking
bracket depth to correctly identify chained key access like:
  data["user"]["address"]["  -> variable=data, keyPath={user, address}
Also handles comma-separated multi-key Part access:
  data["user", "address", "  -> variable=data, keyPath={user, address}
Also triggers on bare bracket access (no quote typed yet):
  data[  -> variable=data, keyPath={}, isStringKey=False

Returns None if not in key context, or an association with:
  - "variable": the variable name being accessed
  - "prefix": the partial key typed so far (may be non-string chars for bare bracket)
  - "isStringKey": True if typing a string key (after "), False if bracket-only
  - "keyPath": list of keys already accessed (for nested associations)
  - "prefixStart": 1-based character position where the prefix starts in the line
*)
getAssociationKeyContext[text_String, line_Integer, char_Integer] :=
Module[{lines, currentLine, beforeCursor, chars, pos, inString, prefix = "",
        segments, bracketDepth, segment, currentSegment, variable, keyPath,
        ch, prefixStart = -1, barePrefix = "", barePrefixStart = -1, 
        bracketPos = -1, isBracketOnly = False, isPart = False},
  
  lines = StringSplit[text, {"\r\n", "\n", "\r"}, All];
  
  If[line > Length[lines],
    Return[None]
  ];
  
  currentLine = lines[[line]];
  
  If[char > StringLength[currentLine] + 1,
    Return[None]
  ];
  
  beforeCursor = StringTake[currentLine, char - 1];
  
  If[StringLength[beforeCursor] < 2, Return[None]];
  
  chars = Characters[beforeCursor];
  pos = Length[chars];
  
  (*
  Step 0: Try bracket-only detection first.
  If the last character (after trimming whitespace and any non-quote prefix) is [ or [[,
  the user just typed the bracket — trigger with isStringKey=False.
  Also handle the case where user typed some non-quote chars after [ as a prefix
  (e.g., data[ho for filtering).
  *)
  (* Scan backwards past word characters to collect any bare prefix *)
  barePrefixStart = Length[chars] + 1;
  bracketPos = Length[chars];
  While[bracketPos > 0 && StringMatchQ[chars[[bracketPos]], WordCharacter | "$"],
    bracketPos--
  ];
  If[bracketPos >= 1 && bracketPos < Length[chars],
    barePrefix = StringJoin[chars[[bracketPos + 1 ;; Length[chars]]]];
    barePrefixStart = bracketPos + 1
  ];
  If[bracketPos >= 1 && chars[[bracketPos]] === "[",
    (* Check that this isn't inside a quote — no unmatched " between [ and cursor *)
    Module[{quoteCount, qi},
      quoteCount = 0;
      Do[
        If[chars[[qi]] === "\"", quoteCount++],
        {qi, bracketPos + 1, Length[chars]}
      ];
      (* If odd number of quotes, we're inside a string — don't use bracket-only path *)
      If[OddQ[quoteCount], Goto["QuotePath"]]
    ];
    (* We're at [ or [[ without a quote — bracket-only trigger *)
    isBracketOnly = True;
    Goto["BracketOnlyPath"]
  ];
  
  Label["QuotePath"];
  
  (*
  Step 1: Check if we're currently inside an open string (no closing quote).
  Scan backwards from end to find the last " that opens a string.
  *)
  prefix = "";
  prefixStart = -1;
  
  (* Scan backwards past any non-quote chars to find the opening quote *)
  While[pos > 0 && chars[[pos]] =!= "\"",
    pos--
  ];
  
  If[pos == 0, Return[None]];  (* No quote found *)
  
  (* pos is now at a " character. Everything after it is the prefix *)
  prefix = StringTake[beforeCursor, {pos + 1, -1}];
  prefixStart = pos;  (* 1-based position of the char after the quote *)
  
  (*
  Step 2: Check what's before the opening quote.
  Valid patterns:
    - [ before the quote: assoc["         -> first key access
    - [[ before the quote: assoc[["       -> Part first key
    - Key[ before the quote: assoc[Key["  -> Key first access
    - , before the quote: assoc["a", "    -> multi-key Part access (next level)
  *)
  Module[{preQuote, trimmed, lastChar, commaKeys},
    preQuote = StringTake[beforeCursor, pos - 1];
    trimmed = StringReplace[preQuote, RegularExpression["\\s+$"] -> ""];
    
    If[StringLength[trimmed] == 0, Return[None]];
    
    lastChar = StringTake[trimmed, -1];
    
    If[lastChar =!= "[" && lastChar =!= ",",
      Return[None]
    ];
    
    (*
    If the last char is a comma, this is multi-key Part syntax:
    assoc["a", "b", "  -> keyPath = {a, b}
    We need to scan backwards through the comma-separated string keys
    to find the opening bracket and variable name.
    *)
    If[lastChar === ",",
      commaKeys = parseCommaKeyPath[beforeCursor, StringLength[trimmed]];
      If[commaKeys === None, Return[None]];
      Return[<|
        "variable" -> commaKeys["variable"],
        "prefix" -> prefix,
        "isStringKey" -> True,
        "isPart" -> True,
        "keyPath" -> commaKeys["keyPath"],
        "prefixStart" -> prefixStart
      |>]
    ]
  ];
  
  Label["BracketOnlyPath"];
  
  (*
  Step 3: Parse the chained bracket access chain backwards.
  
  Two entry points:
  A) From quote path: pos is at the opening ", scan from pos-1 (the [) leftward.
     isStringKey = True, prefix/prefixStart already set from quote.
  B) From bracket-only path: bracketPos is at [, no quote involved.
     isStringKey = False, prefix = barePrefix, prefixStart = barePrefixStart.
  
  Strategy: scan left from the [, collecting completed bracket groups to build
  the keyPath, until we hit the base variable name.
  *)
  Module[{scanPos, keyPathReversed, depth, keyStr, scanChars, startKey, endKey,
          varEnd, varStart, varName},
    
    If[isBracketOnly,
      (* Bracket-only path: bracketPos is at [, barePrefix/barePrefixStart are set *)
      prefix = barePrefix;
      prefixStart = barePrefixStart;
      scanPos = bracketPos;
      scanChars = chars;
      keyPathReversed = {};
      
      (* Must be at [ *)
      If[scanPos < 1 || scanChars[[scanPos]] =!= "[",
        Return[None]
      ];
      scanPos--;
      (* Also skip a second [ if present (Part syntax: d[[) *)
      If[scanPos >= 1 && scanChars[[scanPos]] === "[",
        scanPos--;
        isPart = True
      ],
      
      (* Quote path: pos is at the opening quote, scan from pos-1 *)
      scanPos = StringLength[StringReplace[StringTake[beforeCursor, pos - 1], RegularExpression["\\s+$"] -> ""]];
      scanChars = chars;
      keyPathReversed = {};
      
      (* scanPos should now be at '[' *)
      (* Skip Key[ if present: ...Key[" *)
      If[scanPos >= 4 && StringTake[beforeCursor, {scanPos - 3, scanPos}] === "Key[",
        scanPos = scanPos - 4;
        If[scanPos >= 1 && scanChars[[scanPos]] === "[",
          scanPos--;
          (* Check for second [ (Part syntax: d[[Key[") *)
          If[scanPos >= 1 && scanChars[[scanPos]] === "[",
            scanPos--;
            isPart = True
          ]
        ]
      ,
        (* Must be at [ *)
        If[scanPos < 1 || scanChars[[scanPos]] =!= "[",
          Return[None]
        ];
        scanPos--;
        (* Also skip a second [ if present (Part syntax: d[[") *)
        If[scanPos >= 1 && scanChars[[scanPos]] === "[",
          scanPos--;
          isPart = True
        ]
      ]
    ];
    
    (* Skip whitespace *)
    While[scanPos > 0 && StringMatchQ[scanChars[[scanPos]], WhitespaceCharacter],
      scanPos--
    ];
    
    (* Now repeatedly try to match ]["key"] or ]]["key"]] or [[integer]] going leftward *)
    While[scanPos > 0 && scanChars[[scanPos]] === "]",
      (* We found a ], try to match a completed bracket group *)
      
      (* Check for ]] (Part syntax) *)
      If[scanPos >= 2 && scanChars[[scanPos - 1]] === "]",
        (* ]] -- look for matching [[ *)
        endKey = scanPos - 2;
        
        If[endKey >= 1 && scanChars[[endKey]] === "\"",
          (* String key in Part: [["key"]] *)
          endKey--;  (* past the closing " *)
          startKey = endKey;
          While[startKey > 0 && scanChars[[startKey]] =!= "\"",
            startKey--
          ];
          If[startKey < 1, Break[]];
          keyStr = StringTake[beforeCursor, {startKey + 1, endKey}];
          (* Now expect [[ before the opening " *)
          If[startKey < 3 || StringTake[beforeCursor, {startKey - 2, startKey - 1}] =!= "[[",
            Break[]
          ];
          AppendTo[keyPathReversed, keyStr];
          scanPos = startKey - 3,
          
          (* Non-string Part: [[1]], [[All]], [[1;;3]], [[;;5]] etc. *)
          Module[{exprEnd, exprStart, exprStr},
            exprEnd = endKey;
            exprStart = exprEnd;
            (* Scan backwards to find [[ — collect chars that are digits, letters, ;, $, whitespace *)
            While[exprStart > 0 && !(exprStart >= 2 && scanChars[[exprStart - 1]] === "[" && scanChars[[exprStart]] === "["),
              exprStart--
            ];
            If[exprStart < 2, Break[]];
            (* exprStart is at the second [, so the expression is from exprStart+1 to exprEnd *)
            exprStr = StringReplace[StringTake[beforeCursor, {exprStart + 1, exprEnd}], 
              RegularExpression["^\\s+|\\s+$"] -> ""];
            If[exprStr === "", Break[]];
            AppendTo[keyPathReversed, exprStr];
            scanPos = exprStart - 2
          ]
        ],
        
        (* Single ] -- look for matching [ *)
        endKey = scanPos - 1;
        (* Find the closing quote *)
        If[endKey < 1 || scanChars[[endKey]] =!= "\"",
          (* Could be Key["..."] *)
          If[endKey >= 2 && StringTake[beforeCursor, {endKey - 1, endKey}] === "\"]",
            endKey = endKey - 2;
            If[endKey < 1 || scanChars[[endKey]] =!= "\"", Break[]];
            endKey--;
            startKey = endKey;
            While[startKey > 0 && scanChars[[startKey]] =!= "\"", startKey--];
            If[startKey < 1, Break[]];
            keyStr = StringTake[beforeCursor, {startKey + 1, endKey}];
            (* Expect Key[ before the " *)
            If[startKey < 5 || StringTake[beforeCursor, {startKey - 4, startKey - 1}] =!= "Key[",
              Break[]
            ];
            (* Expect [ before Key[ *)
            If[startKey < 6 || scanChars[[startKey - 5]] =!= "[",
              Break[]
            ];
            AppendTo[keyPathReversed, keyStr];
            scanPos = startKey - 6,
            (* Could be a single-bracket non-string: [1], [All], etc. *)
            Module[{exprEnd2, exprStart2, exprStr2},
              exprEnd2 = endKey;
              exprStart2 = exprEnd2;
              While[exprStart2 > 0 && scanChars[[exprStart2]] =!= "[",
                exprStart2--
              ];
              If[exprStart2 < 1, Break[]];
              exprStr2 = StringReplace[StringTake[beforeCursor, {exprStart2 + 1, exprEnd2}],
                RegularExpression["^\\s+|\\s+$"] -> ""];
              If[exprStr2 === "", Break[]];
              (* Only accept if it looks like an integer, All, Span, or similar *)
              If[StringMatchQ[exprStr2, (DigitCharacter | LetterCharacter | ";" | "-" | " " | "$")..],
                AppendTo[keyPathReversed, exprStr2];
                scanPos = exprStart2 - 1,
                Break[]
              ]
            ]
          ],
          (* Standard ["key"] *)
          endKey--;  (* past the closing " *)
          startKey = endKey;
          While[startKey > 0 && scanChars[[startKey]] =!= "\"",
            startKey--
          ];
          If[startKey < 1, Break[]];
          keyStr = StringTake[beforeCursor, {startKey + 1, endKey}];
          (* Expect [ before the opening " *)
          If[startKey < 2 || scanChars[[startKey - 1]] =!= "[",
            Break[]
          ];
          AppendTo[keyPathReversed, keyStr];
          scanPos = startKey - 2
        ]
      ];
      
      (* Skip whitespace *)
      While[scanPos > 0 && StringMatchQ[scanChars[[scanPos]], WhitespaceCharacter],
        scanPos--
      ];
    ];
    
    (* What's left should be the variable name *)
    If[scanPos < 1, Return[None]];
    varEnd = scanPos;
    varStart = varEnd;
    While[varStart > 0 && StringMatchQ[scanChars[[varStart]], WordCharacter | "$"],
      varStart--
    ];
    varStart++;
    If[varStart > varEnd, Return[None]];
    
    varName = StringTake[beforeCursor, {varStart, varEnd}];
    If[varName === "Key" || varName === "", Return[None]];
    
    keyPath = Reverse[keyPathReversed];
    
    <|
      "variable" -> varName, 
      "prefix" -> prefix, 
      "isStringKey" -> !isBracketOnly,
      "isPart" -> isPart,
      "keyPath" -> keyPath,
      "prefixStart" -> prefixStart
    |>
  ]
]


(*
Get association key completions based on the context
Now supports hierarchical key lookup based on keyPath.
Uses textEdit to correctly replace the range between the opening quote
and any existing closing quote, so completions work whether or not
the closing quote already exists.

Arguments:
  cst - the CST (may be Null)
  text - full document text
  keyContext - from getAssociationKeyContext
  lspLine0 - 0-based line number (LSP format)
  lspChar0 - 0-based character offset of cursor (LSP format)
  fullLine - full text of the current line
*)
getAssociationKeyCompletions[cst_, text_String, keyContext_Association, 
                              lspLine0_Integer, lspChar0_Integer, fullLine_String] :=
Module[{variable, prefix, keyPath, allKeys, variableStructure, keysAtLevel, 
        matchingKeys, completions, pathDescription, editStartChar, editEndChar,
        afterCursor, hasClosingQuote, isStringKey, isPart},
  
  variable = keyContext["variable"];
  prefix = keyContext["prefix"];
  keyPath = Lookup[keyContext, "keyPath", {}];
  isStringKey = Lookup[keyContext, "isStringKey", True];
  isPart = Lookup[keyContext, "isPart", False];
  
  (*
  Extract hierarchical association keys defined in the file
  *)
  allKeys = extractAssociationKeys[cst, text];
  
  (*
  Get the hierarchical structure for the specific variable
  *)
  variableStructure = Lookup[allKeys, variable, <||>];
  
  (*
  Navigate to the correct level based on keyPath
  *)
  keysAtLevel = navigateToLevel[variableStructure, keyPath, isPart];
  
  (*
  Only show completions for variables that have known association/list definitions.
  If the variable is not found in the extracted keys, return nothing — do not
  fall back to showing keys from other variables.
  *)
  
  (*
  Filter by prefix.
  For bracket-only (isStringKey=False), prefix might match the beginning of the key
  or the beginning of a quoted representation. We filter on the raw key string.
  *)
  matchingKeys = If[prefix === "",
    keysAtLevel,
    Select[keysAtLevel, 
      Function[{key},
        Module[{keyStr},
          keyStr = If[StringQ[key], key, ToString[key]];
          StringStartsQ[keyStr, prefix, IgnoreCase -> True]
        ]
      ]
    ]
  ];
  
  (*
  When user typed a quote (isStringKey=True), filter out non-string keys
  (e.g. numeric indices) since the user is explicitly entering a string key.
  *)
  If[isStringKey,
    matchingKeys = Select[matchingKeys, StringQ]
  ];
  
  (*
  Sort by relevance
  *)
  matchingKeys = SortBy[matchingKeys, {
    If[prefix =!= "", !StringStartsQ[ToString[#], prefix], False]&,
    StringLength[ToString[#]]&
  }];
  
  (*
  Build path description for labelDetails
  *)
  pathDescription = If[Length[keyPath] > 0,
    variable <> "[" <> StringJoin[Riffle[Map[Function[{k},
      If[StringQ[k] && !StringMatchQ[k, DigitCharacter..],
        "\"" <> k <> "\"",
        k
      ]], keyPath], "]["]] <> "]",
    variable
  ];
  
  (*
  Determine the textEdit range.
  
  Two modes:
  A) isStringKey=True: cursor is after an opening " inside brackets.
     editStartChar = right after the opening "
     newText = key + closing "  (the opening " is already in the document)
     
  B) isStringKey=False: cursor is right after [ or [[ (bracket-only trigger).
     editStartChar = right after [ (where prefix starts, or cursor if no prefix)
     newText = "key" (with both quotes) for string keys, or raw value for non-string keys
  *)
  editStartChar = lspChar0 - StringLength[prefix];
  
  (* Check if there's a closing quote right at or after the cursor *)
  afterCursor = If[lspChar0 < StringLength[fullLine],
    StringTake[fullLine, {lspChar0 + 1, -1}],  (* chars after cursor *)
    ""
  ];
  
  If[isStringKey,
    (* Quote-based path: check for existing closing quote *)
    hasClosingQuote = StringLength[afterCursor] > 0 && StringTake[afterCursor, 1] === "\"";
    editEndChar = If[hasClosingQuote,
      lspChar0 + 1,  (* include the existing closing quote in the replacement range *)
      lspChar0       (* no closing quote, just replace the prefix *)
    ],
    (* Bracket-only path: check for existing closing "]" or "\"]" etc. *)
    editEndChar = lspChar0  (* just replace the prefix/cursor position *)
  ];
  
  (*
  Create completion items with textEdit
  *)
  completions = Table[
    Module[{keyStr, newText, item, textEdit, isKeyString, displayLabel, filterStr},
      keyStr = ToString[key];
      isKeyString = StringQ[key];
      
      (*
      Build the newText based on the mode:
      - isStringKey=True (quote already typed): key + closing "
      - isStringKey=False (bracket-only):
          - string key: "key" (both quotes)
          - non-string key (integer/symbol): raw value
      *)
      If[isStringKey,
        newText = keyStr <> "\"",
        If[isKeyString,
          newText = "\"" <> keyStr <> "\"",
          newText = keyStr
        ]
      ];
      
      displayLabel = If[isKeyString, "\"" <> keyStr <> "\"", keyStr];
      filterStr = keyStr;
      
      textEdit = <|
        "range" -> <|
          "start" -> <| "line" -> lspLine0, "character" -> editStartChar |>,
          "end"   -> <| "line" -> lspLine0, "character" -> editEndChar |>
        |>,
        "newText" -> newText
      |>;
      
      item = <|
        "label" -> displayLabel,
        "kind" -> $CompletionItemKind["Property"],
        "detail" -> "Association Key",
        "sortText" -> "0_" <> keyStr,
        "textEdit" -> textEdit,
        "filterText" -> filterStr,
        "labelDetails" -> <|"description" -> "from " <> pathDescription|>
      |>;
      
      item
    ],
    {key, Take[matchingKeys, UpTo[50]]}
  ];
  
  completions
]


(*
Navigate to the correct level in a hierarchical key structure
structure: <| "key1" -> <| "_children" -> <| ... |> |>, ... |>
keyPath: {"key1", "key2", ...}
Returns: list of keys at that level
*)
navigateToLevel[structure_Association, keyPath_List, includeIndices_:True] :=
Module[{current, key},
  current = structure;
  
  Do[
    (*
    If current level is a list-of-associations (_isList -> True),
    integer/All/Span indices navigate into the merged _children.
    String keys also navigate into _children (looking up inside the merged assocs).
    *)
    If[TrueQ[Lookup[current, "_isList", False]],
      (* This is a list of associations *)
      Module[{children, elements, idx},
        children = Lookup[current, "_children", <||>];
        elements = Lookup[current, "_elements", {}];
        If[IntegerQ[key] || key === "All" || key === All || 
           StringQ[key] && StringMatchQ[key, DigitCharacter..] ||
           (* Span-like patterns: "1;;3", ";;", "2;;", ";;5" *)
           StringQ[key] && StringContainsQ[key, ";;"],
          (* Index into list *)
          (* For specific integer index, use per-element keys if available *)
          idx = If[IntegerQ[key], key, 
                  If[StringQ[key] && StringMatchQ[key, DigitCharacter..], 
                    ToExpression[key], 0]];
          If[idx >= 1 && idx <= Length[elements],
            (* Navigate to specific element's keys *)
            current = elements[[idx]],
            (* All, Span, or out-of-range index -> use merged children *)
            current = children
          ],
          (* String key lookup inside the merged assoc keys *)
          If[KeyExistsQ[children, key],
            If[TrueQ[Lookup[children[key], "_isList", False]],
              current = children[key],
              current = Lookup[children[key], "_children", <||>]
            ],
            Return[{}, Module]
          ]
        ]
      ],
      (* Normal association navigation *)
      If[KeyExistsQ[current, key],
        (* If the value is a list-of-assocs, keep the full structure (with _isList etc.) *)
        If[TrueQ[Lookup[current[key], "_isList", False]],
          current = current[key],
          current = Lookup[current[key], "_children", <||>]
        ],
        (* Path not found, return empty *)
        Return[{}, Module]
      ]
    ],
    {key, keyPath}
  ];
  
  (*
  If we end up at a list level, return numeric indices 1..N plus the merged assoc keys.
  Otherwise return the keys at this level (exclude meta-keys).
  *)
  If[TrueQ[Lookup[current, "_isList", False]],
    (* At a list level: only show numeric indices, not merged association keys.
       The merged keys are accessible after navigating into the list via an index. *)
    Module[{listLen, indices},
      listLen = Lookup[current, "_listLength", 0];
      indices = If[includeIndices && listLen > 0, Table[i, {i, 1, listLen}], {}];
      indices
    ],
    DeleteCases[Keys[current], "_children" | "_isList" | "_listLength" | "_elements"]
  ]
]


(*
Get top-level keys from a hierarchical structure
*)
getTopLevelKeys[structure_Association] :=
  Module[{s},
    If[TrueQ[Lookup[structure, "_isList", False]],
      (* At a list level: only show numeric indices *)
      Module[{listLen, indices},
        listLen = Lookup[structure, "_listLength", 0];
        indices = If[listLen > 0, Table[i, {i, 1, listLen}], {}];
        indices
      ],
      DeleteCases[Keys[structure], "_children" | "_isList" | "_listLength" | "_elements"]
    ]
  ]


(*
Extract all association keys defined in the file
Returns: <| "variableName" -> <| "key1" -> <| "_children" -> ... |>, ... |>, ... |>
The structure is hierarchical to support nested associations.
Also tracks mutations: Part assignment (assoc["key"] = val, assoc[["key"]] = val),
AppendTo, PrependTo, AssociateTo, and functional patterns (var = Append[var, ...], etc.)
*)
extractAssociationKeys[cst_, text_String] :=
Module[{result, assignments, textKeys, mutations},
  
  result = <||>;
  
  (*
  If CST is not available, fall back to text-based extraction
  This handles the case where parsing hasn't completed yet
  *)
  If[cst === Null,
    textKeys = extractAssociationKeysFromText[text];
    Return[textKeys]
  ];
  
  (*
  Find all variable assignments where the value is an association
  Pattern: varName = <| ... |>
  *)
  assignments = Cases[cst,
    BinaryNode[Set | SetDelayed, children_, data_] :> 
      extractAssignmentKeysHierarchical[children],
    Infinity
  ];
  
  (*
  Merge results from direct assignments
  *)
  Do[
    If[assignment =!= None && assignment["variable"] =!= None,
      result[assignment["variable"]] = assignment["structure"]
    ],
    {assignment, assignments}
  ];
  
  (*
  Extract mutations that add/modify keys on existing variables:
  - Part assignment: assoc["key"] = val, assoc[["key"]] = val
  - AppendTo/PrependTo: AppendTo[list, <|...|>]
  - AssociateTo: AssociateTo[assoc, "key" -> val]
  - Functional set patterns: var = Append[var, ...], var = Join[var, ...], etc.
  *)
  mutations = extractMutationsFromCST[cst];
  
  (* Debug: log CST extraction results *)
  Quiet[
    Module[{ds},
      ds = OpenAppend["/tmp/lsp-assoc-debug.log"];
      WriteString[ds, "--- extractAssociationKeys (CST path) ---\n"];
      WriteString[ds, "directKeys=" <> ToString[Keys[result]] <> "\n"];
      WriteString[ds, "cstMutationKeys=" <> ToString[Keys[mutations]] <> "\n"];
      WriteString[ds, "cstMutations=" <> ToString[mutations] <> "\n"];
      Close[ds];
    ]
  ];
  
  result = mergeStructures[result, mutations];
  
  result
]


(*
========================================
Mutation Tracking - CST-based
========================================

Detect operations that mutate existing list/association variables and
extract any new keys they introduce so that autocomplete stays current.
*)

(*
Extract all mutation-introduced keys from the CST.
Returns: <| "variableName" -> <| "key" -> <||>, ... |>, ... |>
*)
extractMutationsFromCST[cst_] :=
Module[{result, partAssignments, mutatingCalls, functionalSets},
  
  result = <||>;
  
  (*
  1. Part assignment: assoc["key"] = val  or  assoc[["key"]] = val
     In the CST these are BinaryNode[Set, ...] where the LHS is a CallNode
     (not a bare LeafNode[Symbol, ...]).
  *)
  partAssignments = Cases[cst,
    BinaryNode[Set, children_List, _] :> 
      extractPartAssignmentKeys[children],
    Infinity
  ];
  
  Do[
    If[pa =!= None,
      result = mergeIntoResult[result, pa["variable"], pa["keyPath"], pa["valueStructure"]]
    ],
    {pa, partAssignments}
  ];
  
  (*
  2. Mutating function calls: AppendTo, PrependTo, AssociateTo, KeyDropFrom
     In the CST these are CallNode[{LeafNode[Symbol, funcName, _]}, GroupNode[GroupSquare, ...], ...]
  *)
  mutatingCalls = Cases[cst,
    CallNode[{LeafNode[Symbol, funcName_String, _]}, 
             GroupNode[GroupSquare, groupChildren_, _], _] /;
      MemberQ[{"AppendTo", "PrependTo", "AssociateTo"}, funcName] :>
      extractMutatingCallKeys[funcName, groupChildren],
    Infinity
  ];
  
  Do[
    If[mc =!= None,
      result = mergeIntoResult[result, mc["variable"], mc["keyPath"], mc["valueStructure"]]
    ],
    {mc, mutatingCalls}
  ];
  
  (*
  3. Functional set patterns: var = Append[var, ...], var = Prepend[var, ...],
     var = Join[var, ...], var = Merge[var, ...], var = Association[var, ...]
     In the CST these are BinaryNode[Set, ...] where the RHS is a CallNode
     with a recognized function name and the first argument matches the LHS variable.
  *)
  functionalSets = Cases[cst,
    BinaryNode[Set, children_List, _] :> 
      extractFunctionalSetKeys[children],
    Infinity
  ];
  
  Do[
    If[fs =!= None,
      result = mergeIntoResult[result, fs["variable"], fs["keyPath"], fs["valueStructure"]]
    ],
    {fs, functionalSets}
  ];
  
  result
]


(*
Helper: get the significant (non-whitespace, non-operator) children from a CST children list.
Returns only the semantically meaningful nodes.
*)
getSignificantChildren[children_List] :=
  Select[children,
    !MatchQ[#, LeafNode[Whitespace | Token`Newline | Token`Spaces | Token`Comment | 
      Token`Equal | Token`ColonEqual | Token`OpenSquare | Token`CloseSquare |
      Token`Comma | Token`OpenCurly | Token`CloseCurly |
      Token`LessBar | Token`BarGreater, _, _]]&
  ]


(*
Extract keys from Part assignment: assoc["key"] = val or assoc[["key"]] = val
The children list is from BinaryNode[Set, children, _].

Returns None if not a Part assignment pattern, or:
  <| "variable" -> name, "keyPath" -> {key1, key2, ...}, "valueStructure" -> <| ... |> |>
*)
extractPartAssignmentKeys[children_List] :=
Module[{sigChildren, lhs, rhs, varName, keyPath, valueStructure},
  
  sigChildren = getSignificantChildren[children];
  If[Length[sigChildren] < 2, Return[None]];
  
  lhs = sigChildren[[1]];
  rhs = sigChildren[[2]];
  
  (*
  LHS must be a CallNode (function-call-like bracket access on a variable).
  CST structure: CallNode[{LeafNode[Symbol, varName, _]}, GroupNode[GroupSquare, groupChildren, _], _]
  This covers both assoc["key"] and assoc[["key"]] since both use GroupSquare in raw CST.
  *)
  If[!MatchQ[lhs, CallNode[_, _, _]], Return[None]];
  
  (*
  Extract the variable name from the head of the CallNode.
  May be nested for chained access: assoc["a"]["b"] = val
  *)
  {varName, keyPath} = extractCallChainVarAndKeys[lhs];
  If[varName === None || Length[keyPath] == 0, Return[None]];
  
  (*
  Extract structure from the RHS value if it's an association or list-of-associations.
  Otherwise just record that these keys exist.
  *)
  valueStructure = extractValueStructure[rhs];
  
  <| "variable" -> varName, "keyPath" -> keyPath, "valueStructure" -> valueStructure |>
]


(*
Recursively extract the base variable name and key path from a
(possibly chained) CallNode representing bracket access.

assoc["key1"]["key2"] in CST is:
  CallNode[
    {CallNode[{LeafNode[Symbol, "assoc", _]}, GroupNode[GroupSquare, [...,"key1",...], _], _]},
    GroupNode[GroupSquare, [...,"key2",...], _], _]

Returns: {varName, {key1, key2, ...}}
*)
extractCallChainVarAndKeys[node_] :=
Module[{head, groupNode, keys, innerVar, innerKeys, thisKey},
  
  If[!MatchQ[node, CallNode[_, _, _]], Return[{None, {}}]];
  
  head = node[[1]]; (* List of head nodes *)
  groupNode = node[[2]]; (* GroupNode with the bracket contents *)
  
  (* Extract the key(s) from this bracket group *)
  thisKey = extractKeysFromGroup[groupNode];
  
  (* Check if the head is a simple variable or another CallNode (chained access) *)
  If[MatchQ[head, {LeafNode[Symbol, varName_String, _]}],
    (* Base case: simple variable *)
    Return[{head[[1, 2]], thisKey}]
  ];
  
  (* Check for chained access: head is {CallNode[...]} *)
  If[MatchQ[head, {CallNode[_, _, _]}],
    {innerVar, innerKeys} = extractCallChainVarAndKeys[head[[1]]];
    Return[{innerVar, Join[innerKeys, thisKey]}]
  ];
  
  {None, {}}
]


(*
Extract keys from a GroupNode[GroupSquare, children, _] (bracket contents).
Handles both single key and comma-separated keys.
Returns a list of key strings/integers.
*)
extractKeysFromGroup[GroupNode[GroupSquare | GroupDoubleBracket, groupChildren_List, _]] :=
Module[{sigChildren, commaChildren, keyNodes},
  
  (* Remove bracket tokens and whitespace *)
  sigChildren = Select[groupChildren,
    !MatchQ[#, LeafNode[Token`OpenSquare | Token`CloseSquare | 
      Token`LongName`LeftDoubleBracket | Token`LongName`RightDoubleBracket |
      Whitespace | Token`Newline | Token`Spaces, _, _]]&
  ];
  
  (* Check for comma-separated keys: InfixNode[Comma, ...] *)
  commaChildren = Cases[sigChildren, InfixNode[Comma, c_, _] :> c, {1}];
  
  If[Length[commaChildren] > 0,
    (* Multiple comma-separated keys *)
    keyNodes = Select[commaChildren[[1]],
      !MatchQ[#, LeafNode[Token`Comma | Whitespace | Token`Newline | Token`Spaces, _, _]]&
    ];
    extractSingleKey /@ keyNodes,
    
    (* Single key *)
    Select[extractSingleKey /@ sigChildren, # =!= None &]
  ]
]

extractKeysFromGroup[_] := {}


(*
Extract a single key value from a CST node.
Returns the key as String, Integer, or symbol name, or None.
*)
extractSingleKey[LeafNode[String, val_String, _]] := StringTrim[val, "\""]
extractSingleKey[LeafNode[Integer, val_String, _]] := FromDigits[val]
extractSingleKey[LeafNode[Symbol, val_String, _]] := val
extractSingleKey[_] := None


(*
Extract the hierarchical structure from a value node (RHS of assignment).
If the value is an association, extract its keys hierarchically.
If it's a list of associations, build a list structure.
Otherwise return <||> (no nested structure).
*)
extractValueStructure[node_] :=
Module[{assocChildren, listChildren},
  
  (* Check for direct association: <| ... |> *)
  If[MatchQ[node, GroupNode[Association, _, _]],
    Return[extractHierarchicalKeys[node[[2]]]]
  ];
  
  (* Check for list: { ... } possibly containing associations *)
  If[MatchQ[node, GroupNode[List, _, _]],
    listChildren = node[[2]];
    Return[extractListValueStructure[listChildren]]
  ];
  
  (* Scalar or unknown value *)
  <||>
]


(*
Extract structure from list children, checking if they contain associations.
*)
extractListValueStructure[listChildren_List] :=
Module[{assocNodes, commaNodes, commaAssocs, mergedKeys, elementKeys},
  
  assocNodes = Cases[listChildren, GroupNode[Association, ac_, _] :> ac, {1}];
  
  commaNodes = Cases[listChildren, InfixNode[Comma, c_, _] :> c, {1}];
  If[Length[commaNodes] > 0,
    commaAssocs = Cases[commaNodes[[1]], GroupNode[Association, ac_, _] :> ac, {1}];
    assocNodes = Join[assocNodes, commaAssocs]
  ];
  
  If[Length[assocNodes] > 0,
    elementKeys = {};
    mergedKeys = <||>;
    Do[
      Module[{keys},
        keys = extractHierarchicalKeys[an];
        AppendTo[elementKeys, keys];
        Do[
          If[!KeyExistsQ[mergedKeys, k],
            mergedKeys[k] = keys[k],
            If[KeyExistsQ[keys[k], "_children"] && KeyExistsQ[mergedKeys[k], "_children"],
              mergedKeys[k] = <| "_children" -> 
                Join[mergedKeys[k]["_children"], keys[k]["_children"]] |>,
              If[KeyExistsQ[keys[k], "_children"],
                mergedKeys[k] = keys[k]
              ]
            ]
          ],
          {k, Keys[keys]}
        ]
      ],
      {an, assocNodes}
    ];
    <| "_isList" -> True, "_listLength" -> Length[assocNodes],
       "_elements" -> elementKeys, "_children" -> mergedKeys |>,
    
    <||>
  ]
]


(*
Extract keys from mutating function calls: AppendTo, PrependTo, AssociateTo.
groupChildren is the contents of the GroupNode[GroupSquare, ...] (the function arguments).

AppendTo[var, value]        -> adds value's keys to var (if value is assoc)
PrependTo[var, value]       -> adds value's keys to var (if value is assoc)
AssociateTo[var, key -> val] -> adds key to var
AssociateTo[var, {k1 -> v1, k2 -> v2}] -> adds k1, k2 to var

Returns None or <| "variable" -> ..., "keyPath" -> {}, "valueStructure" -> <|...|> |>
*)
extractMutatingCallKeys[funcName_String, groupChildren_List] :=
Module[{sigArgs, varName, valueNode, keyPath, valueStructure, rules},
  
  (* Extract significant children: remove brackets, commas, whitespace *)
  sigArgs = Select[groupChildren,
    !MatchQ[#, LeafNode[Token`OpenSquare | Token`CloseSquare | Token`Comma | 
      Whitespace | Token`Newline | Token`Spaces, _, _]]&
  ];
  
  (* Also extract from InfixNode[Comma, ...] which wraps multi-argument calls *)
  If[Length[sigArgs] == 1 && MatchQ[sigArgs[[1]], InfixNode[Comma, _, _]],
    sigArgs = Select[sigArgs[[1, 2]],
      !MatchQ[#, LeafNode[Token`Comma | Whitespace | Token`Newline | Token`Spaces, _, _]]&
    ]
  ];
  
  If[Length[sigArgs] < 2, Return[None]];
  
  (* First argument is the variable *)
  If[!MatchQ[sigArgs[[1]], LeafNode[Symbol, _, _]], Return[None]];
  varName = sigArgs[[1, 2]];
  
  valueNode = sigArgs[[2]];
  
  Switch[funcName,
    "AppendTo" | "PrependTo",
      (*
      AppendTo[var, <|...|>] or AppendTo[var, scalarValue]
      If value is an association, extract its keys.
      The variable is treated as a list-of-associations if it already is one,
      or gets new keys merged if it's a plain association.
      *)
      valueStructure = extractValueStructure[valueNode];
      <| "variable" -> varName, "keyPath" -> {}, "valueStructure" -> valueStructure |>,
    
    "AssociateTo",
      (*
      AssociateTo[var, "key" -> val]
      AssociateTo[var, {"key1" -> val1, "key2" -> val2}]
      AssociateTo[var, <| "key" -> val |>]
      *)
      valueStructure = extractAssociateToKeys[valueNode];
      If[valueStructure === None, Return[None]];
      <| "variable" -> varName, "keyPath" -> {}, "valueStructure" -> valueStructure |>,
    
    _,
      None
  ]
]


(*
Extract keys from the second argument of AssociateTo.
Can be: "key" -> val, {"k1" -> v1, ...}, or <| "k" -> v |>
Returns an association structure or None.
*)
extractAssociateToKeys[node_] :=
Module[{rules, result},
  
  (* Single rule: "key" -> value *)
  If[MatchQ[node, BinaryNode[Rule | RuleDelayed, _, _]],
    result = <||>;
    Module[{key, ruleChildren, valueNode, vs},
      ruleChildren = node[[2]];
      key = extractKeyFromRuleChildren[ruleChildren];
      If[key =!= None,
        (* Check if the value is a nested association *)
        valueNode = Cases[ruleChildren, 
          n_ /; MatchQ[n, GroupNode[Association, _, _] | GroupNode[List, _, _]], {1}];
        If[Length[valueNode] > 0,
          vs = extractValueStructure[valueNode[[1]]];
          If[AssociationQ[vs] && Length[vs] > 0,
            result[key] = <| "_children" -> vs |>,
            result[key] = <||>
          ],
          result[key] = <||>
        ]
      ]
    ];
    Return[result]
  ];
  
  (* Association literal: <| "key" -> val, ... |> *)
  If[MatchQ[node, GroupNode[Association, _, _]],
    Return[extractHierarchicalKeys[node[[2]]]]
  ];
  
  (* List of rules: {"key1" -> val1, "key2" -> val2} *)
  If[MatchQ[node, GroupNode[List, _, _]],
    Module[{listChildren, ruleNodes, commaChildren},
      listChildren = node[[2]];
      
      ruleNodes = Cases[listChildren,
        BinaryNode[Rule | RuleDelayed, rc_, _] :> rc, {1}];
      
      commaChildren = Cases[listChildren, InfixNode[Comma, c_, _] :> c, {1}];
      If[Length[commaChildren] > 0,
        ruleNodes = Join[ruleNodes, Cases[commaChildren[[1]],
          BinaryNode[Rule | RuleDelayed, rc_, _] :> rc, {1}]]
      ];
      
      result = <||>;
      Do[
        Module[{key},
          key = extractKeyFromRuleChildren[rc];
          If[key =!= None,
            result[key] = <||>
          ]
        ],
        {rc, ruleNodes}
      ];
      If[Length[result] > 0, Return[result]]
    ]
  ];
  
  None
]


(*
Extract keys from functional set patterns:
  var = Append[var, value]
  var = Prepend[var, value]
  var = Join[var, otherList]
  var = Merge[var, otherAssoc]
  var = Association[var, key -> val]

children is from BinaryNode[Set, children, _].
Returns None or <| "variable" -> ..., "keyPath" -> {}, "valueStructure" -> <|...|> |>
*)
extractFunctionalSetKeys[children_List] :=
Module[{sigChildren, lhs, rhs, varName, rhsHead, rhsGroupChildren,
        rhsSigArgs, funcName, valueNode, valueStructure},
  
  sigChildren = getSignificantChildren[children];
  If[Length[sigChildren] < 2, Return[None]];
  
  lhs = sigChildren[[1]];
  rhs = sigChildren[[2]];
  
  (* LHS must be a simple variable (not a CallNode like Part assignment) *)
  If[!MatchQ[lhs, LeafNode[Symbol, _, _]], Return[None]];
  varName = lhs[[2]];
  
  (* RHS must be a CallNode: funcName[args...] *)
  If[!MatchQ[rhs, CallNode[_, _, _]], Return[None]];
  
  rhsHead = rhs[[1]];
  If[!MatchQ[rhsHead, {LeafNode[Symbol, _, _]}], Return[None]];
  funcName = rhsHead[[1, 2]];
  
  If[!MemberQ[{"Append", "Prepend", "Join", "Merge", "Association"}, funcName],
    Return[None]
  ];
  
  (* Extract the arguments from the GroupNode *)
  rhsGroupChildren = rhs[[2]];
  If[!MatchQ[rhsGroupChildren, GroupNode[GroupSquare, _, _]], Return[None]];
  
  rhsSigArgs = Select[rhsGroupChildren[[2]],
    !MatchQ[#, LeafNode[Token`OpenSquare | Token`CloseSquare | Token`Comma | 
      Whitespace | Token`Newline | Token`Spaces, _, _]]&
  ];
  
  (* Unwrap InfixNode[Comma, ...] if present *)
  If[Length[rhsSigArgs] == 1 && MatchQ[rhsSigArgs[[1]], InfixNode[Comma, _, _]],
    rhsSigArgs = Select[rhsSigArgs[[1, 2]],
      !MatchQ[#, LeafNode[Token`Comma | Whitespace | Token`Newline | Token`Spaces, _, _]]&
    ]
  ];
  
  If[Length[rhsSigArgs] < 2, Return[None]];
  
  (* First argument should be the same variable name (self-reference) *)
  If[!MatchQ[rhsSigArgs[[1]], LeafNode[Symbol, varName, _]], Return[None]];
  
  Switch[funcName,
    "Append" | "Prepend",
      (* var = Append[var, value] *)
      valueNode = rhsSigArgs[[2]];
      valueStructure = extractValueStructure[valueNode];
      <| "variable" -> varName, "keyPath" -> {}, "valueStructure" -> valueStructure |>,
    
    "Join",
      (* var = Join[var, otherList] - extract keys from all remaining args *)
      valueStructure = <||>;
      Do[
        Module[{vs},
          vs = extractValueStructure[rhsSigArgs[[i]]];
          valueStructure = mergeKeyStructure[valueStructure, vs]
        ],
        {i, 2, Length[rhsSigArgs]}
      ];
      <| "variable" -> varName, "keyPath" -> {}, "valueStructure" -> valueStructure |>,
    
    "Merge" | "Association",
      (* var = Merge[var, other] or var = Association[var, key -> val] *)
      valueStructure = <||>;
      Do[
        Module[{vs},
          vs = extractValueStructure[rhsSigArgs[[i]]];
          valueStructure = mergeKeyStructure[valueStructure, vs]
        ],
        {i, 2, Length[rhsSigArgs]}
      ];
      <| "variable" -> varName, "keyPath" -> {}, "valueStructure" -> valueStructure |>,
    
    _,
      None
  ]
]


(*
Merge a single mutation result into the accumulated result association.
Handles nested key paths (e.g., assoc["a"]["b"] = val adds "b" under "a").

Returns: the modified result association (caller must capture the return value).

result: the accumulated <| var -> structure |>
variable: the variable name
keyPath: list of keys for nested access (e.g., {"a", "b"})
valueStructure: the structure to merge at the target location
*)
mergeIntoResult[result_Association, variable_String, keyPath_List, valueStructure_Association] :=
Module[{merged = result, existing},
  
  existing = Lookup[merged, variable, <||>];
  
  If[Length[keyPath] == 0,
    (* Top-level merge: merge valueStructure keys into existing *)
    merged[variable] = mergeKeyStructure[existing, valueStructure];
    merged
    ,
    (* For nested paths like assoc["a"]["b"] = val,
       build the nested structure and merge it in *)
    merged[variable] = mergeAtKeyPath[existing, keyPath, valueStructure];
    merged
  ]
]

(* No-op for None valueStructure — return result unchanged *)
mergeIntoResult[result_Association, variable_String, keyPath_List, None] := result

(*
Recursively build/merge a value at a nested key path within a key structure.
existing: current structure at this level
keyPath: remaining keys to navigate
valueStructure: the structure to place at the final key
Returns: the updated structure
*)
mergeAtKeyPath[existing_Association, keyPath_List, valueStructure_Association] :=
Module[{merged = existing, key, childrenNow, updatedChildren},
  
  key = First[keyPath];
  
  If[Length[keyPath] == 1,
    (* Final key — merge valueStructure here *)
    If[AssociationQ[valueStructure] && Length[valueStructure] > 0,
      (* Value has nested structure *)
      If[KeyExistsQ[merged, key] && AssociationQ[merged[key]] && KeyExistsQ[merged[key], "_children"],
        merged[key] = <| "_children" -> mergeKeyStructure[merged[key]["_children"], valueStructure] |>,
        merged[key] = <| "_children" -> valueStructure |>
      ],
      (* Scalar value - just ensure key exists *)
      If[!KeyExistsQ[merged, key],
        merged[key] = <||>
      ]
    ];
    merged
    ,
    (* Intermediate key — recurse deeper *)
    If[!KeyExistsQ[merged, key],
      merged[key] = <| "_children" -> <||> |>
    ];
    If[!AssociationQ[merged[key]] || !KeyExistsQ[merged[key], "_children"],
      merged[key] = <| "_children" -> <||> |>
    ];
    childrenNow = merged[key]["_children"];
    updatedChildren = mergeAtKeyPath[childrenNow, Rest[keyPath], valueStructure];
    merged[key] = <| "_children" -> updatedChildren |>;
    merged
  ]
]

(* Fallback for empty keyPath — just merge at top level *)
mergeAtKeyPath[existing_Association, {}, valueStructure_Association] :=
  mergeKeyStructure[existing, valueStructure]


(*
Merge two key structures together, combining _children recursively.
*)
mergeKeyStructure[existing_Association, new_Association] :=
Module[{merged},
  merged = existing;
  Do[
    If[!KeyExistsQ[merged, k],
      merged[k] = new[k],
      (* Both have this key - merge _children if present *)
      If[KeyExistsQ[new[k], "_children"] && KeyExistsQ[merged[k], "_children"],
        merged[k] = <| "_children" -> mergeKeyStructure[merged[k]["_children"], new[k]["_children"]] |>,
        If[KeyExistsQ[new[k], "_children"],
          merged[k] = new[k]
        ]
      ]
    ],
    {k, Keys[new]}
  ];
  merged
]


(*
Merge two top-level result structures (variable -> keys mappings).
*)
mergeStructures[base_Association, additions_Association] :=
Module[{merged},
  merged = base;
  Do[
    If[KeyExistsQ[merged, var],
      merged[var] = mergeKeyStructure[merged[var], additions[var]],
      merged[var] = additions[var]
    ],
    {var, Keys[additions]}
  ];
  merged
]


(*
Extract variable name and hierarchical key structure from an assignment
Returns: <| "variable" -> name, "structure" -> <| key -> <| "_children" -> ... |> |> |>
*)
extractAssignmentKeysHierarchical[children_List] :=
Module[{varName, assocNode, listNode, structure},
  
  (*
  Find the variable name (first Symbol in children)
  *)
  varName = Cases[children, LeafNode[Symbol, name_, _] :> name, {1}];
  
  If[Length[varName] == 0,
    Return[None]
  ];
  
  varName = First[varName];
  
  (*
  Find association node in children - look at direct children only
  *)
  assocNode = Cases[children, GroupNode[Association, assocChildren_, _] :> assocChildren, {1}];
  
  If[Length[assocNode] > 0,
    structure = extractHierarchicalKeys[First[assocNode]];
    Return[<|"variable" -> varName, "structure" -> structure|>]
  ];
  
  (*
  Check for list-of-associations: {<|...|>, <|...|>, ...}
  The CST has GroupNode[List, listChildren, _] containing GroupNode[Association, ...]
  *)
  listNode = Cases[children, GroupNode[List, listChildren_, _] :> listChildren, {1}];
  
  If[Length[listNode] > 0,
    Module[{listChildren, assocNodes, mergedKeys},
      listChildren = First[listNode];
      
      (* Find all associations directly in the list, or inside InfixNode[Comma, ...] *)
      assocNodes = Cases[listChildren, 
        GroupNode[Association, ac_, _] :> ac, {1}];
      
      (* Also look inside Comma nodes *)
      Module[{commaNodes, commaAssocs},
        commaNodes = Cases[listChildren, InfixNode[Comma, c_, _] :> c, {1}];
        If[Length[commaNodes] > 0,
          commaAssocs = Cases[commaNodes[[1]], 
            GroupNode[Association, ac_, _] :> ac, {1}];
          assocNodes = Join[assocNodes, commaAssocs]
        ]
      ];
      
      If[Length[assocNodes] > 0,
        (* Extract per-element keys and build merged keys *)
        Module[{elementKeys},
          elementKeys = {};
          mergedKeys = <||>;
          Do[
            Module[{keys},
              keys = extractHierarchicalKeys[an];
              AppendTo[elementKeys, keys];
              Do[
                If[!KeyExistsQ[mergedKeys, k],
                  mergedKeys[k] = keys[k],
                  (* Merge _children if both have them *)
                  If[KeyExistsQ[keys[k], "_children"] && KeyExistsQ[mergedKeys[k], "_children"],
                    mergedKeys[k] = <| "_children" -> 
                      Join[mergedKeys[k]["_children"], keys[k]["_children"]] |>,
                    If[KeyExistsQ[keys[k], "_children"],
                      mergedKeys[k] = keys[k]
                    ]
                  ]
                ],
                {k, Keys[keys]}
              ]
            ],
            {an, assocNodes}
          ];
          structure = <| "_isList" -> True, "_listLength" -> Length[assocNodes], 
                         "_elements" -> elementKeys,
                         "_children" -> mergedKeys |>;
          Return[<|"variable" -> varName, "structure" -> structure|>]
        ]
      ]
    ]
  ];
  
  None
]


(*
Extract hierarchical key structure from association children
Returns: <| "key1" -> <| "_children" -> <| nested... |> |>, "key2" -> <||>, ... |>
*)
extractHierarchicalKeys[children_List] :=
Module[{result, rules, commaChildren},
  
  result = <||>;
  
  (*
  Find all Rule nodes at this level
  Rules can be either:
  1. Direct children (single key-value pair): <| "key" -> value |>
  2. Inside InfixNode[Comma, ...] (multiple pairs): <| "k1" -> v1, "k2" -> v2 |>
  *)
  
  (* First, try to find rules directly *)
  rules = Cases[children,
    BinaryNode[Rule | RuleDelayed, ruleChildren_, _] :> ruleChildren,
    {1}
  ];
  
  (* Also look for rules inside InfixNode[Comma, ...] for multi-key associations *)
  commaChildren = Cases[children, InfixNode[Comma, c_, _] :> c, {1}];
  If[Length[commaChildren] > 0,
    rules = Join[rules, Cases[commaChildren[[1]],
      BinaryNode[Rule | RuleDelayed, ruleChildren_, _] :> ruleChildren,
      {1}
    ]]
  ];
  
  Do[
    Module[{key, valueNode, childStructure, listNode},
      (* Extract the key *)
      key = extractKeyFromRuleChildren[rule];
      
      If[key =!= None,
        (* Check if the value is a nested association - only look at direct children *)
        valueNode = Cases[rule, GroupNode[Association, assocChildren_, _] :> assocChildren, {1}];
        
        If[Length[valueNode] > 0,
          (* Recursively extract nested keys *)
          childStructure = extractHierarchicalKeys[First[valueNode]];
          result[key] = <| "_children" -> childStructure |>,
          
          (* Check if value is a list containing associations: {<|...|>, ...} *)
          listNode = Cases[rule, GroupNode[List, listChildren_, _] :> listChildren, {1}];
          If[Length[listNode] > 0,
            Module[{lc, assocNodes, mergedKeys, numAssocs},
              lc = First[listNode];
              (* Find associations directly or inside Comma *)
              assocNodes = Cases[lc, GroupNode[Association, ac_, _] :> ac, {1}];
              Module[{commaNodes2, commaAssocs2},
                commaNodes2 = Cases[lc, InfixNode[Comma, c_, _] :> c, {1}];
                If[Length[commaNodes2] > 0,
                  commaAssocs2 = Cases[commaNodes2[[1]], 
                    GroupNode[Association, ac_, _] :> ac, {1}];
                  assocNodes = Join[assocNodes, commaAssocs2]
                ]
              ];
              numAssocs = Length[assocNodes];
              If[numAssocs > 0,
                Module[{elementKeys2},
                  elementKeys2 = {};
                  mergedKeys = <||>;
                  Do[
                    Module[{akeys},
                      akeys = extractHierarchicalKeys[an];
                      AppendTo[elementKeys2, akeys];
                      Do[
                        If[!KeyExistsQ[mergedKeys, k],
                          mergedKeys[k] = akeys[k],
                          If[KeyExistsQ[akeys[k], "_children"] && KeyExistsQ[mergedKeys[k], "_children"],
                            mergedKeys[k] = <| "_children" -> 
                              Join[mergedKeys[k]["_children"], akeys[k]["_children"]] |>,
                            If[KeyExistsQ[akeys[k], "_children"],
                              mergedKeys[k] = akeys[k]
                            ]
                          ]
                        ],
                        {k, Keys[akeys]}
                      ]
                    ],
                    {an, assocNodes}
                  ];
                  result[key] = <| "_isList" -> True, "_listLength" -> numAssocs,
                                    "_elements" -> elementKeys2,
                                    "_children" -> mergedKeys |>
                ],
                (* List without associations *)
                result[key] = <||>
              ]
            ],
            (* No nested association or list *)
            result[key] = <||>
          ]
        ]
      ]
    ],
    {rule, rules}
  ];
  
  result
]


(*
Extract the key from a Rule's children
*)
extractKeyFromRuleChildren[ruleChildren_List] :=
Module[{firstNode},
  
  (*
  The key is always the FIRST child of the Rule node (before the ->).
  We must pick the first non-whitespace, non-operator LeafNode.
  Using First avoids accidentally matching the value side of the rule.
  *)
  firstNode = FirstCase[ruleChildren,
    LeafNode[type_, val_, _] /; !MemberQ[{Token`MinusGreater, Token`Comma, 
      Token`Equal, Whitespace, Token`Newline}, type] :> {type, val},
    None
  ];
  
  If[firstNode === None, Return[None]];
  
  Which[
    firstNode[[1]] === String,
      StringTrim[firstNode[[2]], "\""],
    firstNode[[1]] === Integer,
      FromDigits[firstNode[[2]]],
    firstNode[[1]] === Symbol,
      firstNode[[2]],
    True,
      ToString[firstNode[[2]]]
  ]
]


(*
Text-based fallback for extracting association keys when CST is unavailable.
Uses bracket-depth-aware parsing rather than regex to correctly handle nesting.
Also detects mutation patterns: Part assignment, AppendTo, AssociateTo, etc.
*)
extractAssociationKeysFromText[text_String] :=
Module[{result, chars, pos, len, varName, mutations},
  
  result = <||>;
  chars = Characters[text];
  len = Length[chars];
  pos = 1;
  
  (*
  Scan for patterns: varName = <| ... |>
  Use bracket-depth tracking to find the matching |>
  *)
  While[pos <= len,
    Module[{varStart, varEnd, assocStart, assocEnd},
      (* Skip to next potential variable name *)
      While[pos <= len && !LetterQ[chars[[pos]]] && chars[[pos]] =!= "$",
        pos++
      ];
      If[pos > len, Break[]];
      
      varStart = pos;
      While[pos <= len && (LetterQ[chars[[pos]]] || DigitQ[chars[[pos]]] || chars[[pos]] === "$"),
        pos++
      ];
      varEnd = pos - 1;
      varName = StringJoin[chars[[varStart ;; varEnd]]];
      
      (* Skip whitespace *)
      While[pos <= len && MemberQ[{" ", "\t", "\n", "\r"}, chars[[pos]]],
        pos++
      ];
      
      (* Check for = *)
      If[pos > len || chars[[pos]] =!= "=",
        Continue[]
      ];
      pos++;
      
      (* Check it's not == *)
      If[pos <= len && chars[[pos]] === "=",
        Continue[]
      ];
      
      (* Skip whitespace *)
      While[pos <= len && MemberQ[{" ", "\t", "\n", "\r"}, chars[[pos]]],
        pos++
      ];
      
      (* Check for <| (direct association) or { (list that may contain associations) *)
      If[pos + 1 <= len && chars[[pos]] === "<" && chars[[pos + 1]] === "|",
        (* Direct association: varName = <| ... |> *)
        pos += 2;
        assocStart = pos;
        
        (* Find matching |> using bracket depth *)
        assocEnd = findMatchingAssocClose[text, pos];
        If[assocEnd == -1,
          Continue[]
        ];
        
        result[varName] = parseAssociationBody[text, assocStart, assocEnd - 1];
        pos = assocEnd + 2,
        
        (* Check for list: varName = { ... } *)
        If[pos <= len && chars[[pos]] === "{",
          pos++;
          Module[{listStart, listEnd, listDepth, listInStr, listCh,
                  mergedKeys, assocBodyStart, assocBodyEnd},
            listStart = pos;
            (* Find matching } using bracket depth *)
            listDepth = 1;
            listEnd = -1;
            listInStr = False;
            While[pos <= len && listDepth > 0,
              listCh = chars[[pos]];
              If[listInStr,
                If[listCh === "\"", listInStr = False],
                Which[
                  listCh === "\"", listInStr = True,
                  listCh === "{", listDepth++,
                  listCh === "}", listDepth--; If[listDepth == 0, listEnd = pos - 1],
                  True, Null
                ]
              ];
              pos++
            ];
            If[listEnd == -1, Continue[]];
            
            (* Now scan inside the list for <| ... |> associations *)
            mergedKeys = <||>;
            Module[{scanPos, numAssocsText = 0, elementKeysText = {}},
              scanPos = listStart;
              While[scanPos <= listEnd,
                (* Skip to next <| *)
                While[scanPos < listEnd && !(chars[[scanPos]] === "<" && chars[[scanPos + 1]] === "|"),
                  scanPos++
                ];
                If[scanPos >= listEnd, Break[]];
                scanPos += 2;
                assocBodyStart = scanPos;
                assocBodyEnd = findMatchingAssocClose[text, scanPos];
                If[assocBodyEnd == -1, Break[]];
                numAssocsText++;
                
                (* Parse and merge keys *)
                Module[{bodyKeys},
                  bodyKeys = parseAssociationBody[text, assocBodyStart, assocBodyEnd - 1];
                  AppendTo[elementKeysText, bodyKeys];
                  Do[
                    If[!KeyExistsQ[mergedKeys, k],
                      mergedKeys[k] = bodyKeys[k],
                      If[KeyExistsQ[bodyKeys[k], "_children"] && KeyExistsQ[mergedKeys[k], "_children"],
                        mergedKeys[k] = <| "_children" -> 
                          Join[mergedKeys[k]["_children"], bodyKeys[k]["_children"]] |>,
                        If[KeyExistsQ[bodyKeys[k], "_children"],
                          mergedKeys[k] = bodyKeys[k]
                        ]
                      ]
                    ],
                    {k, Keys[bodyKeys]}
                  ]
                ];
                scanPos = assocBodyEnd + 2
              ];
              
              If[Length[mergedKeys] > 0,
                result[varName] = <| "_isList" -> True, "_listLength" -> numAssocsText,
                                     "_elements" -> elementKeysText,
                                     "_children" -> mergedKeys |>
              ]
            ]
          ],
          (* Neither <| nor { — skip *)
          Continue[]
        ]
      ]
    ]
  ];
  
  (*
  Also extract mutations from text: Part assignment, AppendTo, AssociateTo, etc.
  *)
  mutations = extractMutationsFromText[text];
  
  (* Debug: log text extraction results before merge *)
  Quiet[
    Module[{ds},
      ds = OpenAppend["/tmp/lsp-assoc-debug.log"];
      WriteString[ds, "--- extractAssociationKeysFromText ---\n"];
      WriteString[ds, "directKeys=" <> ToString[Keys[result]] <> "\n"];
      WriteString[ds, "mutationKeys=" <> ToString[Keys[mutations]] <> "\n"];
      Close[ds];
    ]
  ];
  
  result = mergeStructures[result, mutations];
  
  result
]


(*
========================================
Text-based Mutation Extraction
========================================

Detects mutation patterns in raw text when CST is not available.
Covers: Part assignment, AppendTo, PrependTo, AssociateTo, and
functional patterns (var = Append[var, ...], etc.)
*)

(*
Extract mutation-introduced keys from raw text.
Returns: <| "variable" -> <| "key" -> <||>, ... |>, ... |>
*)
extractMutationsFromText[text_String] :=
Module[{result, partKeys, funcKeys},
  
  result = <||>;
  
  (* Debug: log that we entered extractMutationsFromText *)
  Quiet[
    Module[{ds},
      ds = OpenAppend["/tmp/lsp-assoc-debug.log"];
      WriteString[ds, "--- extractMutationsFromText entered ---\n"];
      WriteString[ds, "textLength=" <> ToString[StringLength[text]] <> "\n"];
      Close[ds];
    ]
  ];
  
  (*
  1. Part assignment: var["key"] = val  or  var[["key"]] = val
     Regex: identifier followed by [ or [[ then "string" then ] or ]] then = (not ==)
  *)
  partKeys = StringCases[text,
    RegularExpression["([a-zA-Z$][a-zA-Z0-9$]*)\\s*\\[\\[?\\s*\"([^\"]*)\"\\s*\\]\\]?\\s*=(?!=)"] :>
      {"$1", "$2"}
  ];
  
  Quiet[
    Module[{ds},
      ds = OpenAppend["/tmp/lsp-assoc-debug.log"];
      WriteString[ds, "partKeys=" <> ToString[partKeys] <> "\n"];
      Close[ds];
    ]
  ];
  
  Do[
    Module[{var, key},
      var = pk[[1]];
      key = pk[[2]];
      If[!KeyExistsQ[result, var],
        result[var] = <||>
      ];
      result[var][key] = <||>
    ],
    {pk, partKeys}
  ];
  
  (*
  2. AppendTo[var, <| ... |>] and PrependTo[var, <| ... |>]
     Extract the variable name and any association keys from the value argument.
  *)
  Module[{appendMatches},
    appendMatches = StringCases[text,
      RegularExpression["(AppendTo|PrependTo)\\s*\\[\\s*([a-zA-Z$][a-zA-Z0-9$]*)\\s*,"] :>
        {"$2"}
    ];
    
    (* For each AppendTo/PrependTo, just register the variable exists.
       The value might be an association — try to extract keys from following text. *)
    Do[
      Module[{var},
        var = am[[1]];
        If[!KeyExistsQ[result, var],
          result[var] = <||>
        ]
      ],
      {am, appendMatches}
    ];
    
    (* More precise: find AppendTo[var, <| "key" -> val |>] *)
    Module[{appendAssocMatches},
      appendAssocMatches = StringCases[text,
        RegularExpression["(?:AppendTo|PrependTo)\\s*\\[\\s*([a-zA-Z$][a-zA-Z0-9$]*)\\s*,\\s*<\\|([^|]*(?:\\|[^>][^|]*)*)\\|>"] :>
          {"$1", "$2"}
      ];
      Do[
        Module[{var, body, bodyKeys},
          var = aam[[1]];
          body = aam[[2]];
          (* Parse simple "key" -> patterns from the body *)
          bodyKeys = StringCases[body,
            RegularExpression["\"([^\"]*)\"\\s*->"] :> "$1"
          ];
          If[!KeyExistsQ[result, var],
            result[var] = <||>
          ];
          Do[
            result[var][bk] = <||>,
            {bk, bodyKeys}
          ]
        ],
        {aam, appendAssocMatches}
      ]
    ]
  ];
  
  (*
  3. AssociateTo[var, "key" -> val] and AssociateTo[var, {"key1" -> v1, ...}]
  *)
  Module[{assocToMatches},
    (* Simple single rule: AssociateTo[var, "key" -> val] *)
    assocToMatches = StringCases[text,
      RegularExpression["AssociateTo\\s*\\[\\s*([a-zA-Z$][a-zA-Z0-9$]*)\\s*,\\s*\"([^\"]*)\"\\s*->"] :>
        {"$1", "$2"}
    ];
    Do[
      Module[{var, key},
        var = atm[[1]];
        key = atm[[2]];
        If[!KeyExistsQ[result, var],
          result[var] = <||>
        ];
        result[var][key] = <||>
      ],
      {atm, assocToMatches}
    ];
    
    (* AssociateTo with list of rules: AssociateTo[var, {"key1" -> ..., "key2" -> ...}] *)
    Module[{assocToListMatches},
      assocToListMatches = StringCases[text,
        RegularExpression["AssociateTo\\s*\\[\\s*([a-zA-Z$][a-zA-Z0-9$]*)\\s*,\\s*\\{([^}]*)\\}"] :>
          {"$1", "$2"}
      ];
      Do[
        Module[{var, body, bodyKeys},
          var = atlm[[1]];
          body = atlm[[2]];
          bodyKeys = StringCases[body,
            RegularExpression["\"([^\"]*)\"\\s*->"] :> "$1"
          ];
          If[!KeyExistsQ[result, var],
            result[var] = <||>
          ];
          Do[
            result[var][bk] = <||>,
            {bk, bodyKeys}
          ]
        ],
        {atlm, assocToListMatches}
      ]
    ];
    
    (* AssociateTo with association literal: AssociateTo[var, <| "key" -> val |>] *)
    Module[{assocToAssocMatches},
      assocToAssocMatches = StringCases[text,
        RegularExpression["AssociateTo\\s*\\[\\s*([a-zA-Z$][a-zA-Z0-9$]*)\\s*,\\s*<\\|([^|]*(?:\\|[^>][^|]*)*)\\|>"] :>
          {"$1", "$2"}
      ];
      Do[
        Module[{var, body, bodyKeys},
          var = atam[[1]];
          body = atam[[2]];
          bodyKeys = StringCases[body,
            RegularExpression["\"([^\"]*)\"\\s*->"] :> "$1"
          ];
          If[!KeyExistsQ[result, var],
            result[var] = <||>
          ];
          Do[
            result[var][bk] = <||>,
            {bk, bodyKeys}
          ]
        ],
        {atam, assocToAssocMatches}
      ]
    ]
  ];
  
  (*
  4. Functional set patterns: var = Append[var, ...], var = Prepend[var, ...],
     var = Join[var, ...], var = Merge[var, ...]
     We detect these and try to extract association keys from the value argument.
  *)
  Module[{funcSetMatches},
    (* Match: var = Func[var, <| "key" -> val |>] *)
    funcSetMatches = StringCases[text,
      RegularExpression["([a-zA-Z$][a-zA-Z0-9$]*)\\s*=\\s*(?:Append|Prepend)\\s*\\[\\s*\\1\\s*,\\s*<\\|([^|]*(?:\\|[^>][^|]*)*)\\|>"] :>
        {"$1", "$2"}
    ];
    Do[
      Module[{var, body, bodyKeys},
        var = fsm[[1]];
        body = fsm[[2]];
        bodyKeys = StringCases[body,
          RegularExpression["\"([^\"]*)\"\\s*->"] :> "$1"
        ];
        If[!KeyExistsQ[result, var],
          result[var] = <||>
        ];
        Do[
          result[var][bk] = <||>,
          {bk, bodyKeys}
        ]
      ],
      {fsm, funcSetMatches}
    ]
  ];
  
  (* Debug: log final mutation result *)
  Quiet[
    Module[{ds},
      ds = OpenAppend["/tmp/lsp-assoc-debug.log"];
      WriteString[ds, "mutationsFromText=" <> ToString[result] <> "\n"];
      WriteString[ds, "--- extractMutationsFromText done ---\n"];
      Close[ds];
    ]
  ];
  
  result
]


(*
Find the position of |> that matches the <| at the given position.
Returns the position of | in |>, or -1 if not found.
Uses character list to avoid StringTake overhead and escape issues.
*)
findMatchingAssocClose[text_String, startPos_Integer] :=
Module[{chars, pos, len, depth, inStr, ch, nextCh},
  chars = Characters[text];
  len = Length[chars];
  pos = startPos;
  depth = 1;
  inStr = False;
  
  While[pos <= len && depth > 0,
    ch = chars[[pos]];
    
    If[inStr,
      (* Inside a string literal - just look for the closing quote *)
      If[ch === "\"", inStr = False],
      (* Not in string *)
      nextCh = If[pos < len, chars[[pos + 1]], ""];
      Which[
        ch === "\"",
          inStr = True,
        ch === "<" && nextCh === "|",
          depth++; pos++,
        ch === "|" && nextCh === ">",
          depth--;
          If[depth == 0, Return[pos, Module]];
          pos++,
        True,
          Null
      ]
    ];
    pos++
  ];
  
  -1
]


(*
Parse the body of an association between <| and |> into hierarchical keys.
Operates on character positions within the full text.
Uses Characters list for efficiency and to avoid escape issues.
*)
parseAssociationBody[text_String, startPos_Integer, endPos_Integer] :=
Module[{result, chars, pos, key, valueStart, valueEnd, depth, inStr, ch},
  
  result = <||>;
  chars = Characters[text];
  pos = startPos;
  
  While[pos <= endPos,
    (* Skip whitespace and commas *)
    While[pos <= endPos && MemberQ[{" ", "\t", "\n", "\r", ","}, chars[[pos]]],
      pos++
    ];
    If[pos > endPos, Break[]];
    
    (* Try to read a key: "string", integer, or symbol *)
    If[chars[[pos]] === "\"",
      (* String key *)
      pos++;  (* past opening " *)
      key = "";
      While[pos <= endPos && chars[[pos]] =!= "\"",
        key = key <> chars[[pos]];
        pos++
      ];
      If[pos > endPos, Break[]];
      pos++,  (* past closing " *)
      
      (* Non-string key: integer or symbol *)
      If[DigitQ[chars[[pos]]] || LetterQ[chars[[pos]]] || chars[[pos]] === "$",
        key = "";
        While[pos <= endPos && (DigitQ[chars[[pos]]] || LetterQ[chars[[pos]]] || chars[[pos]] === "$"),
          key = key <> chars[[pos]];
          pos++
        ],
        (* Unknown character - skip to next comma or end *)
        While[pos <= endPos && chars[[pos]] =!= ",",
          pos++
        ];
        Continue[]
      ]
    ];
    
    (* Skip whitespace *)
    While[pos <= endPos && MemberQ[{" ", "\t", "\n", "\r"}, chars[[pos]]],
      pos++
    ];
    
    (* Expect -> *)
    If[pos + 1 > endPos || chars[[pos]] =!= "-" || chars[[pos + 1]] =!= ">",
      Continue[]
    ];
    pos += 2;
    
    (* Skip whitespace *)
    While[pos <= endPos && MemberQ[{" ", "\t", "\n", "\r"}, chars[[pos]]],
      pos++
    ];
    
    (* Read the value *)
    If[pos + 1 <= endPos && chars[[pos]] === "<" && chars[[pos + 1]] === "|",
      (* Nested association *)
      pos += 2;
      valueStart = pos;
      valueEnd = findMatchingAssocClose[text, pos];
      If[valueEnd == -1,
        result[key] = <||>;
        Break[],
        result[key] = <| "_children" -> parseAssociationBody[text, valueStart, valueEnd - 1] |>;
        pos = valueEnd + 2
      ],
      
      (* Check for list value: { ... } which may contain associations *)
      If[pos <= endPos && chars[[pos]] === "{",
        Module[{listStart, listEnd2, listDepth2, listInStr2, listCh2,
                mergedKeys2, assocBodyStart2, assocBodyEnd2, numAssocs2, elementKeys3 = {}},
          pos++;
          listStart = pos;
          listDepth2 = 1;
          listEnd2 = -1;
          listInStr2 = False;
          While[pos <= endPos && listDepth2 > 0,
            listCh2 = chars[[pos]];
            If[listInStr2,
              If[listCh2 === "\"", listInStr2 = False],
              Which[
                listCh2 === "\"", listInStr2 = True,
                listCh2 === "{", listDepth2++,
                listCh2 === "}", listDepth2--; If[listDepth2 == 0, listEnd2 = pos - 1],
                True, Null
              ]
            ];
            pos++
          ];
          If[listEnd2 >= listStart,
            mergedKeys2 = <||>;
            numAssocs2 = 0;
            Module[{scanPos2},
              scanPos2 = listStart;
              While[scanPos2 <= listEnd2,
                While[scanPos2 < listEnd2 && !(chars[[scanPos2]] === "<" && chars[[scanPos2 + 1]] === "|"),
                  scanPos2++
                ];
                If[scanPos2 >= listEnd2, Break[]];
                scanPos2 += 2;
                assocBodyStart2 = scanPos2;
                assocBodyEnd2 = findMatchingAssocClose[text, scanPos2];
                If[assocBodyEnd2 == -1, Break[]];
                numAssocs2++;
                Module[{bodyKeys2},
                  bodyKeys2 = parseAssociationBody[text, assocBodyStart2, assocBodyEnd2 - 1];
                  AppendTo[elementKeys3, bodyKeys2];
                  Do[
                    If[!KeyExistsQ[mergedKeys2, k],
                      mergedKeys2[k] = bodyKeys2[k],
                      If[KeyExistsQ[bodyKeys2[k], "_children"] && KeyExistsQ[mergedKeys2[k], "_children"],
                        mergedKeys2[k] = <| "_children" -> 
                          Join[mergedKeys2[k]["_children"], bodyKeys2[k]["_children"]] |>,
                        If[KeyExistsQ[bodyKeys2[k], "_children"],
                          mergedKeys2[k] = bodyKeys2[k]
                        ]
                      ]
                    ],
                    {k, Keys[bodyKeys2]}
                  ]
                ];
                scanPos2 = assocBodyEnd2 + 2
              ]
            ];
            If[Length[mergedKeys2] > 0,
              result[key] = <| "_isList" -> True, "_listLength" -> numAssocs2,
                               "_elements" -> elementKeys3,
                               "_children" -> mergedKeys2 |>,
              result[key] = <||>
            ],
            result[key] = <||>
          ]
        ],
        
        (* Scalar value - skip to next comma or end, respecting nesting *)
        result[key] = <||>;
        depth = 0;
        inStr = False;
        While[pos <= endPos,
          ch = chars[[pos]];
          If[inStr,
            If[ch === "\"", inStr = False],
            Which[
              ch === "\"", inStr = True,
              ch === "," && depth == 0, Break[],
              MemberQ[{"(", "{", "[", "<"}, ch], depth++,
              MemberQ[{")", "}", "]", ">"}, ch], depth--,
              True, Null
            ]
          ];
          pos++
        ]
      ]
    ]
  ];
  
  result
]


(*
========================================
End Association Key Completion + Mutation Tracking
========================================
*)


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
