BeginPackage["LSPServer`Hover`"]

linearToMDSyntax

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`PacletIndex`"]
Needs["LSPServer`ReplaceLongNamePUA`"]
Needs["LSPServer`Utils`"]
Needs["CodeFormatter`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]

(* 

  Line width after which new line will be inserted for Hover function definition pattern.
   
  The default line width is 78, which unnecessarily breaks the line in Hover function 
  definition pattern after 78 characters. We find many function definition patterns
  are usually more than 78 characters. So, we are setting the line width to 200.

  see bug #449934

*)
$HoverLineWidth = 200;


(*
Cached Association sets for O(1) symbol category lookup.
The underlying lists are loaded at startup in LSPServer.wl.
*)
$undocumentedSet := $undocumentedSet = Association[Thread[
  WolframLanguageSyntax`Generate`$undocumentedSymbols -> True
]]

$experimentalSet := $experimentalSet = Association[Thread[
  WolframLanguageSyntax`Generate`$experimentalSymbols -> True
]]

$obsoleteSet := $obsoleteSet = Association[Thread[
  WolframLanguageSyntax`Generate`$obsoleteSymbols -> True
]]


expandContent[content:KeyValuePattern["method" -> "textDocument/hover"], pos_] :=
Catch[
Module[{params, id, doc, uri, res},

  
  log[1, "textDocument/hover: enter expand"];

  
  id = content["id"];
  params = content["params"];
  
  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "canceled"];
  
    Throw[{<| "method" -> "textDocument/hoverFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    log[2, "stale"];

    Throw[{<| "method" -> "textDocument/hoverFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  res = <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
      "textDocument/concreteParse",
      "textDocument/aggregateParse",
      "textDocument/abstractParse",
      "textDocument/hoverFencepost"
  };

  log[1, "textDocument/hover: exit"];

  res

]]
  
handleContent[content:KeyValuePattern["method" -> "textDocument/hoverFencepost"]] :=
Catch[
Module[{id, params, doc, uri, position, entry, text, textLines, strs, line, char, pre, ast, cstTabs, syms, toks, nums,
  res},

  
  log[1, "textDocument/hoverFencepost: enter"];


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


  log[2, "hover: before parse"];

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];
  
  text = entry["Text"];
  ast = entry["AST"];
  cstTabs = Lookup[entry, "CSTTabs", Null];

  If[cstTabs === Null,
    (*
    Using "TabWidth" -> 4 here because the notification is rendered down to HTML and tabs need to be expanded in HTML
    FIXME: Must use the tab width from the editor
    *)
    cstTabs = CodeConcreteParse[text, "TabWidth" -> 4];
    
    entry["CSTTabs"] = cstTabs;

    $OpenFilesMap[uri] = entry
  ];

  log[2, "hover: after parse"];

  If[StringContainsQ[text, "\t"],
    (*
    Adjust the hover position to accommodate tab stops
    FIXME: Must use the tab width from the editor
    *)
    textLines = StringSplit[text, {"\r\n", "\n", "\r"}, All];
    pre = StringTake[textLines[[line]], char-1];
    char = 1;
    Scan[(If[# == "\t", char = (4 * Quotient[char, 4] + 1) + 4, char++])&, Characters[pre]];
  ];


  log[2, "hover: before finding position"];

  toks = Cases[cstTabs,
  LeafNode[_, _,
    KeyValuePattern[Source -> src_ /; SourceMemberQ[src, {line, char}]]], Infinity];

  log[2, "hover: after finding position"];

  strs = Cases[toks, LeafNode[String, _, _], Infinity];

  syms = Cases[toks, LeafNode[Symbol, _, _], Infinity];

  nums = Cases[toks, LeafNode[Integer | Real | Rational, _, _], Infinity];

  res =
    Which[
      strs != {},
        handleStrings[id, strs, line]
      ,
      syms != {},
        handleSymbols[id, ast, cstTabs, syms]
      ,
      nums != {},
        handleNumbers[id, nums]
      ,
      True,
        {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}
    ];

  
  log[1, "hover hoverFencepost: exit"];
  

  res
]]


(*
For strings that contain \[] or \: notation, display the decoded string
*)
handleStrings[id_, strsIn_, positionLine_] :=
Catch[
Module[{lines, lineMap, originalLineNumber, line,
  originalColumn, rules, decoded, rule, segment1, index, result, segments,
  originalColumnCount, strs},

  (*
  Find strings with multi-SourceCharacter WLCharacters
  *)
  strs = Cases[strsIn, LeafNode[String, str_ /; containsUnicodeCharacterQ[str], _], Infinity];

  lineMap = <||>;

  Function[{str},

    {originalLineNumber, originalColumn} = str[[3, Key[Source], 1]];

    segments = StringSplit[str[[2]], {"\r\n", "\n", "\r"}, All];

    If[Length[segments] == 1,

      segment1 = segments[[1]];

      rules = {};
      
      decoded = convertSegment[segment1];

      (*
      Handle tab stops
      FIXME: Must use the tab width from the editor
      *)
      originalColumnCount = 1;
      Scan[(
        If[# == "\t", originalColumnCount = (4 * Quotient[originalColumnCount, 4] + 1) + 4, originalColumnCount++];)&
        ,
        Characters[decoded]
      ];

      If[!FailureQ[decoded],
        index = 1;
        Function[{char},
          Switch[char,
            "\t",
              index = (4 * Quotient[index, 4] + 1) + 4
            ,
            " ",
              index++
            ,
            _,
              rule = index -> char;
              AppendTo[rules, rule];
              index++
          ]
        ] /@ Characters[decoded]
      ];

      If[rules != {},

        line = <| "line" -> originalLineNumber, "characters" -> ReplacePart[Table[" ", {originalColumnCount + 1}], rules] |>;

        If[KeyExistsQ[lineMap, line["line"]],
          lineMap[line["line"]] = merge[lineMap[line["line"]], line]
          ,
          lineMap[line["line"]] = line
        ];
      ]

      ,
      (* Length[segments] > 1 *)

      MapIndexed[Function[{segment, segmentIndex},

        rules = {};
        Which[
          (positionLine == (originalLineNumber + segmentIndex[[1]] - 1)) && containsUnicodeCharacterQ[segment] && segmentIndex == {1},
            decoded = convertStartingSegment[segment];
            If[!FailureQ[decoded],

              (*
              Handle tab stops
              FIXME: Must use the tab width from the editor
              *)
              originalColumnCount = 1;
              Scan[(
                If[# == "\t", originalColumnCount = (4 * Quotient[originalColumnCount, 4] + 1) + 4, originalColumnCount++];)&
                ,
                Characters[decoded]
              ];

              index = 1;
              Function[{char},
                Switch[char,
                  "\t",
                    index = (4 * Quotient[index, 4] + 1) + 4
                  ,
                  " ",
                    index++
                  ,
                  _,
                    rule = index -> char;
                    AppendTo[rules, rule];
                    index++
                ];
              ] /@ Characters[decoded]
            ];
          ,
          (positionLine == (originalLineNumber + segmentIndex[[1]] - 1)) && containsUnicodeCharacterQ[segment] && segmentIndex == {Length[segments]},
            decoded = convertEndingSegment[segment];
            If[!FailureQ[decoded],

              (*
              Handle tab stops
              FIXME: Must use the tab width from the editor
              *)
              originalColumnCount = 1;
              Scan[(
                If[# == "\t", originalColumnCount = (4 * Quotient[originalColumnCount, 4] + 1) + 4, originalColumnCount++];)&
                ,
                Characters[decoded]
              ];

              index = 1;
              Function[{char},
                Switch[char,
                  "\t",
                    index = (4 * Quotient[index, 4] + 1) + 4
                  ,
                  " ",
                    index++
                  ,
                  _,
                    rule = index -> char;
                    AppendTo[rules, rule];
                    index++
                ];
              ] /@ Characters[decoded]
            ];
          ,
          (positionLine == (originalLineNumber + segmentIndex[[1]] - 1)) && containsUnicodeCharacterQ[segment],
            decoded = convertMiddleSegment[segment];
            If[!FailureQ[decoded],

              (*
              Handle tab stops
              FIXME: Must use the tab width from the editor
              *)
              originalColumnCount = 1;
              Scan[(
                If[# == "\t", originalColumnCount = (4 * Quotient[originalColumnCount, 4] + 1) + 4, originalColumnCount++];)&
                ,
                Characters[decoded]
              ];

              index = 1;
              Function[{char},
                Switch[char,
                  "\t",
                    index = (4 * Quotient[index, 4] + 1) + 4
                  ,
                  " ",
                    index++
                  ,
                  _,
                    rule = index -> char;
                    AppendTo[rules, rule];
                    index++
                ];
              ] /@ Characters[decoded]
            ];
        ];

        If[rules != {},

          line = <| "line" -> originalLineNumber + segmentIndex[[1]] - 1, "characters" -> ReplacePart[Table[" ", {originalColumnCount + 1}], rules] |>;

          If[KeyExistsQ[lineMap, line["line"]],
            lineMap[line["line"]] = merge[lineMap[line["line"]], line]
            ,
            lineMap[line["line"]] = line
          ];
        ]

      ], segments]

    ];

  ] /@ strs;

  lines = Values[lineMap];

  lines = escapeMarkdown[replaceLinearSyntax[replaceControl[replaceLongNamePUA[StringJoin[#["characters"]]]]]]& /@ lines;

  Which[
    Length[lines] == 0,
      result = Null
    ,
    Length[lines] == 1,
      result = <| "contents" -> <| "kind" -> "markdown", "value" -> lines[[1]] |> |>
    ,
    True,
      result = <| "contents" -> "BAD!!!" |>
  ];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> result |>}
]]


(*
For symbols, display their usage message
*)


handleUserSymbols[astIn_, cstIn_, symsIn_] := 
Module[{tokenSymbol, functionSource, 
  functionCallPatternAST1, functionCallPatternAST2, functionCallPatternAST,  
  functionCallPatternCST, functionCallPattern, functionInformationAssoc, requiredUsage,
  symbolContext},

  tokenSymbol = symsIn /. LeafNode[Symbol, ts_, _] :> ts; 

  tokenSymbol = First[tokenSymbol]; 

  (*
  Look up the full context for this symbol from the PacletIndex.
  This gives us the context (e.g. "MyPackage`" or "MyPackage`Private`")
  that the symbol is defined in.
  *)
  symbolContext = GetSymbolContext[tokenSymbol];

  (* 
  Get all the usage messages that are defined in the file 
  *)
  requiredUsage = Cases[astIn, 
    CallNode[
      LeafNode[Symbol, "Set" | "SetDelayed", <||>],
      {
        CallNode[
          LeafNode[Symbol, "MessageName", <||>], 
          {
            LeafNode[Symbol, tokenSymbol, _], 
            LeafNode[String, "\"usage\"", _],
            ___
          }, 
          _
        ], 
        LeafNode[String, msg_, _]
      }, 
      _
    ] :> ToExpression[msg], 
    (* 
    This levelspec selection works for the following cases:
           usage messages without terminating ';'
           usage within package context 
           usage messages without terminating ';' in a package
    *)

    (* 
    Test case: 
      A function is defined in a Package inside Private context and
      the function usage is defined in the Package.

      To get the usage right, depth of 6 is sufficient, because the usage pattern is within
        
        ContainerNode[{                       (adds 2 levels)
            PackageNode[{                     (adds 2 levels)
                  CallNode[{                  (adds 2 levels)
    *)
    6
  ]; 

  log[2, "requiredUsage from handleUserSymbols: "]; 
  log[2, requiredUsage];


  (* 
  Get functionCallPattern AST for functions with SetDelayed & UpSetDelayed 
  *)
  functionCallPatternAST1 = Cases[astIn, CallNode[
    LeafNode[Symbol, "Set" | "SetDelayed" | "UpSet" | "UpSetDelayed", _],
    {
      lhs:CallNode[_, _, _],
      rhs:_
    },
    KeyValuePattern["Definitions" -> {___, LeafNode[Symbol, tokenSymbol, _], ___}]

    (* 
    Test case: 
      A function is defined in a Package inside Private context and
      the function usage is defined in the Package.

      To get the function-call-pattern right, depth of 8 is sufficient, because the function-call-pattern is within
        
        ContainerNode[{                       (adds 2 levels)
            PackageNode[{                     (adds 2 levels)
                ContextNode[{                 (adds 2 levels)
                    CallNode[{                (adds 2 levels)
    *)
    ] :> lhs, 8
  ];

  (* 
  Delete LHS of the usage messages from the functionCallPattern
  *)

  functionCallPatternAST1 = DeleteCases[functionCallPatternAST1, $messageNamePattern];

  (* 
    Get functionCallPattern AST for functions with TagSetDelayed 
  *)
  functionCallPatternAST2 = Cases[astIn, CallNode[
    LeafNode[Symbol, "TagSet" | "TagSetDelayed", _],
    {
      LeafNode[Symbol, tokenSymbol, _], 
      lhs:CallNode[_, _, _], 
      rhs:_
    },
    KeyValuePattern["Definitions" -> {___, LeafNode[Symbol, tokenSymbol, _], ___}]

    ] :> lhs, 8
  ];

  functionCallPatternAST = Join[functionCallPatternAST1, functionCallPatternAST2];

  functionSource = #[[3, Key[Source]]]& /@ functionCallPatternAST;

  (* 
  Test case: 
    A function is defined in a Package inside Private context and
    the function usage is defined in the Package.

    To get the function-call-pattern right, depth of 6 is sufficient, because the function-call-pattern is within
      
      ContainerNode[{                       (adds 2 levels)
              InfixNode[{                   (adds 2 levels)
                    BinaryNode[{            (adds 2 levels)
  *)
  functionCallPatternCST = FirstCase[cstIn, _[_, _, KeyValuePattern[Source -> #]], $Failed, 6]& /@ functionSource;

  If[Length[functionCallPatternCST] == 0 && Length[requiredUsage] == 0,
    functionInformationAssoc =     <|
      "SymbolType" -> "UserDefined",
      "Usage" -> None,
      "DocumentationLink" -> None,
      "FunctionDefinitionPatterns" -> None,
      "FunctionInformation" -> False,
      "Context" -> symbolContext
    |>
    ,
    If[Length[functionCallPatternCST] == 0,
      functionCallPattern = None
      ,
      functionCallPattern = CodeFormatCST[#, "LineWidth" -> $HoverLineWidth]& /@ functionCallPatternCST;
      functionCallPattern = DeleteDuplicates[functionCallPattern];
      (* Trim trailing whitespace/newlines from each formatted pattern *)
      functionCallPattern = StringReplace[#, RegularExpression["\\s+$"] -> ""]& /@ functionCallPattern;
      functionCallPattern = StringRiffle[functionCallPattern, "\n"];
    ];

    If[Length[requiredUsage] == 0,
      requiredUsage = None
      ,
      requiredUsage = StringRiffle[requiredUsage, "\n"]
    ];
    
    functionInformationAssoc = <|
      "SymbolType" -> "UserDefined",
      "Usage" -> requiredUsage,
      "DocumentationLink" -> None,
      "FunctionDefinitionPatterns" -> functionCallPattern,
      "FunctionInformation" -> True,
      "Context" -> symbolContext
    |>
  ];

  functionInformationAssoc
]

handleSystemSymbols[symIn_] :=
Catch[
Module[{usage, documentationLink, sym},

  If[!(NameQ[symIn] && Context[symIn] == "System`"),
    Throw[<|
      "SymbolType" -> "INVALID",
      "Usage" -> "INVALID",
      "DocumentationLink" -> None,
      "FunctionDefinitionPatterns" -> None,
      "FunctionInformation" -> False,
      "Context" -> None
    |>]
  ];

  sym = StringReplace[symIn, StartOfString ~~ "System`" -> ""];

  usage = ToExpression[sym <> "::usage"];

  If[!StringQ[usage],
    Throw[<|
      "SymbolType" -> "INVALID",
      "Usage" -> "INVALID",
      "DocumentationLink" -> None,
      "FunctionDefinitionPatterns" -> None,
      "FunctionInformation" -> False,
      "Context" -> None
    |>]
  ];

  usage = StringJoin[linearToMDSyntax[usage]];

  (*
  
  Do not care about CONSTANT

  If[MemberQ[WolframLanguageSyntax`Generate`$constants, sym],
    line = line <> "\n\nCONSTANT"
  ];
  *)

  If[KeyExistsQ[$undocumentedSet, sym],
    usage = usage <> "\n\nUNDOCUMENTED"
  ];

  If[KeyExistsQ[$experimentalSet, sym],
    usage = usage <> "\n\nEXPERIMENTAL"
  ];

  If[KeyExistsQ[$obsoleteSet, sym],
    usage = usage <> "\n\nOBSOLETE"
  ];

  documentationLink = "[" <> sym <> ": "<> "Web Documentation]" <> 
        "(" <> "https://reference.wolfram.com/language/ref/" <> sym <> ".html" <> ")";
  
  <|
    "SymbolType" -> "System",
    "Usage" -> usage,
    "DocumentationLink" -> documentationLink,
    "FunctionDefinitionPatterns" -> None,
    "FunctionInformation" -> True,
    "Context" -> "System`"
  |>
]]


(*
Handle symbols from external loaded packages (dependencies).
Tries to get usage message from the loaded package.
*)
(*
Handle symbols defined in other workspace files (cross-file hover).
Uses the PacletIndex to find definitions and usage messages, then reads
the definition file to extract function call patterns.
*)
handleCrossFileSymbols[symIn_] :=
Catch[
Module[{tokenSymbol, symData, defs, usages, context, kind, defUri, usage,
  defText, defAST, defCST, fileEntry, defFilePath,
  functionCallPatternAST1, functionCallPatternAST2, functionCallPatternAST,
  functionCallPatternCST, functionSource, functionCallPattern},
  
  (* Strip explicit context prefix if present to get the bare name *)
  tokenSymbol = If[StringContainsQ[symIn, "`"],
    Last[StringSplit[symIn, "`"]],
    symIn
  ];
  
  (* Look up symbol in PacletIndex *)
  symData = Lookup[$PacletIndex, "Symbols", <||>];
  symData = Lookup[symData, tokenSymbol, Null];
  
  If[symData === Null,
    Throw[<|
      "SymbolType" -> "INVALID",
      "Usage" -> "INVALID",
      "DocumentationLink" -> None,
      "FunctionDefinitionPatterns" -> None,
      "FunctionInformation" -> False,
      "Context" -> None
    |>]
  ];
  
  defs = Replace[symData["Definitions"], _Missing -> {}];
  usages = Replace[symData["Usages"], _Missing -> {}];
  
  If[Length[defs] === 0,
    Throw[<|
      "SymbolType" -> "INVALID",
      "Usage" -> "INVALID",
      "DocumentationLink" -> None,
      "FunctionDefinitionPatterns" -> None,
      "FunctionInformation" -> False,
      "Context" -> None
    |>]
  ];
  
  (* Get basic info from the first definition *)
  context = Replace[defs[[1]]["context"], _Missing -> None];
  kind = Replace[defs[[1]]["kind"], _Missing -> "unknown"];
  defUri = defs[[1]]["uri"];
  
  (* Get usage message if available *)
  usage = If[Length[usages] > 0,
    StringRiffle[usages, "\n"],
    None
  ];
  
  (*
  Extract function definition patterns from the definition file.
  Try the open file cache first, then read from disk.
  *)
  functionCallPattern = None;
  
  If[kind === "function" || kind === "declaration",
    
    (* Try to get the file - check if it's currently open first *)
    fileEntry = Lookup[$OpenFilesMap, defUri, Null];
    
    If[fileEntry =!= Null,
      (* File is open - use its cached AST and CST *)
      defAST = fileEntry["AST"];
      defCST = Lookup[fileEntry, "CSTTabs", Null];
      If[defCST === Null,
        defText = fileEntry["Text"];
        defCST = Quiet[CodeConcreteParse[defText, "TabWidth" -> 4]]
      ],
      
      (* File is not open - read from disk *)
      defFilePath = StringReplace[defUri, "file://" -> ""];
      defText = Quiet[Import[defFilePath, "Text"]];
      If[StringQ[defText],
        Module[{rawCST},
          rawCST = Quiet[CodeConcreteParse[defText]];
          (* Only re-parse with TabWidth if file actually contains tabs *)
          defCST = If[StringContainsQ[defText, "\t"],
            Quiet[CodeConcreteParse[defText, "TabWidth" -> 4]],
            rawCST
          ];
          defAST = If[!FailureQ[rawCST],
            Quiet[CodeParser`Abstract`Abstract[CodeParser`Abstract`Aggregate[rawCST]]],
            $Failed
          ]
        ],
        (* File not readable *)
        defCST = Null;
        defAST = Null
      ]
    ];
    
    If[defAST =!= Null && !FailureQ[defAST] && defCST =!= Null && !FailureQ[defCST],
      (* Extract function call patterns - same approach as handleUserSymbols *)
      functionCallPatternAST1 = Cases[defAST, CallNode[
        LeafNode[Symbol, "Set" | "SetDelayed" | "UpSet" | "UpSetDelayed", _],
        {
          lhs:CallNode[_, _, _],
          rhs:_
        },
        KeyValuePattern["Definitions" -> {___, LeafNode[Symbol, tokenSymbol, _], ___}]
        ] :> lhs, 8
      ];
      
      (* Also try TagSetDelayed *)
      functionCallPatternAST2 = Cases[defAST, CallNode[
        LeafNode[Symbol, "TagSet" | "TagSetDelayed", _],
        {
          LeafNode[Symbol, tokenSymbol, _], 
          lhs:CallNode[_, _, _], 
          rhs:_
        },
        KeyValuePattern["Definitions" -> {___, LeafNode[Symbol, tokenSymbol, _], ___}]
        ] :> lhs, 8
      ];
      
      functionCallPatternAST = Join[functionCallPatternAST1, functionCallPatternAST2];
      
      (* Remove usage message LHS from patterns *)
      functionCallPatternAST = DeleteCases[functionCallPatternAST, $messageNamePattern];
      
      If[Length[functionCallPatternAST] > 0,
        functionSource = #[[3, Key[Source]]]& /@ functionCallPatternAST;
        functionCallPatternCST = FirstCase[defCST, _[_, _, KeyValuePattern[Source -> #]], $Failed, 6]& /@ functionSource;
        functionCallPatternCST = DeleteCases[functionCallPatternCST, $Failed];
        
        If[Length[functionCallPatternCST] > 0,
          functionCallPattern = CodeFormatCST /@ functionCallPatternCST;
          functionCallPattern = DeleteDuplicates[functionCallPattern];
          (* Trim trailing whitespace/newlines from each formatted pattern *)
          functionCallPattern = StringReplace[#, RegularExpression["\\s+$"] -> ""]& /@ functionCallPattern;
          functionCallPattern = StringRiffle[functionCallPattern, "\n"]
        ]
      ]
    ]
  ];
  
  (* Build the result *)
  If[!StringQ[usage] && !StringQ[functionCallPattern],
    (* No info at all - but we know it exists, show basic info *)
    <|
      "SymbolType" -> "CrossFile",
      "Usage" -> "Defined in workspace (" <> kind <> ")",
      "DocumentationLink" -> None,
      "FunctionDefinitionPatterns" -> None,
      "FunctionInformation" -> True,
      "Context" -> context
    |>,
    <|
      "SymbolType" -> "CrossFile",
      "Usage" -> If[StringQ[usage], usage, "No usage message."],
      "DocumentationLink" -> None,
      "FunctionDefinitionPatterns" -> functionCallPattern,
      "FunctionInformation" -> True,
      "Context" -> context
    |>
  ]
]]


handleExternalSymbols[symIn_] :=
Catch[
Module[{usage, symContext, bareSymbol, deps, fullSymbol, pacletName, documentationLink,
  defPatterns, downVals, upVals, subVals, symbol, foundContext},

  (* Get dependency contexts *)
  deps = GetDependencyContexts[];
  
  (* Handle both bare symbols and explicitly contexted symbols *)
  If[StringContainsQ[symIn, "`"],
    (* Explicit context - extract parts and use directly *)
    bareSymbol = Last[StringSplit[symIn, "`"]];
    symContext = StringJoin[Riffle[Most[StringSplit[symIn, "`"]], "`"]] <> "`";
    fullSymbol = symIn;
    
    (* For explicit context, verify the symbol exists *)
    If[!Quiet[NameQ[fullSymbol]],
      Throw[<|
        "SymbolType" -> "INVALID",
        "Usage" -> "INVALID",
        "DocumentationLink" -> None,
        "FunctionDefinitionPatterns" -> None,
        "FunctionInformation" -> False,
        "Context" -> None
      |>]
    ],
    
    (* Bare symbol - try to find which dependency context it belongs to *)
    bareSymbol = symIn;
    
    (* First, try to get context using proper evaluation *)
    symContext = Quiet[
      Check[
        (* Use Evaluate + ToExpression to get the actual context *)
        (* Try each dependency context to see if the symbol exists there *)
        foundContext = None;
        Scan[
          Function[{dep},
            If[foundContext === None,
              If[Quiet[NameQ[dep <> bareSymbol]],
                foundContext = dep
              ]
            ]
          ],
          deps
        ];
        (* If not found in deps, try Context with Evaluate *)
        If[foundContext === None,
          foundContext = Quiet[
            Context[Evaluate[ToExpression[bareSymbol]]],
            {Context::notfound, ToExpression::sntx}
          ]
        ];
        foundContext,
        None,
        {Context::notfound}
      ],
      {Context::notfound, ToExpression::sntx}
    ];
    
    If[!StringQ[symContext] || symContext === "Global`" || symContext === "System`" || symContext === None,
      (* Symbol not found in any known context *)
      Throw[<|
        "SymbolType" -> "INVALID",
        "Usage" -> "INVALID",
        "DocumentationLink" -> None,
        "FunctionDefinitionPatterns" -> None,
        "FunctionInformation" -> False,
        "Context" -> None
      |>]
    ];
    fullSymbol = symContext <> bareSymbol;
    
    (* For bare symbols, check if context is a dependency *)
    If[Length[deps] > 0 && !AnyTrue[deps, StringStartsQ[symContext, #] || symContext === # &],
      Throw[<|
        "SymbolType" -> "INVALID",
        "Usage" -> "INVALID",
        "DocumentationLink" -> None,
        "FunctionDefinitionPatterns" -> None,
        "FunctionInformation" -> False,
        "Context" -> None
      |>]
    ];
    
    (* If no deps tracked but we found a non-System/Global context, still try to get info *)
    If[Length[deps] == 0 && (symContext === "Global`" || symContext === "System`" || symContext === None),
      Throw[<|
        "SymbolType" -> "INVALID",
        "Usage" -> "INVALID",
        "DocumentationLink" -> None,
        "FunctionDefinitionPatterns" -> None,
        "FunctionInformation" -> False,
        "Context" -> None
      |>]
    ]
  ];
  
  (*
  Ensure the context is loaded before trying to get usage.
  This is important because usage messages may not be properly formatted
  until the package is actually loaded.
  *)
  Quiet[
    Check[Needs[symContext], Null, {Needs::nocont, Get::noopen}],
    {Needs::nocont, Get::noopen, General::stop}
  ];
  
  (* Try to get usage message *)
  usage = Quiet[
    Check[
      ToExpression[fullSymbol <> "::usage"],
      None,
      {MessageName::noinfo}
    ],
    {MessageName::noinfo, ToExpression::notstrbox}
  ];
  
  If[!StringQ[usage],
    (* No usage message - provide basic info *)
    usage = "Symbol from " <> symContext
  ];
  
  (* Extract definition patterns from DownValues, UpValues, SubValues *)
  defPatterns = Quiet[
    Check[
      symbol = ToExpression[fullSymbol];
      downVals = DownValues[symbol];
      upVals = UpValues[symbol];
      subVals = SubValues[symbol];
      
      (* Extract the LHS patterns from definitions *)
      Join[
        (* DownValues: f[args] := ... *)
        Cases[downVals, HoldPattern[RuleDelayed[HoldPattern[lhs_], _]] :> extractPatternString[lhs], {1}],
        (* UpValues: g[f[args]] ^:= ... - show the full pattern *)
        Cases[upVals, HoldPattern[RuleDelayed[HoldPattern[lhs_], _]] :> extractPatternString[lhs], {1}],
        (* SubValues: f[x][y] := ... *)
        Cases[subVals, HoldPattern[RuleDelayed[HoldPattern[lhs_], _]] :> extractPatternString[lhs], {1}]
      ],
      {},
      {DownValues::sym, UpValues::sym, SubValues::sym}
    ],
    {DownValues::sym, UpValues::sym, SubValues::sym, ToExpression::sntx}
  ];
  
  (* Remove duplicates and limit to reasonable number *)
  defPatterns = DeleteDuplicates[defPatterns];
  defPatterns = Take[defPatterns, UpTo[10]];
  
  (* Extract paclet name for potential documentation link *)
  pacletName = First[StringSplit[symContext, "`"], ""];
  
  (* Build documentation link if possible *)
  documentationLink = If[pacletName =!= "",
    "[" <> bareSymbol <> " (" <> pacletName <> "): Web Documentation]" <> 
      "(" <> "https://reference.wolfram.com/language/" <> pacletName <> "/ref/" <> bareSymbol <> ".html" <> ")",
    None
  ];
  
  <|
    "SymbolType" -> "External",
    "Usage" -> usage,
    "DocumentationLink" -> documentationLink,
    "FunctionDefinitionPatterns" -> If[Length[defPatterns] > 0, 
      StringRiffle[defPatterns, "\n"], 
      None
    ],
    "FunctionInformation" -> True,
    "Context" -> symContext
  |>
]]

(*
Extract a readable string representation of a pattern/definition LHS
*)
extractPatternString[expr_] :=
Module[{str},
  str = Quiet[
    ToString[Unevaluated[expr], InputForm],
    {ToString::shdw}
  ];
  (* Clean up HoldPattern wrapper if present *)
  str = StringReplace[str, "HoldPattern[" ~~ mid__ ~~ "]" /; StringCount[mid, "["] == StringCount[mid, "]"] :> mid];
  str
]

handleSymbols[id_, astIn_, cstIn_, symsIn_] :=
Catch[
Module[{lines, result, syms, functionInformationAssoc},

  syms = symsIn;

  syms = #["String"]& /@ syms;

  lines = Function[{sym},

    (* 
    Find system symbol information
    *)
    functionInformationAssoc = handleSystemSymbols[sym];
    (* 
    If system symbol information is not available, try to find the user defined function information 
    *)
    If[functionInformationAssoc["SymbolType"] == "INVALID",
      functionInformationAssoc = handleUserSymbols[astIn, cstIn, symsIn];
    ];
    (*
    If user symbol information is not available (INVALID or no function info found),
    try cross-file hover via PacletIndex.
    handleUserSymbols returns "UserDefined" with FunctionInformation->False when
    no definitions or usage messages are found in the current file.
    *)
    If[functionInformationAssoc["SymbolType"] == "INVALID" || 
       (functionInformationAssoc["SymbolType"] == "UserDefined" && !TrueQ[functionInformationAssoc["FunctionInformation"]]),
      functionInformationAssoc = handleCrossFileSymbols[sym];
    ];
    (*
    If cross-file information is not available, try to find external package symbol information
    *)
    If[functionInformationAssoc["SymbolType"] == "INVALID",
      functionInformationAssoc = handleExternalSymbols[sym];
    ];

    functionInformationAssoc

  ] /@ syms;


  Which[
    Length[lines] == 0,
      result = Null
    ,
    Length[lines] == 1,
      result = <| "contents" -> <| "kind" -> "markdown", "value" -> formatUsageCallPatterns[lines[[1]]] |> |>;
    ,
    True,
      result = <| "contents" -> "BAD!!!" |>
  ];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> result |>}
]]


(*
For numbers with ^^, display their decimal value
*)
handleNumbers[id_, numsIn_] :=
Catch[
Module[{lines, result, nums, dec},

  nums = numsIn;

  nums = #["String"]& /@ nums;

  nums = Cases[nums, s_ /; StringContainsQ[s, "^^"]];

  lines = Function[{num},

    dec = ToExpression[num];

    (*
    Need to use InputForm because Reals and Rationals can format as 2D if using the default OutputForm
    *)
    dec = ToString[dec, InputForm];

    dec

  ] /@ nums;

  Which[
    Length[lines] == 0,
      result = Null
    ,
    Length[lines] == 1,
      result = <| "contents" -> <| "kind" -> "markdown", "value" -> lines[[1]] |> |>
    ,
    True,
      result = <| "contents" -> "BAD!!!" |>
  ];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> result |>}
]]


linearToMDSyntax[str_] := 
Module[{},
  reassembleEmbeddedLinearSyntax[CodeTokenize[str]] /. {
    LeafNode[Token`Newline, _, _] -> "\n\n",
    LeafNode[Token`LinearSyntax`Bang, _, _] -> "",
    LeafNode[Token`LinearSyntaxBlob, s_, _] :> parseLinearSyntaxBlob[s],
    LeafNode[String, s_, _] :> parseString[s],
    LeafNode[_, s_, _] :> escapeMarkdown[replaceLinearSyntax[replaceControl[replaceLongNamePUA[s]]]],
    ErrorNode[_, s_, _] :> escapeMarkdown[replaceLinearSyntax[replaceControl[replaceLongNamePUA[s]]]]
  }
]

(*
Format the context string for display in hover markdown.
Backticks in Wolfram contexts (e.g. "System`") collide with markdown
inline code syntax. We use double-backtick delimiters so the inner
backtick is rendered literally: `` System` ``
*)
formatContextLine[context_String] :=
  "`` " <> context <> " ``"

formatContextLine[_] := None


(*
Format definition patterns as a wolfram code block.
Input is a raw string with newline-separated patterns (from CodeFormatCST).
We wrap in fenced code so underscores, brackets, etc. render literally.
*)
formatDefinitionPatterns[patterns_String] :=
  "```wolfram\n" <> patterns <> "\n```"

formatDefinitionPatterns[None] := None
formatDefinitionPatterns[_] := None


formatUsageCallPatterns[assoc_] := 
Module[{res, parts, contextLine, patternsBlock},
  
  (* Build context line if available *)
  contextLine = formatContextLine[Lookup[assoc, "Context", None]];
  
  If[Not[TrueQ[assoc["FunctionInformation"]]],
    res = "No function information."
    ,
    Switch[assoc["SymbolType"],
      "System",
        parts = {};
        If[StringQ[contextLine], AppendTo[parts, contextLine]];
        AppendTo[parts, StringJoin[{assoc["Usage"]}]];
        AppendTo[parts, "_" <> assoc["DocumentationLink"] <> "_"];
        res = StringRiffle[parts, "\n\n"],
      "CrossFile",
        (* Cross-file workspace symbols *)
        parts = {};
        If[StringQ[contextLine], AppendTo[parts, contextLine]];
        
        If[StringQ[assoc["Usage"]],
          AppendTo[parts, StringJoin[linearToMDSyntax[assoc["Usage"]]]]
        ];
        
        (* Definition patterns in a fenced code block *)
        patternsBlock = formatDefinitionPatterns[assoc["FunctionDefinitionPatterns"]];
        If[StringQ[patternsBlock],
          AppendTo[parts, "**Definitions**"];
          AppendTo[parts, patternsBlock]
        ];
        
        res = StringRiffle[parts, "\n\n"],
      "External",
        (* External package symbols *)
        parts = {};
        If[StringQ[contextLine], AppendTo[parts, contextLine]];
        AppendTo[parts, StringJoin[linearToMDSyntax[assoc["Usage"]]]];
        
        (* Definition patterns in a fenced code block *)
        patternsBlock = formatDefinitionPatterns[assoc["FunctionDefinitionPatterns"]];
        If[StringQ[patternsBlock],
          AppendTo[parts, "**Definitions**"];
          AppendTo[parts, patternsBlock]
        ];
        
        (* Documentation link *)
        If[assoc["DocumentationLink"] =!= None,
          AppendTo[parts, "_" <> assoc["DocumentationLink"] <> "_"]
        ];
        
        res = StringRiffle[parts, "\n\n"],
      _,
        (* UserDefined symbols — defined in the current file *)
        parts = {};
        If[StringQ[contextLine], AppendTo[parts, contextLine]];
        
        If[StringQ[assoc["Usage"]],
          AppendTo[parts, StringJoin[linearToMDSyntax[assoc["Usage"]]]]
        ];
        
        (* Definition patterns in a fenced code block *)
        patternsBlock = formatDefinitionPatterns[assoc["FunctionDefinitionPatterns"]];
        If[StringQ[patternsBlock],
          AppendTo[parts, "**Definitions**"];
          AppendTo[parts, patternsBlock]
        ];
        
        res = StringRiffle[parts, "\n\n"]
    ];
  ];

  res
]


endsWithOddBackslashesQ[str_String] := 
  StringMatchQ[str, RegularExpression[".*(?<!\\\\)\\\\(\\\\\\\\)*"]]

convertSegment[segment_String /; StringMatchQ[segment, "\"" ~~ ___ ~~ "\""]] :=
  Quiet[Check[ToExpression[segment], $Failed]]

(*
Something from MessageName ::
*)
convertSegment[segment_String] :=
  Quiet[Check[ToExpression["\"" <> segment <> "\""], $Failed]]

convertStartingSegment[segment_] :=
  Quiet[Check[ToExpression[segment <> If[endsWithOddBackslashesQ[segment], "\\\"", "\""]], $Failed]]

convertMiddleSegment[segment_] :=
  Quiet[Check[ToExpression["\"" <> segment <> If[endsWithOddBackslashesQ[segment], "\\\"", "\""]], $Failed]]

convertEndingSegment[segment_] :=
  Quiet[Check[ToExpression["\"" <> segment], $Failed]]



containsUnicodeCharacterQ[str_String] :=
  (*
  Fast test of single backslash before more complicated test
  *)
  StringContainsQ[str, "\\"] &&
  StringContainsQ[str, RegularExpression[
        "(?<!\\\\)\\\\(?:\\\\\\\\)*(?# odd number of leading backslashes)(?:\
(?:\\[[a-zA-Z0-9]+\\])|(?# \\[Alpha] long name)\
(?::[0-9a-fA-F]{4})|(?# \\:xxxx 4 hex)\
(?:\\.[0-9a-fA-F]{2})|(?# \\.xx 2 hex)\
(?:[0-7]{3})|(?# \\xxx octal)\
(?:\\|[0-9a-fA-F]{6})|(?# \\|xxxxxx 6 hex)\
(?:[bf])(?# \\x escape)\
)"]]







parseLinearSyntaxBlob[s_] :=
Module[{res},
  res = Quiet[ToExpression[s]];
  If[FailureQ[res],
    Message[interpretBox::failed];
  ];
  interpretBox[res]
]

parseString[s_] :=
Module[{a1, unquoted, hasStartingQuote, hasEndingQuote},

  (*
  The string may be reassembled and there may have been an error in the linear syntax,
  meaning that there is no trailing quote
  *)
  hasStartingQuote = StringMatchQ[s, "\"" ~~ ___];
  hasEndingQuote = StringMatchQ[s, ___ ~~ "\""];
  unquoted = StringReplace[s, (StartOfString ~~ "\"") | ("\"" ~~ EndOfString) -> ""];

  a1 = reassembleEmbeddedLinearSyntax[CodeTokenize[unquoted]] /. {
    LeafNode[Token`LinearSyntax`Bang, _, _] -> "",
    LeafNode[Token`LinearSyntaxBlob, s1_, _] :> parseLinearSyntaxBlob[s1],
    LeafNode[String, s1_, _] :> parseString[s1],
    LeafNode[_, s1_, _] :> escapeMarkdown[replaceLinearSyntax[replaceControl[replaceLongNamePUA[s1]]]],
    ErrorNode[_, s1_, _] :> escapeMarkdown[replaceLinearSyntax[replaceControl[replaceLongNamePUA[s1]]]]
  };
  {If[hasStartingQuote, "\"", ""], a1, If[hasEndingQuote, "\"", ""]}
]


(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::UnexpectedCharacter:: *)

interpretBox::unhandled = "unhandled: `1`"

interpretBox::unhandledgridbox = "unhandled GridBox"

interpretBox::unhandledSeq = "unhandled: letter sequence that should probably be a RowBox: \n`1`\nIf this looks like boxes, then this is a strange usage message."

interpretBox::unhandled2 = "unhandled: `1`. If this looks like a correct box, then please add to interpretBox. Otherwise, this is a strange usage message."

interpretBox::failed = "unhandled: Linear syntax could not be parsed by ToExpression."

interpretBox[RowBox[children_]] :=
  interpretBox /@ children



interpretBox[StyleBox[a_, "TI", ___Rule]] :=
  {"*", interpretBox[a], "*"}

interpretBox[StyleBox[a_, Bold, ___Rule]] :=
  {"**", interpretBox[a], "**"}

interpretBox[StyleBox[a_, _String, ___Rule]] :=
  interpretBox[a]

interpretBox[StyleBox[a_, ___Rule]] :=
  interpretBox[a]

interpretBox[StyleBox[___]] := (
  Message[interpretBox::unhandled, "StyleBox with weird args"];
  "\[UnknownGlyph]"
)

(* 
    This a workaround for bug #412513, which is a typo in the usage message in the BeginPackage function.
    When bug #412513 is fix, we can remove this workaround.
*)
interpretBox[StyleBox[a_, s_Symbol /; SymbolName[s] == "TR"]] :=
  interpretBox[ToString @ a]


interpretBox[SubscriptBox[a_, b_]] :=
  interpretBox /@ {a, "_", b}

interpretBox[SuperscriptBox[a_, b_, ___Rule]] :=
  interpretBox /@ {a, "^", b}

interpretBox[SubsuperscriptBox[a_, b_, c_]] :=
  interpretBox /@ {a, "_", b, "^", c}

interpretBox[FractionBox[a_, b_]] :=
  interpretBox /@ {a, "/", b}

interpretBox[TagBox[a_, _, ___Rule]] :=
  interpretBox[a]

interpretBox[FormBox[a_, _]] :=
  interpretBox[a]

interpretBox[TooltipBox[a_, _]] :=
  interpretBox[a]

interpretBox[UnderscriptBox[a_, b_, ___Rule]] :=
  interpretBox /@ {a, "+", b}

interpretBox[OverscriptBox[a_, b_]] :=
  interpretBox /@ {a, "&", b}

interpretBox[UnderoverscriptBox[a_, b_, c_, ___Rule]] :=
  interpretBox /@ {a, "+", b, "%", c}

interpretBox[GridBox[_, ___Rule]] := (
  Message[interpretBox::unhandledgridbox];
  "\[UnknownGlyph]"
)

interpretBox[CheckboxBox[_]] := (
  Message[interpretBox::unhandled, "CheckboxBox"];
  "\[UnknownGlyph]"
)

interpretBox[CheckboxBox[_, _]] := (
  Message[interpretBox::unhandled, "CheckboxBox"];
  "\[UnknownGlyph]"
)

interpretBox[DynamicBox[_, ___]] := (
  Message[interpretBox::unhandled, "DynamicBox"];
  "\[UnknownGlyph]"
)

interpretBox[TemplateBox[_, _]] := (
  Message[interpretBox::unhandled, "TemplateBox"];
  "\[UnknownGlyph]"
)

interpretBox[SqrtBox[a_]] :=
  interpretBox /@ {"@", a}

interpretBox[OpenerBox[_]] := (
  Message[interpretBox::unhandled, "OpenerBox"];
  "\[UnknownGlyph]"
)

interpretBox[RadioButtonBox[_, _]] := (
  Message[interpretBox::unhandled, "RadioButtonBox"];
  "\[UnknownGlyph]"
)

interpretBox[RadicalBox[a_, b_]] :=
  interpretBox /@ {"@", a, "%", b}

interpretBox[s_String /; StringMatchQ[s, WhitespaceCharacter... ~~ "\"" ~~ __ ~~ "\"" ~~ WhitespaceCharacter...]] :=
  parseString[s]

(*
Sanity check that the box that starts with a letter is actually a single word or sequence of words
*)
interpretBox[s_String /; StringStartsQ[s, LetterCharacter | "$"] &&
  !StringMatchQ[s, (WordCharacter | "$" | " " | "`" | "_" | "/" | "\[FilledRightTriangle]") ...]] := (
  Message[interpretBox::unhandledSeq, s];
  "\[UnknownGlyph]"
)

interpretBox[s_String] :=
  escapeMarkdown[replaceLinearSyntax[replaceControl[replaceLongNamePUA[s]]]]

interpretBox[$Failed] := (
  "\[UnknownGlyph]"
)

interpretBox[s_Symbol] := (
  (*
  This is way too common to ever fix properly, so concede and convert to string
  *)
  (* Message[interpretBox::unhandled, Symbol];
  "\[UnknownGlyph]" *)
  ToString[s]
)

(*
HACK: Riffle::usage has a Cell expression
*)
interpretBox[Cell[BoxData[a_], _String, ___Rule]] := (
  Message[interpretBox::unhandled, Cell];
  "\[UnknownGlyph]"
)

interpretBox[Cell[TextData[a_], _String, ___Rule]] := (
  Message[interpretBox::unhandled, Cell];
  "\[UnknownGlyph]"
)

(*
HACK: RandomImage::usage has a typos (missing comma) and creates this expression:
("")^2 (", ")^2 type
*)
(* interpretBox[_Times] := (
  Message[interpretBox::unhandled, "strange Times (probably missing a comma)"];
  "\[UnknownGlyph]"
) *)

(*
HACK: NeuralFunctions`Private`MaskAudio::usage has weird typos
*)
(* interpretBox[_PatternTest] := (
  Message[interpretBox::unhandled, "strange PatternTest"];
  "\[UnknownGlyph]"
) *)

interpretBox[b_] := (
  Message[interpretBox::unhandled2, b];
  "\[UnknownGlyph]"
)


escapeMarkdown[s_String] :=
  StringReplace[s, {
    (*
    Only escape characters that have special meaning in markdown and could break rendering.
    VSCode's markdown renderer is fairly tolerant, so we only need to escape:
    - Backticks (code spans)
    - Asterisks (bold/italic) 
    - Underscores (bold/italic)
    - HTML special chars (< > &)
    
    We do NOT need to escape: () [] {} # + - . ! 
    These only have special meaning in specific contexts and escaping them 
    makes the text harder to read.
    *)
    "`" -> "\\`",
    "*" -> "\\*",
    "_" -> "\\_",
    "<" -> "&lt;",
    ">" -> "&gt;",
    "&" -> "&amp;"
  }]




(*
FIXME: maybe have some nicer replacement strings
do not necessarily have to display the escape sequence
*)
replaceControl[s_String] :=
  StringReplace[s, {
    (*
    ASCII control characters
    *)
    "\.00" -> "\\.00",
    "\.01" -> "\\.01",
    "\.02" -> "\\.02",
    "\.03" -> "\\.03",
    "\.04" -> "\\.04",
    "\.05" -> "\\.05",
    "\.06" -> "\\.06",
    "\.07" -> "\\.07",
    "\b" -> "\\b",
    (*\t*)
    (*\n*)
    "\.0b" -> "\\.0b",
    "\f" -> "\\f",
    (*\r*)
    "\.0e" -> "\\.0e",
    "\.0f" -> "\\.0f",
    "\.10" -> "\\.10",
    "\.11" -> "\\.11",
    "\.12" -> "\\.12",
    "\.13" -> "\\.13",
    "\.14" -> "\\.14",
    "\.15" -> "\\.15",
    "\.16" -> "\\.16",
    "\.17" -> "\\.17",
    "\.18" -> "\\.18",
    "\.19" -> "\\.19",
    "\.1a" -> "\\.1a",
    "\[RawEscape]" -> "\\[RawEscape]",
    "\.1c" -> "\\.1c",
    "\.1d" -> "\\.1d",
    "\.1e" -> "\\.1e",
    "\.1f" -> "\\.1f",

    (*
    DEL
    *)
    "\.7f" -> "\\.7f",

    (*
    C1 block
    *)
    "\.80" -> "\\.80",
    "\.81" -> "\\.81",
    "\.82" -> "\\.82",
    "\.83" -> "\\.83",
    "\.84" -> "\\.84",
    "\.85" -> "\\.85",
    "\.86" -> "\\.86",
    "\.87" -> "\\.87",
    "\.88" -> "\\.88",
    "\.89" -> "\\.89",
    "\.8a" -> "\\.8a",
    "\.8b" -> "\\.8b",
    "\.8c" -> "\\.8c",
    "\.8d" -> "\\.8d",
    "\.8e" -> "\\.8e",
    "\.8f" -> "\\.8f",
    "\.90" -> "\\.90",
    "\.91" -> "\\.91",
    "\.92" -> "\\.92",
    "\.93" -> "\\.93",
    "\.94" -> "\\.94",
    "\.95" -> "\\.95",
    "\.96" -> "\\.96",
    "\.97" -> "\\.97",
    "\.98" -> "\\.98",
    "\.99" -> "\\.99",
    "\.9a" -> "\\.9a",
    "\.9b" -> "\\.9b",
    "\.9c" -> "\\.9c",
    "\.9d" -> "\\.9d",
    "\.9e" -> "\\.9e",
    "\.9f" -> "\\.9f"
  }]

(*
Linear syntax characters like \( \) \* etc. don't need special escaping for markdown display.
Just pass through as-is. The characters will display literally in VSCode.
*)
replaceLinearSyntax[s_String] := s


(* :!CodeAnalysis::EndBlock:: *)


reassembleEmbeddedLinearSyntax::unhandled = "Unbalanced openers and closers."

(*
Fix the terrible, terrible design mistake that prevents linear syntax embedded in strings from round-tripping

TODO: dump explanation about terrible, terrible design mistake here
*)
reassembleEmbeddedLinearSyntax[toks_] :=
Catch[
Module[{embeddedLinearSyntax, openerPoss, closerPoss},

  openerPoss = Position[toks, LeafNode[String, s_ /; StringCount[s, "\("] == 1 && StringCount[s, "\)"] == 0, _]];

  closerPoss = Position[toks,
    LeafNode[String, s_ /; StringCount[s, "\("] == 0 && StringCount[s, "\)"] == 1, _] |
      ErrorNode[Token`Error`UnterminatedString, s_ /; StringCount[s, "\("] == 0 && StringCount[s, "\)"] == 1, _]];

  If[Length[openerPoss] != Length[closerPoss],
    Message[reassembleEmbeddedLinearSyntax::unhandled];
    Throw[toks]
  ];

  Fold[
    Function[{toks1, span},
      embeddedLinearSyntax = LeafNode[String, StringJoin[#[[2]]& /@ Take[toks1, {span[[1, 1]], span[[2, 1]]}]], <||>];
      ReplacePart[Drop[toks1, {span[[1, 1]] + 1, span[[2, 1]]}], span[[1]] -> embeddedLinearSyntax]]
    ,
    toks
    ,
    Transpose[{openerPoss, closerPoss}] //Reverse
  ]
]]

$messageNamePattern = CallNode[LeafNode[Symbol, "MessageName", _], 
  {
    LeafNode[Symbol, _, _], 
    LeafNode[String, _, _],
    ___
  }, 
  _
];


End[]

EndPackage[]
