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


hoverEntryReadyQ[entry_Association] :=
Module[{ast, scheduledJobs},
  ast = Lookup[entry, "AST", Null];
  scheduledJobs = Lookup[entry, "ScheduledJobs", {}];

  ast =!= Null && !FailureQ[ast] && scheduledJobs === {}
]


expandContent[content:KeyValuePattern["method" -> "textDocument/hover"], pos_] :=
Catch[
Module[{params, id, doc, uri, entry, res},


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

  entry = Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]];

  If[AssociationQ[entry] && hoverEntryReadyQ[entry],
    log[1, "textDocument/hover: fast-path exit"];
    Throw[{<| "method" -> "textDocument/hoverFencepost", "id" -> id, "params" -> params, "priority" -> True |>}]
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

handleContent[content: KeyValuePattern["method" -> "textDocument/hoverFencepost"]] :=
Catch[
Module[{id, params, doc, uri, position, entry, text, textLines, strs, line, char, pre, ast, cstTabs, syms, toks, nums, slots,
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
  ast = Lookup[entry, "AST", Null];

  If[ast === Null || MissingQ[ast],
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  cstTabs = Lookup[entry, "CSTTabs", Null];

  If[cstTabs === Null,
    (*
    Using "TabWidth" -> 4 here because the notification is rendered down to HTML and tabs need to be expanded in HTML
    FIXME: Must use the tab width from the editor
    For .ipwl files, parse from the annotation-free preprocessed source so token
    positions align with what the PacletIndex and AST see.
    *)
    cstTabs = CodeConcreteParse[Lookup[entry, "PreprocessedText", text], "TabWidth" -> 4];

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
    (LeafNode | CompoundNode)[_, _,
      KeyValuePattern[Source -> src_ /; SourceMemberQ[src, {line, char}]]], Infinity];

  log[2, "hover: after finding position"];

  strs = Cases[toks, LeafNode[String, _, _], Infinity];

  syms = Cases[toks, LeafNode[Symbol, _, _], {1}];

  nums = Cases[toks, LeafNode[Integer | Real | Rational, _, _], Infinity];

  (* Slot/SlotSequence tokens: bare # and ## are LeafNodes; #n and ##n are CompoundNodes *)
  slots = Cases[toks,
    (LeafNode[_, s_String, _] /; StringStartsQ[s, "#"]) |
    CompoundNode[Slot | SlotSequence, _, _],
    Infinity];

  res =
    Which[
      strs != {},
        handleStrings[id, strs, line]
      ,
      syms != {},
        handleSymbols[id, uri, ast, cstTabs, syms, line]
      ,
      slots != {},
        handleSlots[id, uri, ast, cstTabs, slots, line]
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

      {rules} = Last@Reap[If[!FailureQ[decoded],
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
              Sow[index -> char];
              index++
          ]
        ] /@ Characters[decoded]
      ]];

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

        {rules} = Last@Reap[Which[
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
                    Sow[index -> char];
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
                    Sow[index -> char];
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
                    Sow[index -> char];
                    index++
                ];
              ] /@ Characters[decoded]
            ];
        ]];

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
Private helper: infer the WL pattern for any literal AST node, resolving all
the way down to atoms.  Recurses into List and Association constructors.

  integer literal          -> Blank[Integer]
  real literal             -> Blank[Real]
  string literal           -> Blank[String]
  rational literal         -> Blank[Rational]
  True / False             -> _?BooleanQ
  {1, 2, 3}                -> {___Integer}          (homogeneous atom list)
  {1, 2., 3}               -> {(_Integer|_Real)..}  (mixed atom list)
  {{1,2},{3,4}}            -> {{___Integer}..}       (nested lists)
  <|"a"->1, "b"->2.|>     -> <|"a"->_Integer, "b"->_Real|>
  unknown                  -> None
*)
inferNodePattern[node_] :=
  Which[
    MatchQ[node, LeafNode[Integer,  _, _]], Blank[Integer],
    MatchQ[node, LeafNode[Real,     _, _]], Blank[Real],
    MatchQ[node, LeafNode[String,   _, _]], Blank[String],
    MatchQ[node, LeafNode[Rational, _, _]], Blank[Rational],
    MatchQ[node, LeafNode[Symbol, "True" | "False", _]], _?BooleanQ,

    MatchQ[node, CallNode[LeafNode[Symbol, "List", _], _List, _]],
      Module[{elemPats, deduped},
        elemPats = DeleteCases[inferNodePattern /@ node[[2]], None];
        deduped  = DeleteDuplicates[elemPats];
        Which[
          Length[deduped] === 0,
            None,
          Length[deduped] === 1 && MatchQ[deduped[[1]], _Blank],
            List[BlankNullSequence @@ deduped[[1]]],
          Length[deduped] === 1,
            List[Repeated[deduped[[1]]]],
          True,
            List[Repeated[Alternatives @@ deduped]]
        ]
      ],

    MatchQ[node, CallNode[LeafNode[Symbol, "Association", _], _List, _]],
      Module[{pairs},
        pairs = Cases[node[[2]],
          CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _],
            {LeafNode[String, keyStr_String, _], valNode_}, _] :>
            (StringTake[keyStr, {2, -2}] -> inferNodePattern[valNode])
        ];
        If[Length[pairs] === 0 || AnyTrue[pairs[[All, 2]], # === None &],
          None,
          Association @@ pairs
        ]
      ],

    (* Part[expr, i] — element type of the collection *)
    MatchQ[node, CallNode[LeafNode[Symbol, "Part", _], {_, __}, _]],
      inferListElementPattern[node[[2, 1]]],

    (* Lookup[assoc, key] — value type of the association *)
    MatchQ[node, CallNode[LeafNode[Symbol, "Lookup", _], {_, _, ___}, _]],
      Module[{assocArg = node[[2, 1]], valuePats},
        Which[
          MatchQ[assocArg, CallNode[LeafNode[Symbol, "Association", _], _List, _]],
            valuePats = DeleteDuplicates[DeleteCases[
              Cases[assocArg[[2]],
                CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], {_, valNode_}, _] :>
                  inferNodePattern[valNode],
                1],
              None]];
            Switch[Length[valuePats], 0, None, 1, valuePats[[1]], _, Alternatives @@ valuePats],
          True, None
        ]
      ],

    True, None
  ]


(*
Infer the WL hover pattern for a single element of a list-valued AST node.
Used by iterator-variable and Map/Scan parameter inference.  Delegates to
inferNodePattern for each element so that nested lists and associations resolve
all the way down to atoms.

  {1, 2, 3}              -> Blank[Integer]
  {"a", "b"}             -> Blank[String]
  {"", 2, 3}             -> Alternatives[Blank[String], Blank[Integer]]
  {{1,2},{3,4}}          -> {___Integer}
  {<|"a"->1|>}           -> <|"a"->_Integer|>
  Range[n]               -> Blank[Integer]
  unknown elements       -> None
*)
inferListElementPattern[listNode_] :=
  Which[
    MatchQ[listNode, CallNode[LeafNode[Symbol, "List", _], {_, ___}, _]],
      Module[{types = DeleteDuplicates[DeleteCases[
          inferNodePattern /@ listNode[[2]],
          None]]},
        Which[
          Length[types] === 1, types[[1]],
          Length[types] > 1,   Alternatives @@ types,
          True,                None
        ]
      ],
    MatchQ[listNode, CallNode[LeafNode[Symbol, "Range", _], {__}, _]], Blank[Integer],
    True, None
  ]


(*
For symbols, display their usage message
*)


handleUserSymbols[uri_, astIn_, cstIn_, symsIn_, cursorLine_] :=
Module[{tokenSymbol, functionSource,
  functionCallPatternAST1, functionCallPatternAST2, functionCallPatternAST,
  functionCallPatternCST, functionCallPattern, functionInformationAssoc, requiredUsage,
  symbolContext, docComments, defSources, docCommentsBySource, perDefDocComments,
  inferredPattern, hasVarDefsInFile, declaredType, declaredTypeSource},

  tokenSymbol = symsIn /. LeafNode[Symbol, ts_, _] :> ts;

  tokenSymbol = First[tokenSymbol];

  (*
  Look up the full context for this symbol from the PacletIndex.
  This gives us the context (e.g. "MyPackage`" or "MyPackage`Private`")
  that the symbol is defined in.
  *)
  symbolContext = GetSymbolContext[uri, tokenSymbol];

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

  (*
  Build a doc-comment lookup keyed by definition start-line.
  We use the same extractDocComments logic from PacletIndex.
  *)
  docComments = ExtractDocComments[cstIn];

  (*
  For each function definition (by source), look up whether the line immediately
  before it has a doc-comment, and collect those associations.
  docCommentsBySource maps Source -> parsed doc-comment assoc (or None).
  *)
  defSources = #[[3, Key[Source]]]& /@ functionCallPatternAST;
  docCommentsBySource = AssociationThread[
    defSources,
    Lookup[docComments, #[[1, 1]] - 1, None]& /@ defSources
  ];

  (*
  Collect the unique doc-comments attached to the definitions of this symbol.
  perDefDocComments is the list aligned with functionCallPatternCST.
  *)
  perDefDocComments = Lookup[docCommentsBySource, functionSource, None];

  (*
  Enrich each doc-comment with InputPatternStrings inferred from the corresponding
  definition LHS.  Only typed blanks (e.g. x_Integer -> "_Integer") are included;
  bare wildcards (x_) are omitted since they add no information.
  *)
  perDefDocComments = MapThread[
    Function[{dc, lhsNode},
      Module[{pats, strs},
        pats = LSPServer`PacletIndex`ExtractLHSInputPatterns[lhsNode];
        strs = Select[Map[ToString[#, InputForm] &, pats], # =!= "_" && StringLength[#] > 0 &];
        Which[
          AssociationQ[dc] && Length[strs] > 0,
            Append[dc, "InputPatternStrings" -> strs],
          !AssociationQ[dc] && Length[strs] > 0,
            <|"Description" -> None, "InputPatternStrings" -> strs|>,
          True, dc
        ]
      ]
    ],
    {perDefDocComments, functionCallPatternAST}
  ];

  (*
  Inject InferredReturnPattern into doc-comments that lack an explicit Return: annotation.
  This allows hover to show the automatically inferred return type even when the function
  has no doc-comment at all, or has a doc-comment without a Return: field.
  We look up InferredReturnPattern from the PacletIndex by matching definition source lines.
  *)
  Module[{pacletDefs, irpByLine},
    pacletDefs = Select[
      LSPServer`PacletIndex`GetVisibleSymbolDefinitions[uri, tokenSymbol],
      Lookup[#, "uri", None] === uri &
    ];
    (* Map: def start line -> InferredReturnPattern *)
    irpByLine = Association[
      Map[
        Function[{def},
          With[{irp = Lookup[def, "InferredReturnPattern", None]},
            If[!MatchQ[irp, None | _Missing],
              def["source"][[1, 1]] -> irp,
              Nothing
            ]
          ]
        ],
        pacletDefs
      ]
    ];
    If[Length[irpByLine] > 0,
      perDefDocComments = MapThread[
        Function[{dc, astNode},
          Module[{defLine, irp, irpStr},
            defLine = astNode[[3, Key[Source]]][[1, 1]];
            irp = Lookup[irpByLine, defLine, None];
            If[MatchQ[irp, None | _Missing],
              dc,  (* no inferred pattern for this definition *)
              irpStr = Quiet[ToString[irp, InputForm], {ToString::shdw}];
              Which[
                (* No DocComment at all: synthesize one with just the inferred return type *)
                !AssociationQ[dc],
                  <|"Description" -> None, "ReturnPattern" -> irp,
                    "ReturnPatternString" -> irpStr|>,
                (* DocComment exists but has no ReturnPattern: inject the inferred one *)
                MatchQ[Lookup[dc, "ReturnPattern", None], None | _Missing],
                  Join[dc, <|"ReturnPattern" -> irp, "ReturnPatternString" -> irpStr|>],
                (* DocComment already has an explicit ReturnPattern: leave it alone *)
                True, dc
              ]
            ]
          ]
        ],
        {perDefDocComments, functionCallPatternAST}
      ]
    ]
  ];

  (*
  Read the position-aware InferredPattern: pick the most recent constant/variable
  definition whose source line is at or before the cursor line.  This makes hover
  reflect the type of the variable AT the cursor position rather than always
  showing the first (or merged) i  nferred pattern.
  Also track whether the symbol has ANY variable definition in the current file.
  so pre-assignment hover shows no type rather than falling through to the
  (position-unaware) cross-file handler.
  *)
  {inferredPattern, hasVarDefsInFile} = Module[{syms, defs, byUri, varDefs, relevant, result},
    syms = Lookup[LSPServer`PacletIndex`$PacletIndex, "Symbols", <||>];
    If[!KeyExistsQ[syms, tokenSymbol],
      {None, False},
      defs = Lookup[syms[tokenSymbol], "Definitions", {}];
      byUri = Select[defs, #["uri"] === uri &];
      varDefs = Select[byUri, MemberQ[{"constant", "declaration"}, Lookup[#, "kind", ""]] &];
      (* Keep only variable definitions at or before the cursor *)
      relevant = Select[varDefs,
        MatchQ[Lookup[#, "source", {}], {{l_, _}, _} /; l <= cursorLine] &
      ];
      (* Sort ascending by start line, pick the InferredPattern of the last assignment
         that has a non-None, non-Missing InferredPattern.  None entries are skipped so
         that an equality-If or other deliberately opaque assignment does not erase a
         more informative PatternTest type inferred from an earlier comparison-If. *)
      relevant = SortBy[relevant, #["source"][[1, 1]] &];
      result = None;
      Scan[
        Function[{def},
          Module[{ip},
            ip = Lookup[def, "InferredPattern", None];
            If[ip =!= None && !MatchQ[ip, _Missing], result = ip]
          ]
        ],
        relevant
      ];
      {result, Length[varDefs] > 0}
    ]
  ];

  (*
  Read DeclaredType from PacletIndex - explicit .ipwl annotation, takes precedence
  over InferredPattern everywhere. Check the current file's URI first, then any
  companion .ipwl file.
  *)
  {declaredType, declaredTypeSource} = Module[{allDefs, dtEntry},
    allDefs = LSPServer`PacletIndex`GetVisibleSymbolDefinitions[uri, tokenSymbol];
    dtEntry = SelectFirst[allDefs,
      !MatchQ[Lookup[#, "DeclaredType", None], None | _Missing] &,
      None
    ];
    If[AssociationQ[dtEntry],
      {
        dtEntry["DeclaredType"],
        (* Companion file hint: show filename when declaration comes from a different file *)
        If[dtEntry["uri"] =!= uri,
          FileNameTake[StringReplace[dtEntry["uri"], "file://" -> ""]],
          None
        ]
      },
      {None, None}
    ]
  ];

  (*
  Parameter pattern inference: if the PacletIndex produced no inferredPattern,
  check whether tokenSymbol is a formal parameter of a function whose body
  contains cursorLine.

    Case A – typed named-function param:  f[x_Integer] := …  ->  Blank[Integer]
    Case B – untyped Function param inferred from application:
              Function[{a}, …][ "hello" ]  ->  Blank[String]
  *)
  If[inferredPattern === None,
    inferredPattern = Module[
      {cstBlankToPattern, argNodePattern},

      (* Convert Blank[T]/BlankSequence[T] CST node -> WL expression e.g. Blank[Integer] *)
      cstBlankToPattern = Function[{bn},
        If[MatchQ[bn, CallNode[LeafNode[Symbol, "Blank"|"BlankSequence"|"BlankNullSequence", _],
                               {LeafNode[Symbol, _String, _]}, _]],
          Quiet[ToExpression[bn[[1,2]] <> "[" <> bn[[2,1,2]] <> "]"], {ToExpression::shdw}],
          None (* untyped _ *)
        ]
      ];

      (* Infer WL pattern from a local RHS node. *)
      argNodePattern = Function[{n},
        Which[
          MatchQ[n, LeafNode[String,   _, _]], Blank[String],
          MatchQ[n, LeafNode[Integer,  _, _]], Blank[Integer],
          MatchQ[n, LeafNode[Real,     _, _]], Blank[Real],
          MatchQ[n, LeafNode[Rational, _, _]], Blank[Rational],
          (* Part[expr, i] — element type of the collection *)
          MatchQ[n, CallNode[LeafNode[Symbol, "Part", _], {_, __}, _]],
            inferListElementPattern[n[[2, 1]]],
          (* Lookup[assoc, key] — value type of the association *)
          MatchQ[n, CallNode[LeafNode[Symbol, "Lookup", _], {_, _, ___}, _]],
            Module[{assocArgN = n[[2, 1]], vPats},
              Which[
                MatchQ[assocArgN, CallNode[LeafNode[Symbol, "Association", _], _List, _]],
                  vPats = DeleteDuplicates[DeleteCases[
                    Cases[assocArgN[[2]],
                      CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], {_, valNode_}, _] :>
                        inferNodePattern[valNode],
                      1],
                    None]];
                  Switch[Length[vPats], 0, None, 1, vPats[[1]], _, Alternatives @@ vPats],
                True, None
              ]
            ],
          True,
            With[{pat = LSPServer`PacletIndex`Private`inferPatternFromRHS[n, <||>, uri]},
              If[!MatchQ[pat, None | _Missing], pat, None]
            ]
        ]
      ];

      Catch[
        (* Case A: SetDelayed/Set  f[…, x_T, …] := …  *)
        Scan[Function[{defNode},
          Module[{s = Quiet[defNode[[3, Key[Source]]]]},
            If[MatchQ[s, {{l1_, _}, {l2_, _}} /; l1 <= cursorLine <= l2],
              Cases[defNode[[2,1,2]],
                CallNode[LeafNode[Symbol, "Pattern", _],
                  {LeafNode[Symbol, tokenSymbol, _], bn_}, _] :>
                  Module[{p = cstBlankToPattern[bn]},
                    If[p =!= None, Throw[p]]
                  ],
                Infinity
              ]
            ]
          ]],
          Cases[astIn,
            CallNode[LeafNode[Symbol, "SetDelayed"|"Set", _],
              {CallNode[_, _List, _], _}, _], Infinity]
        ];

        (* Case B: Function[{…, a, …}, body][…, argN, …]  untyped param inferred from arg *)
        Scan[Function[{appNode},
          Module[{funcNode = appNode[[1]], params, bodySrc, paramNames, idx, p},
            params = funcNode[[2,1,2]];  (* contents of List[...] *)
            bodySrc = Quiet[funcNode[[2,2,3,Key[Source]]]];
            If[!MatchQ[bodySrc, {{l1_, _}, {l2_, _}} /; l1 <= cursorLine <= l2],
              Return[]
            ];
            paramNames = Cases[params, LeafNode[Symbol, s_String, _] :> s, 1];
            idx = FirstPosition[paramNames, tokenSymbol, None, {1}];
            If[idx === None, Return[]];
            idx = idx[[1]];
            If[idx > Length[appNode[[2]]], Return[]];
            p = argNodePattern[appNode[[2, idx]]];
            If[p =!= None, Throw[p]]
          ]],
          Cases[astIn,
            node:CallNode[
              CallNode[LeafNode[Symbol, "Function", _],
                {CallNode[LeafNode[Symbol, "List", _], _List, _], _}, _],
              _List, _] :> node,
            Infinity]
        ];

        (* Case C: Table/Do/Sum/Product/Array iterator variable
           {i, lo, hi} or {i, n} -> _Integer;  {i, listExpr} -> element type of listExpr *)
        Scan[Function[{iterNode},
          Module[{args = iterNode[[2]], iterSrc},
            If[Length[args] < 2, Return[]];
            iterSrc = Quiet[iterNode[[3, Key[Source]]]];
            If[!MatchQ[iterSrc, {{l1_, _}, {l2_, _}} /; l1 <= cursorLine <= l2], Return[]];
            Scan[Function[{spec},
              If[MatchQ[spec, CallNode[LeafNode[Symbol, "List", _],
                                       {LeafNode[Symbol, tokenSymbol, _], __}, _]],
                Module[{specArgs = spec[[2]]},
                  If[Length[specArgs] == 2 &&
                     MatchQ[specArgs[[2]], CallNode[LeafNode[Symbol, "List", _], _, _]],
                    (* {i, listExpr} -> element type *)
                    Throw[With[{ep = inferListElementPattern[specArgs[[2]]]},
                      If[ep === None, Blank[], ep]]],
                    (* {i, lo} / {i, lo, hi} / {i, lo, hi, step} -> Integer *)
                    Throw[Blank[Integer]]
                  ]
                ]
              ]],
              Rest[args] (* skip body; iterator specs are args 2..n *)
            ]
          ]],
          Cases[astIn,
            CallNode[LeafNode[Symbol, "Table"|"Do"|"Sum"|"Product"|"Array", _], _List, _],
            Infinity]
        ];

        (* Case D: Map/Scan/Select[Function[{a,...}, body], list] -> a gets element type *)
        Scan[Function[{mapNode},
          Module[{args = mapNode[[2]], func, list, params, bodySrc, paramNames, idx, ep},
            If[Length[args] < 2, Return[]];
            {func, list} = Switch[mapNode[[1, 2]],
              "Select" | "Pick", {args[[2]], args[[1]]},
              _,                 {args[[1]], args[[2]]}
            ];
            If[!MatchQ[func, CallNode[LeafNode[Symbol, "Function", _],
                                      {CallNode[LeafNode[Symbol, "List", _], _List, _], _}, _]],
              Return[]
            ];
            params  = func[[2, 1, 2]];
            bodySrc = Quiet[func[[2, 2, 3, Key[Source]]]];
            If[!MatchQ[bodySrc, {{l1_, _}, {l2_, _}} /; l1 <= cursorLine <= l2], Return[]];
            paramNames = Cases[params, LeafNode[Symbol, s_String, _] :> s, 1];
            idx = FirstPosition[paramNames, tokenSymbol, None, {1}];
            If[idx === None, Return[]];
            ep = inferListElementPattern[list];
            Throw[If[ep === None, Blank[], ep]]
          ]],
          Cases[astIn,
            CallNode[LeafNode[Symbol, "Map"|"Scan"|"Select"|"Pick"|"MapIndexed"|"MapThread", _],
              _List, _], Infinity]
        ];

        (* Case E: Enclose[expr, Function[param, body]] -> param gets _Failure.
           Works for both Function[{e,...}, body] (List form) and Function[e, body]
           (single-symbol form).  Scope: cursor anywhere inside the Function node. *)
        Scan[Function[{encNode},
          Module[{args = encNode[[2]], func, funcSrc, paramNames},
            If[Length[args] < 2, Return[]];
            func = args[[2]];
            funcSrc = Quiet[func[[3, Key[Source]]]];
            If[!MatchQ[funcSrc, {{l1_, _}, {l2_, _}} /; l1 <= cursorLine <= l2], Return[]];
            Which[
              (* Function[{e,...}, body] *)
              MatchQ[func, CallNode[LeafNode[Symbol, "Function", _],
                       {CallNode[LeafNode[Symbol, "List", _], _List, _], _}, _]],
                paramNames = Cases[func[[2, 1, 2]], LeafNode[Symbol, s_String, _] :> s, 1],
              (* Function[e, body] — single Symbol param, no List *)
              MatchQ[func, CallNode[LeafNode[Symbol, "Function", _],
                       {LeafNode[Symbol, _, _], _}, _]],
                paramNames = {func[[2, 1, 2]]},
              True, Return[]
            ];
            If[MemberQ[paramNames, tokenSymbol], Throw[Blank[Failure]]]
          ]],
          Cases[astIn,
            CallNode[LeafNode[Symbol, "Enclose", _], _List, _], Infinity]
        ];

        (* Case F: Module/Block/With local variable.
           Module[{x=5, y="hello", z, ...}, body] and With[{x=5, ...}, body]:
             - cursor inside the outer node's source range
             - tokenSymbol appears as the LHS of a Set in the var-list, or as a bare symbol
             - type is inferred from the RHS (literals -> Blank[T]; List -> element pattern;
               uninitialized -> Blank[])
           Block behaves the same way at the analysis level. *)
        Scan[Function[{mbwNode},
          Catch[
          Module[{scopeArgs, varListNode, outerSrc, varRhsPattern},
            scopeArgs = mbwNode[[2]];
            If[Length[scopeArgs] < 2, Throw[Null, "next"]];
            varListNode = scopeArgs[[1]];
            If[!MatchQ[varListNode, CallNode[LeafNode[Symbol, "List", _], _List, _]],
              Throw[Null, "next"]
            ];
            outerSrc = Quiet[mbwNode[[3, Key[Source]]]];
            If[!MatchQ[outerSrc, {{l1_, _}, {l2_, _}} /; l1 <= cursorLine <= l2], Throw[Null, "next"]];
            varRhsPattern = Catch[
              Scan[Function[{vn},
                Which[
                  (* plain symbol: uninitialized -> unknown type *)
                  MatchQ[vn, LeafNode[Symbol, tokenSymbol, _]],
                    Throw[Blank[]],
                  (* x = rhs: infer type from rhs *)
                  MatchQ[vn, CallNode[LeafNode[Symbol, "Set", _],
                                      {LeafNode[Symbol, tokenSymbol, _], _}, _]],
                    Module[{rhs = vn[[2, 2]], p},
                      p = Which[
                        argNodePattern[rhs] =!= None,
                          argNodePattern[rhs],
                        MatchQ[rhs, CallNode[LeafNode[Symbol, "List", _], _List, _]],
                          With[{ep = inferListElementPattern[rhs]},
                            If[ep =!= None, ep, Blank[List]]],
                        MatchQ[rhs, CallNode[LeafNode[Symbol, "Association", _], _, _]],
                          Blank[Association],
                        True, None
                      ];
                      Throw[p]
                    ]
                ]],
              varListNode[[2]]
            ]];
          (* Flow-sensitive: scan body for re-assignments, pick last before cursor.
             Skip entirely if tokenSymbol not found in var-list (varRhsPattern === Null). *)
          If[varRhsPattern =!= Null,
            Module[{initPat, allPairs, validPairs},
              initPat = If[varRhsPattern === None, None, varRhsPattern];
              allPairs = If[initPat =!= None, {{outerSrc[[1, 1]], initPat}}, {}];
              allPairs = Join[allPairs,
                Cases[Rest[scopeArgs],
                  CallNode[LeafNode[Symbol, "Set", _],
                    {LeafNode[Symbol, tokenSymbol, _], rhs_},
                    KeyValuePattern[Source -> {{setLine_, _}, _}]] :>
                    Module[{p = Which[
                      argNodePattern[rhs] =!= None,
                        argNodePattern[rhs],
                      MatchQ[rhs, CallNode[LeafNode[Symbol, "List", _], _List, _]],
                        With[{ep = inferListElementPattern[rhs]},
                          If[ep =!= None, ep, Blank[List]]],
                      MatchQ[rhs, CallNode[LeafNode[Symbol, "Association", _], _, _]],
                        Blank[Association],
                      True, None
                    ]},
                    If[p =!= None, {setLine, p}, Nothing]
                    ],
                  Infinity]
              ];
              validPairs = Select[allPairs, #[[1]] <= cursorLine &];
              If[Length[validPairs] > 0,
                Throw[Last[SortBy[validPairs, First]][[2]]]
              ]
            ]
          ]
          ],
          "next"]],
          Cases[astIn,
            CallNode[LeafNode[Symbol, "Module" | "Block" | "With", _], _List, _],
            Infinity]
        ];

        None (* no parameter pattern found *)
      ]    ]
  ];

  (*
  Apply branch-narrowing to inferredPattern: if the cursor sits inside a branch
  body of an If node whose condition constrains tokenSymbol, override the inferred
  pattern with the branch-specific type.
  Handles:
    TypeQ[sym]          true  -> Blank[Type]          false -> Except[Blank[Type]]
    sym op literal      true  -> PatternTest[base, (# op lit &)]
                        false -> PatternTest[base, (# negOp lit &)]
    Not[cond]                 -> swap true/false
    And[c1, c2, ...]          -> first applicable conjunct
  *)
  inferredPattern = Module[
    {compNegMap, basePattern, applyTrueCond, applyFalseCond, narrowed, typeQMap},

    compNegMap = <|
      "Greater" -> "LessEqual", "GreaterEqual" -> "Less",
      "Less"    -> "GreaterEqual", "LessEqual"  -> "Greater"
    |>;
    typeQMap = <|
      "StringQ"                       -> Blank[String],
      "Internal`SymbolNameQ"          -> Blank[String],
      "IntegerQ"                      -> Blank[Integer],
      "MachineIntegerQ"               -> Blank[Integer],
      "Internal`NonNegativeIntegerQ"  -> Blank[Integer],
      "RealQ"                         -> Blank[Real],
      "MachineRealQ"                  -> Blank[Real],
      "NumberQ"                       -> Blank[Number],
      "NumericQ"                      -> Blank[Number],
      "InexactNumberQ"                -> Blank[Number],
      "ListQ"                         -> Blank[List],
      "VectorQ"                       -> Blank[List],
      "MatrixQ"                       -> Blank[List],
      "AssociationQ"                  -> Blank[Association]
    |>;
    basePattern = If[inferredPattern =!= None, inferredPattern, Blank[]];

    applyTrueCond = Function[{cNode},
      Which[
        (* TypeQ[sym] *)
        MatchQ[cNode, CallNode[LeafNode[Symbol, _String, _],
                               {LeafNode[Symbol, tokenSymbol, _]}, _]],
          Lookup[typeQMap, cNode[[1, 2]], None],

        (* sym op literal: sym > 5, sym < 3, etc. *)
        MatchQ[cNode, CallNode[
            LeafNode[Symbol, "Greater"|"GreaterEqual"|"Less"|"LessEqual", _],
            {LeafNode[Symbol, tokenSymbol, _],
             LeafNode[Integer|Real|Rational, _, _]}, _]],
          With[{op  = Symbol[cNode[[1, 2]]],
                lv  = Quiet[ToExpression[cNode[[2, 2, 2]]], {ToExpression::shdw}]},
            PatternTest[basePattern, Function[op[Slot[1], lv]]]
          ],

        (* And[c1, c2, ...]: first conjunct that applies *)
        MatchQ[cNode, CallNode[LeafNode[Symbol, "And", _], _List, _]],
          Catch[
            Scan[Function[{sub},
              Module[{r = applyTrueCond[sub]},
                If[r =!= None, Throw[r]]]
            ], cNode[[2]]];
            None
          ],

        True, None
      ]
    ];

    applyFalseCond = Function[{cNode},
      Which[
        (* Not[inner]: false of !cond means inner is true *)
        MatchQ[cNode, CallNode[LeafNode[Symbol, "Not", _], {_}, _]],
          applyTrueCond[cNode[[2, 1]]],

        (* TypeQ[sym]: false branch -> Except[Blank[TypeName]] *)
        MatchQ[cNode, CallNode[LeafNode[Symbol, _String, _],
                               {LeafNode[Symbol, tokenSymbol, _]}, _]],
          Module[{negPat = Lookup[typeQMap, cNode[[1, 2]], None]},
            If[negPat === None, None, Except[negPat]]
          ],

        (* sym op literal: false branch has sym (negated op) literal *)
        MatchQ[cNode, CallNode[
            LeafNode[Symbol, "Greater"|"GreaterEqual"|"Less"|"LessEqual", _],
            {LeafNode[Symbol, tokenSymbol, _],
             LeafNode[Integer|Real|Rational, _, _]}, _]],
          Module[{negOp = Lookup[compNegMap, cNode[[1, 2]], None],
                  lv    = Quiet[ToExpression[cNode[[2, 2, 2]]], {ToExpression::shdw}]},
            If[negOp === None, None,
              With[{op = Symbol[negOp]},
                PatternTest[basePattern, Function[op[Slot[1], lv]]]
              ]
            ]
          ],

        True, None
      ]
    ];

    (* Scan all If nodes in the AST for one whose branch body contains cursorLine *)
    narrowed = Catch[
      Scan[Function[{ifNode},
        Module[{args, trueSrc, falseSrc, r},
          args = ifNode[[2]];
          If[Length[args] < 2, Return[]];

          (* True branch *)
          trueSrc = Quiet[args[[2, 3, Key[Source]]], {}];
          If[MatchQ[trueSrc, {{_Integer, _}, {_Integer, _}}] &&
             trueSrc[[1, 1]] <= cursorLine <= trueSrc[[2, 1]],
            r = applyTrueCond[args[[1]]];
            If[r =!= None, Throw[r]]
          ];

          (* False branch *)
          If[Length[args] >= 3,
            falseSrc = Quiet[args[[3, 3, Key[Source]]], {}];
            If[MatchQ[falseSrc, {{_Integer, _}, {_Integer, _}}] &&
               falseSrc[[1, 1]] <= cursorLine <= falseSrc[[2, 1]],
              r = applyFalseCond[args[[1]]];
              If[r =!= None, Throw[r]]
            ]
          ]
        ]
      ],
      Cases[astIn, CallNode[LeafNode[Symbol, "If", _], _, _], Infinity]
      ];
      inferredPattern  (* no branch override found *)
    ];

    narrowed
  ];

  If[Length[functionCallPatternCST] == 0 && Length[requiredUsage] == 0,
    functionInformationAssoc =     <|
      "SymbolType" -> "UserDefined",
      "Usage" -> None,
      "DocumentationLink" -> None,
      "FunctionDefinitionPatterns" -> None,
      "FunctionInformation" -> If[hasVarDefsInFile || inferredPattern =!= None ||
                                  !MatchQ[declaredType, None | _Missing], True, False],
      "Context" -> symbolContext,
      "DocComments" -> {},
      "InferredPattern" -> inferredPattern,
      "DeclaredType" -> declaredType,
      "DeclaredTypeSource" -> declaredTypeSource
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
      "Context" -> symbolContext,
      (*
      DocComments: list of doc-comment associations (or None) parallel to
      the function definitions, deduplicated for display.
      *)
      "DocComments" -> DeleteDuplicates[Select[perDefDocComments, AssociationQ]],
      "InferredPattern" -> inferredPattern,
      "DeclaredType" -> declaredType,
      "DeclaredTypeSource" -> declaredTypeSource
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
handleCrossFileSymbols[uri_, symIn_] :=
Catch[
Module[{tokenSymbol, symData, defs, usages, context, kind, defUri, usage,
  defText, defAST, defCST, fileEntry, defFilePath,
  functionCallPatternAST1, functionCallPatternAST2, functionCallPatternAST,
  functionCallPatternCST, functionSource, functionCallPattern, aliasContext,
  preferredContext, aliasMap},

  (* Strip explicit context prefix if present to get the bare name *)
  (* Also detect if the prefix is a known alias context *)
  aliasContext = None;
  preferredContext = None;
  tokenSymbol = If[StringContainsQ[symIn, "`"],
    With[{parts = StringSplit[symIn, "`"], ctxPart = StringJoin[Riffle[Most[StringSplit[symIn, "`"]], "`"]] <> "`"},
      aliasMap = GetContextAliases[];
      preferredContext = If[AssociationQ[aliasMap], Lookup[aliasMap, ctxPart, ctxPart], ctxPart];
      If[StringQ[preferredContext] && preferredContext =!= ctxPart,
        aliasContext = ctxPart
      ];
      Last[parts]
    ],
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

  defs = LSPServer`PacletIndex`GetVisibleSymbolDefinitions[uri, tokenSymbol, preferredContext];
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
      "Context" -> context,
      "AliasContext" -> aliasContext,
      "DocComments" -> Module[{enriched},
        enriched = Function[{def},
          Module[{dc, pats, strs},
            dc = Replace[def["DocComment"], _Missing -> None];
            If[!AssociationQ[dc], Return[dc]];
            pats = Lookup[def, "InputPatterns", {}];
            strs = Select[Map[ToString[#, InputForm] &, pats], # =!= "_" && StringLength[#] > 0 &];
            If[Length[strs] > 0, Append[dc, "InputPatternStrings" -> strs], dc]
          ]
        ] /@ defs;
        DeleteDuplicates[Select[enriched, AssociationQ]]
      ],
      "InferredPattern" -> Catch[Scan[Function[{d}, Module[{ip}, ip = Lookup[d, "InferredPattern", None]; If[ip =!= None && !MatchQ[ip, _Missing], Throw[ip]]]], defs]; None]
    |>,
    <|
      "SymbolType" -> "CrossFile",
      "Usage" -> If[StringQ[usage], usage, "No usage message."],
      "DocumentationLink" -> None,
      "FunctionDefinitionPatterns" -> functionCallPattern,
      "FunctionInformation" -> True,
      "Context" -> context,
      "AliasContext" -> aliasContext,
      "DocComments" -> Module[{enriched},
        enriched = Function[{def},
          Module[{dc, pats, strs},
            dc = Replace[def["DocComment"], _Missing -> None];
            If[!AssociationQ[dc], Return[dc]];
            pats = Lookup[def, "InputPatterns", {}];
            strs = Select[Map[ToString[#, InputForm] &, pats], # =!= "_" && StringLength[#] > 0 &];
            If[Length[strs] > 0, Append[dc, "InputPatternStrings" -> strs], dc]
          ]
        ] /@ defs;
        DeleteDuplicates[Select[enriched, AssociationQ]]
      ],
      "InferredPattern" -> Catch[Scan[Function[{d}, Module[{ip}, ip = Lookup[d, "InferredPattern", None]; If[ip =!= None && !MatchQ[ip, _Missing], Throw[ip]]]], defs]; None]
    |>
  ]
]]


handleExternalSymbols[symIn_] :=
Catch[
Module[{usage, symContext, bareSymbol, deps, fullSymbol, pacletName, documentationLink,
  defPatterns, foundContext, aliasContext, loadedContexts, symbolLoadedQ,
  dependencyContextQ, invalid},

  invalid[] := <|
    "SymbolType" -> "INVALID",
    "Usage" -> "INVALID",
    "DocumentationLink" -> None,
    "FunctionDefinitionPatterns" -> None,
    "FunctionInformation" -> False,
    "Context" -> None
  |>;

  (* Get dependency contexts *)
  deps = GetDependencyContexts[];
  loadedContexts = GetKernelContextsCached[];

  (* Handle both bare symbols and explicitly contexted symbols *)
  If[StringContainsQ[symIn, "`"],
    (* Explicit context - extract parts and use directly *)
    bareSymbol = Last[StringSplit[symIn, "`"]];
    symContext = StringJoin[Riffle[Most[StringSplit[symIn, "`"]], "`"]] <> "`";
    fullSymbol = symIn;

    (* Resolve alias context to full context if needed (e.g. Alias` -> FullContext`) *)
    aliasContext = None;
    With[{aliasMap = GetContextAliases[]},
      If[AssociationQ[aliasMap] && KeyExistsQ[aliasMap, symContext],
        aliasContext = symContext;
        symContext = aliasMap[symContext];
        fullSymbol = symContext <> bareSymbol
      ]
    ];

    (* For explicit contexts, accept either an indexed dependency prefix or an already-loaded context.
       Do not call Needs[] here; hover must never run external package init code. *)
    dependencyContextQ = AnyTrue[deps,
      Function[{dep}, StringStartsQ[symContext, dep] || symContext === dep]
    ];
    symbolLoadedQ = Quiet[NameQ[fullSymbol]];

    If[!dependencyContextQ && !symbolLoadedQ && !MemberQ[loadedContexts, symContext],
      Throw[invalid[]]
    ],

    (* Bare symbol - try to find which dependency context it belongs to *)
    bareSymbol = symIn;

    foundContext = SelectFirst[deps, Quiet[NameQ[# <> bareSymbol]] &, None];
    symContext = foundContext;

    If[!StringQ[symContext] || symContext === "Global`" || symContext === "System`" || symContext === None,
      (* Symbol not found in any known context *)
      Throw[invalid[]]
    ];
    fullSymbol = symContext <> bareSymbol;
    symbolLoadedQ = Quiet[NameQ[fullSymbol]]
  ];

  If[!TrueQ[symbolLoadedQ],
    usage = "Symbol from " <> symContext <> " (context not loaded)";
    defPatterns = {}
    ,
    (* Only introspect already-loaded symbols. Use HoldComplete to avoid evaluating OwnValues. *)
    usage = Quiet[
      Check[
        Replace[
          ToExpression[fullSymbol, InputForm, HoldComplete],
          {
            HoldComplete[sym_Symbol] :> Quiet[
              Check[MessageName[sym, "usage"], None, {MessageName::noinfo}],
              {MessageName::noinfo}
            ],
            _ :> None
          }
        ],
        None,
        {ToExpression::sntx}
      ],
      {MessageName::noinfo, ToExpression::notstrbox, ToExpression::sntx}
    ];

    If[!StringQ[usage],
      usage = "Symbol from " <> symContext
    ];

    defPatterns = Quiet[
      Check[
        Replace[
          ToExpression[fullSymbol, InputForm, HoldComplete],
          {
            HoldComplete[sym_Symbol] :> Join[
              Cases[DownValues[sym], HoldPattern[RuleDelayed[HoldPattern[lhs_], _]] :> extractPatternString[lhs], {1}],
              Cases[UpValues[sym], HoldPattern[RuleDelayed[HoldPattern[lhs_], _]] :> extractPatternString[lhs], {1}],
              Cases[SubValues[sym], HoldPattern[RuleDelayed[HoldPattern[lhs_], _]] :> extractPatternString[lhs], {1}]
            ],
            _ :> {}
          }
        ],
        {},
        {DownValues::sym, UpValues::sym, SubValues::sym, ToExpression::sntx}
      ],
      {DownValues::sym, UpValues::sym, SubValues::sym, ToExpression::sntx}
    ]
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
    "Context" -> symContext,
    "AliasContext" -> aliasContext
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


symbolHasOnlyCurrentFileDefinitions[uri_String, symIn_String] :=
Module[{tokenSymbol, preferredContext, aliasMap, defs},
  preferredContext = None;
  tokenSymbol = If[StringContainsQ[symIn, "`"],
    With[{parts = StringSplit[symIn, "`"], ctxPart = StringJoin[Riffle[Most[StringSplit[symIn, "`"]], "`"]] <> "`"},
      aliasMap = GetContextAliases[];
      preferredContext = If[AssociationQ[aliasMap], Lookup[aliasMap, ctxPart, ctxPart], ctxPart];
      Last[parts]
    ],
    symIn
  ];
  defs = LSPServer`PacletIndex`GetVisibleSymbolDefinitions[uri, tokenSymbol, preferredContext];

  Length[defs] > 0 &&
    AllTrue[defs, Lookup[#, "uri", None] === uri &]
]

handleSymbols[id_, uri_, astIn_, cstIn_, symsIn_, cursorLine_] :=
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
      functionInformationAssoc = handleUserSymbols[
        uri,
        astIn,
        cstIn,
        Cases[symsIn, LeafNode[Symbol, sym, _], {1}],
        cursorLine
      ];
    ];
    (*
    If user symbol information is not available (INVALID or no function info found),
    try cross-file hover via PacletIndex.
    handleUserSymbols returns "UserDefined" with FunctionInformation->False when
    no definitions or usage messages are found in the current file.
    *)
    If[functionInformationAssoc["SymbolType"] == "INVALID" ||
       (functionInformationAssoc["SymbolType"] == "UserDefined" &&
         !TrueQ[functionInformationAssoc["FunctionInformation"]] &&
         !symbolHasOnlyCurrentFileDefinitions[uri, sym]),
      functionInformationAssoc = handleCrossFileSymbols[uri, sym];
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


(*
For Slot (#, #1, #2) and SlotSequence (##, ##1) tokens, infer the pattern from
the application context:
  S1  direct application:  (body &)[args]  or  Function[body][args]
        # -> pattern of args[[slotNum]];  ## -> BlankSequence[T] over all args
  S2  mapping context:  Map[body &, list]  etc.
        # -> element pattern of list;  ## -> BlankSequence version of that pattern
*)
handleSlots[id_, uri_, astIn_, cstIn_, slotsIn_, cursorLine_] :=
Catch[
Module[{slotTok, slotNum, isSequence, blankToSeq, inferredPattern, result},

  slotTok = First[slotsIn];

  (* Extract slot number and whether it's a SlotSequence *)
  {slotNum, isSequence} = Which[
    MatchQ[slotTok, CompoundNode[Slot, {_, LeafNode[Integer, n_String, _]}, _]],
      {Quiet[ToExpression[slotTok[[2, 2, 2]]], {ToExpression::shdw}], False},
    MatchQ[slotTok, CompoundNode[SlotSequence, {_, LeafNode[Integer, n_String, _]}, _]],
      {Quiet[ToExpression[slotTok[[2, 2, 2]]], {ToExpression::shdw}], True},
    StringStartsQ[Quiet[slotTok[[2]], {}], "##"],
      {1, True},
    True,  (* bare # - slot 1 *)
      {1, False}
  ];
  If[!IntegerQ[slotNum], slotNum = 1];

  (* Convert Blank[T]/Blank[] to BlankSequence[T]/BlankSequence[] for ## slots.
     Recurses into Alternatives so mixed-type lists get __String | __Integer.
     Uses Which/Head instead of Replace because Blank[t_] in a Replace rule is
     not a sub-expression pattern - Blank is special in WL pattern matching. *)
  blankToSeq = Function[{p},
    Which[
      Head[p] === Blank && Length[p] === 0, BlankSequence[],
      Head[p] === Blank && Length[p] === 1, BlankSequence[p[[1]]],
      Head[p] === Alternatives, Alternatives @@ (blankToSeq /@ List @@ p),
      True, BlankSequence[]
    ]
  ];

  inferredPattern = Catch[
    (* S1: direct application  (body &)[…]  or  Function[body][…] *)
    Scan[Function[{appNode},
      Module[{funcNode = appNode[[1]], appArgs = appNode[[2]], funcSrc, ep},
        funcSrc = Quiet[funcNode[[3, Key[Source]]]];
        If[!MatchQ[funcSrc, {{l1_, _}, {l2_, _}} /; l1 <= cursorLine <= l2], Return[]];
        If[slotNum > Length[appArgs], Return[]];
        If[isSequence,
          (* ## matches all args from slotNum onward; collect their types *)
          Module[{seqArgs = Drop[appArgs, slotNum - 1],
                  seqTypes},
            seqTypes = DeleteDuplicates[DeleteCases[
              Map[Function[a, Which[
                MatchQ[a, LeafNode[String,   _, _]], Blank[String],
                MatchQ[a, LeafNode[Integer,  _, _]], Blank[Integer],
                MatchQ[a, LeafNode[Real,     _, _]], Blank[Real],
                MatchQ[a, LeafNode[Rational, _, _]], Blank[Rational],
                True, None]], seqArgs],
              None]];
            ep = Which[
              Length[seqTypes] === 1, seqTypes[[1]],
              Length[seqTypes] > 1,   Alternatives @@ seqTypes,
              True, None
            ];
            Throw[If[ep === None, BlankSequence[], blankToSeq[ep]]]
          ],
          (* # - single slot *)
          Throw[With[{n = appArgs[[slotNum]]},
            Which[
              MatchQ[n, LeafNode[String,   _, _]], Blank[String],
              MatchQ[n, LeafNode[Integer,  _, _]], Blank[Integer],
              MatchQ[n, LeafNode[Real,     _, _]], Blank[Real],
              MatchQ[n, LeafNode[Rational, _, _]], Blank[Rational],
              True, None
            ]
          ]]
        ]
      ]],
      Cases[astIn,
        node:CallNode[
          CallNode[LeafNode[Symbol, "Function", _], {_}, _],
          _List, _] :> node,
        Infinity]
    ];

    (* S2: Map/Scan/Select/Pick[body &, list] - # gets element type of list *)
    Scan[Function[{mapNode},
      Module[{mapArgs = mapNode[[2]], func, list, funcSrc, ep},
        If[Length[mapArgs] < 2, Return[]];
        {func, list} = Switch[mapNode[[1, 2]],
          "Select" | "Pick", {mapArgs[[2]], mapArgs[[1]]},
          _,                 {mapArgs[[1]], mapArgs[[2]]}
        ];
        (* Accept both  Function[body]  (no-param pure fn)  and  body & *)
        If[!MatchQ[func, CallNode[LeafNode[Symbol, "Function", _], {_}, _]], Return[]];
        funcSrc = Quiet[func[[3, Key[Source]]]];
        If[!MatchQ[funcSrc, {{l1_, _}, {l2_, _}} /; l1 <= cursorLine <= l2], Return[]];
        If[slotNum =!= 1, Return[]];  (* simple Map uses slot 1 *)
        ep = inferListElementPattern[list];
        Throw[If[isSequence,
          If[ep === None, BlankSequence[], blankToSeq[ep]],
          If[ep === None, Blank[],         ep]
        ]]
      ]],
      Cases[astIn,
        CallNode[LeafNode[Symbol, "Map"|"Scan"|"Select"|"Pick"|"MapIndexed", _], _List, _],
        Infinity]
    ];

    None
  ];

  result = Which[
    (* DeclaredType takes precedence - show "Declared type:" label *)
    !MatchQ[declaredType, None | _Missing],
      Module[{dtLine = formatDeclaredType[declaredType, declaredTypeSource]},
        If[StringQ[dtLine],
          <|"contents" -> <|"kind" -> "markdown", "value" -> dtLine|>|>,
          Null
        ]
      ],
    (* Fall back to InferredPattern *)
    inferredPattern === None || MatchQ[inferredPattern, None | _Missing],
      Null,
    True,
      <|"contents" -> <|"kind" -> "markdown", "value" ->
        "**Inferred Pattern:** `" <>
        Quiet[ToString[inferredPattern, InputForm], {ToString::shdw}] <> "`"
      |>|>
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
formatContextLine[context_String, alias_String] :=
  "`` " <> alias <> " `` -> `` " <> context <> " ``"

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


(*
Format a single doc-comment association as a Markdown block.
Returns a string or None if the doc-comment is invalid.
*)
formatSingleDocComment[dc_Association] :=
Module[{parts, descStr, retStr, retExpr, inputStrs},
  parts = {};
  descStr   = Lookup[dc, "Description", None];
  inputStrs = Lookup[dc, "InputPatternStrings", {}];
  retStr    = Lookup[dc, "ReturnPatternString", None];
  retExpr   = Lookup[dc, "ReturnPattern", None];

  If[StringQ[descStr] && StringLength[StringTrim[descStr]] > 0,
    AppendTo[parts, escapeMarkdown[StringTrim[descStr]]]
  ];

  If[ListQ[inputStrs] && Length[inputStrs] > 0,
    AppendTo[parts,
      "**Parameters:** " <> StringRiffle["`" <> StringTrim[#] <> "`"& /@ inputStrs, ", "]]
  ];

  If[StringQ[retStr] && StringLength[StringTrim[retStr]] > 0,
    AppendTo[parts, "**Returns:** `" <> StringTrim[retStr] <> "`"]
  ];

  If[Length[parts] == 0,
    None,
    StringRiffle[parts, "  \n"]  (* soft line break between desc, params, and return *)
  ]
]

formatSingleDocComment[_] := None


(*
Format a list of doc-comment associations as a combined Markdown "Doc Comments" section.
Applies description inheritance: the first overload in the list that has a non-None
Description provides that description to all subsequent overloads whose Description is None.
Returns a string, or None if there are no displayable comments.
*)
formatDocCommentsSection[docComments_List] :=
Module[{inherited, filled, rendered, nonNone},
  (*
  Find the first non-None description in source order.
  *)
  inherited = Catch[
    Scan[
      Function[{dc},
        Module[{desc},
          desc = Lookup[dc, "Description", None];
          If[StringQ[desc] && StringLength[StringTrim[desc]] > 0,
            Throw[StringTrim[desc]]
          ]
        ]
      ],
      docComments
    ];
    None
  ];

  (*
  Fill None descriptions with the inherited description.
  *)
  filled = If[StringQ[inherited],
    Map[
      Function[{dc},
        If[AssociationQ[dc] && (Lookup[dc, "Description", None] === None || Lookup[dc, "Description", None] === ""),
          Append[dc, "Description" -> inherited],
          dc
        ]
      ],
      docComments
    ],
    docComments
  ];

  rendered = formatSingleDocComment /@ filled;
  nonNone  = Select[rendered, StringQ];
  If[Length[nonNone] == 0,
    None,
    "---\n**Doc Comments**\n\n" <> StringRiffle[nonNone, "\n\n"]
  ]
]

formatDocCommentsSection[_] := None


(*
Format a declared type annotation for display in hover markdown.
Returns a string like "**Declared type:** `_Integer`", or None.
Optional sourceFile adds "*(declared in file.ipwl)*" hint.
Parametric forms (Blank[ParametricRef[...]]) are rendered as _[ref].
*)
(*
  WL gotcha: Blank[T] in a function argument pattern matches anything with Head T, NOT the
  literal expression Blank[T]. Use Which/Head checks to avoid this.
*)
formatDeclaredType[pat_, sourceFile_:None] :=
Module[{str, hint},
  If[pat === None || MatchQ[pat, _Missing | Null],
    None,
    hint = If[StringQ[sourceFile], " *(declared in " <> sourceFile <> ")*", ""];
    Which[
      (* Parametric form: Blank[ParametricRef[ref]] *)
      Head[pat] === Blank && Length[pat] === 1 &&
        Head[pat[[1]]] === LSPServer`TypeWL`ParametricRef,
        "**Declared type:** `_[" <> ToString[pat[[1, 1]], InputForm] <> "]`" <> hint <> " *(parametric)*",
      True,
        str = Quiet[ToString[pat, InputForm], {ToString::shdw}];
        If[StringQ[str] && StringLength[str] > 0,
          "**Declared type:** `" <> str <> "`" <> hint,
          None
        ]
    ]
  ]
]

(*
resolveParametricType[pat, argPatterns]
  Given a DeclaredType (possibly Blank[ParametricRef[...]]) and a list of
  argument patterns at the call site, returns the resolved concrete pattern
  or the original pat if resolution is not possible.

  Named ref: _[name] - not resolvable without call-site AST; returns pat unchanged.
  Positional ref: _[n] - element head of arg n (1-based).
  Non-parametric: pass through.
*)
(*
  WL gotcha: cannot pattern-match on Blank[T] directly - use Which/Head instead.
  Named ref returns itself (cannot resolve without call-site context).
  Positional ref resolves from argPatterns[[n]] if possible.
  Non-parametric passes through unchanged.
*)
resolveParametricType[pat_, argPatterns_List] :=
  If[Head[pat] === Blank && Length[pat] === 1 &&
     Head[pat[[1]]] === LSPServer`TypeWL`ParametricRef,
    Module[{ref = pat[[1, 1]]},
      Which[
        (* Named ref: not resolvable at hover time without call-site context *)
        StringQ[ref],
          pat,
        (* Positional ref: direct type of arg n *)
        IntegerQ[ref],
          If[ref >= 1 && ref <= Length[argPatterns],
            argPatterns[[ref]],
            pat
          ],
        (* Two-index ref: element/part type of arg n *)
        ListQ[ref] && Length[ref] === 2 && IntegerQ[ref[[1]]] && IntegerQ[ref[[2]]],
          Module[{n = ref[[1]]},
            If[n >= 1 && n <= Length[argPatterns],
              Module[{argPat = argPatterns[[n]]},
                Which[
                  (* {___T} or {__T} -> element head T *)
                  Head[argPat] === List && Length[argPat] === 1 &&
                    (Head[argPat[[1]]] === BlankNullSequence || Head[argPat[[1]]] === BlankSequence),
                    If[Length[argPat[[1]]] === 1, Blank[argPat[[1, 1]]], Blank[]],
                  True, pat
                ]
              ],
              pat
            ]
          ],
        True, pat
      ]
    ],
    pat
  ]


(*
Format an inferred variable pattern for display in hover markdown.
Returns a string like "**Inferred Pattern:** `_Integer`", or None.
*)
formatInferredPattern[pat_] :=
Module[{str},
  If[pat === None || MatchQ[pat, _Missing | Null],
    None,
    str = Quiet[ToString[pat, InputForm], {ToString::shdw}];
    If[StringQ[str] && StringLength[str] > 0,
      "**Inferred Pattern:** `" <> str <> "`",
      None
    ]
  ]
]


formatUsageCallPatterns[assoc_] :=
Module[{res, parts, contextLine, patternsBlock, docSection, inferredPatLine},

  (* Build context line if available - show alias mapping when accessed via an alias context *)
  contextLine = With[{ctx = Lookup[assoc, "Context", None], alias = Lookup[assoc, "AliasContext", None]},
    If[StringQ[alias] && StringQ[ctx],
      formatContextLine[ctx, alias],
      formatContextLine[ctx]
    ]
  ];

  (* Build the doc-comments section (shown at the bottom) *)
  docSection = formatDocCommentsSection[Lookup[assoc, "DocComments", {}]];

  (* Declared type takes precedence over inferred pattern *)
  declaredTypeLine = formatDeclaredType[
    Lookup[assoc, "DeclaredType", None],
    Lookup[assoc, "DeclaredTypeSource", None]
  ];
  (* Build the inferred-type line for variables - suppressed when DeclaredType is present *)
  inferredPatLine = If[declaredTypeLine === None,
    formatInferredPattern[Lookup[assoc, "InferredPattern", None]],
    None
  ];

  If[Not[TrueQ[assoc["FunctionInformation"]]],
    res = "No function information."
    ,
    Switch[assoc["SymbolType"],
      "System",
        parts = {};
        If[StringQ[contextLine], AppendTo[parts, contextLine]];
        AppendTo[parts, StringJoin[{assoc["Usage"]}]];
        AppendTo[parts, "_" <> assoc["DocumentationLink"] <> "_"];
        If[StringQ[docSection], AppendTo[parts, docSection]];
        res = StringRiffle[parts, "\n\n"],
      "CrossFile",
        (* Cross-file workspace symbols *)
        parts = {};
        If[StringQ[contextLine], AppendTo[parts, contextLine]];

        If[StringQ[assoc["Usage"]],
          AppendTo[parts, StringJoin[linearToMDSyntax[assoc["Usage"]]]]
        ];

        (* Declared type (from .ipwl) takes precedence over inferred pattern *)
        If[StringQ[declaredTypeLine], AppendTo[parts, declaredTypeLine]];
        (* Inferred type for variable assignments *)
        If[StringQ[inferredPatLine], AppendTo[parts, inferredPatLine]];

        (* Definition patterns in a fenced code block *)
        patternsBlock = formatDefinitionPatterns[assoc["FunctionDefinitionPatterns"]];
        If[StringQ[patternsBlock],
          AppendTo[parts, "**Definitions**"];
          AppendTo[parts, patternsBlock]
        ];

        If[StringQ[docSection], AppendTo[parts, docSection]];

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

        If[StringQ[docSection], AppendTo[parts, docSection]];

        res = StringRiffle[parts, "\n\n"],
      _,
        (* UserDefined symbols - defined in the current file *)
        parts = {};
        If[StringQ[contextLine], AppendTo[parts, contextLine]];

        If[StringQ[assoc["Usage"]],
          AppendTo[parts, StringJoin[linearToMDSyntax[assoc["Usage"]]]]
        ];

        (* Declared type (from .ipwl) takes precedence over inferred pattern *)
        If[StringQ[declaredTypeLine], AppendTo[parts, declaredTypeLine]];
        (* Inferred type for variable assignments *)
        If[StringQ[inferredPatLine], AppendTo[parts, inferredPatLine]];

        (* Definition patterns in a fenced code block *)
        patternsBlock = formatDefinitionPatterns[assoc["FunctionDefinitionPatterns"]];
        If[StringQ[patternsBlock],
          AppendTo[parts, "**Definitions**"];
          AppendTo[parts, patternsBlock]
        ];

        If[StringQ[docSection], AppendTo[parts, docSection]];

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
