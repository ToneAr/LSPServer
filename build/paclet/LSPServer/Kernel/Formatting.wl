BeginPackage["LSPServer`Formatting`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeFormatter`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


(*
Default formatting options
*)
$DefaultFormattingOptions = <|
  "LineWidth" -> 120,
  "NewlinesBetweenTopLevelExpressions" -> 2,
  "NewlinesBetweenSemicolons" -> Automatic,
  "SpaceAfterComma" -> True,
  "SpaceAfterOperator" -> True,
  "SafetyMargin" -> False
|>


lineStartOffsets[text_String] :=
Module[{newlinePositions},
  newlinePositions = StringPosition[text, "\r\n" | "\n" | "\r"];
  Prepend[newlinePositions[[All, 2]] + 1, 1]
]


sourcePointToOffset[lineStarts_List, {line_Integer, col_Integer}] :=
  lineStarts[[line]] + col - 1


sourceRangeToOffsets[text_String, src:{{_Integer, _Integer}, {_Integer, _Integer}}] :=
Module[{lineStarts},
  lineStarts = lineStartOffsets[text];
  {
    sourcePointToOffset[lineStarts, src[[1]]],
    sourcePointToOffset[lineStarts, src[[2]]]
  }
]


sourceRangeString[text_String, src:{{_Integer, _Integer}, {_Integer, _Integer}}] :=
Module[{offsets},
  offsets = sourceRangeToOffsets[text, src];
  StringTake[text, {offsets[[1]], offsets[[2]] - 1}]
]


joinLinesWithSeparators[lines_List, separators_List] :=
  If[Length[lines] <= 1,
    First[lines],
    StringJoin[Flatten[Join[Transpose[{Most[lines], separators}], {{Last[lines]}}]]]
  ]


adjustLeadingIndent[line_String, delta_Integer] :=
Module[{indentLength, dropCount},
  Which[
    delta > 0,
      StringJoin[StringRepeat[" ", delta], line],
    delta < 0,
      indentLength = StringLength[StringTakeWhile[line, WhitespaceCharacter]];
      dropCount = Min[-delta, indentLength];
      StringDrop[line, dropCount],
    True,
      line
  ]
]


commentProtectedGroupQ[
  GroupNode[List | Association, children_List, _]
] :=
  AnyTrue[
    children,
    MatchQ[
      #,
      InfixNode[
        Comma,
        commaChildren_List,
        _
      ] /; Cases[commaChildren, LeafNode[Token`Comment, _, _], Infinity] =!= {}
    ] &
  ]


commentProtectedGroupQ[_] := False


collectProtectedFormattingRanges[text_String, cst_] :=
Module[{ranges},
  ranges = Cases[
    cst,
    group:GroupNode[List | Association, _, data:KeyValuePattern[Source -> src_]] /;
      commentProtectedGroupQ[group] :>
        <|
          "Source" -> src,
          "Offsets" -> sourceRangeToOffsets[text, src],
          "OriginalText" -> sourceRangeString[text, src],
          "OriginalColumn" -> src[[1, 2]]
        |>,
    Infinity
  ];

  ranges = SortBy[ranges, {#["Offsets"][[1]] &, -#["Offsets"][[2]] &}];

  Fold[
    Function[{kept, range},
      If[
        AnyTrue[
          kept,
          range["Offsets"][[1]] >= #["Offsets"][[1]] &&
          range["Offsets"][[2]] <= #["Offsets"][[2]] &
        ],
        kept,
        Append[kept, range]
      ]
    ],
    {},
    ranges
  ]
]


protectFormattingCommentBlocks[text_String, cst_] /; FailureQ[cst] := {text, {}}


protectFormattingCommentBlocks[text_String, cst_] :=
Module[{ranges, protectedText, protectedRanges},
  protectedText = text;
  ranges = collectProtectedFormattingRanges[text, cst];

  protectedRanges = MapIndexed[
    Append[
      #1,
      "Placeholder" -> "LSPInertFormattingPlaceholder" <> ToString[#2[[1]]]
    ] &,
    ranges
  ];

  Scan[
    Function[{range},
      protectedText =
        StringTake[protectedText, range["Offsets"][[1]] - 1] <>
        range["Placeholder"] <>
        StringDrop[protectedText, range["Offsets"][[2]] - 1]
    ],
    Reverse[protectedRanges]
  ];

  {protectedText, protectedRanges}
]


placeholderColumn[text_String, placeholder_String] :=
Module[{position, starts, startOfLine},
  position = StringPosition[text, placeholder, 1];
  If[position === {},
    Return[Missing["NotFound"]]
  ];

  position = position[[1, 1]];
  starts = lineStartOffsets[text];
  startOfLine = Max[Select[starts, # <= position &]];

  position - startOfLine + 1
]


shiftProtectedBlockIndent[text_String, originalColumn_Integer, targetColumn_Integer] :=
Module[{delta, lines, separators},
  delta = targetColumn - originalColumn;
  If[delta == 0 || !StringContainsQ[text, "\r\n" | "\n" | "\r"],
    Return[text]
  ];

  lines = StringSplit[text, {"\r\n", "\n", "\r"}, All];
  separators = StringCases[text, "\r\n" | "\n" | "\r"];

  lines = Join[
    {First[lines]},
    adjustLeadingIndent[#, delta] & /@ Rest[lines]
  ];

  joinLinesWithSeparators[lines, separators]
]


restoreFormattingCommentBlocks[formatted_String, protectedRanges_List] :=
  Fold[
    Function[{acc, range},
      Module[{column, restoredText},
        column = placeholderColumn[acc, range["Placeholder"]];
        If[MissingQ[column],
          Return[acc]
        ];

        restoredText = shiftProtectedBlockIndent[
          range["OriginalText"],
          range["OriginalColumn"],
          column
        ];

        StringReplace[acc, range["Placeholder"] -> restoredText, 1]
      ]
    ],
    formatted,
    protectedRanges
  ]

handleContent[content:KeyValuePattern["method" -> "textDocument/formatting"]] :=
Catch[
Module[{params, doc, uri, id, cst, protectedText, protectedRanges, protectedCST,
  formatted, startLineCol, endLineCol, textEdit, options, tabSize, insertSpaces,
  indentationString, entry, text, lineWidth, formattingOptions},

  log[1, "textDocument/formatting: enter"];

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "canceled"];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    log[2, "stale"];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  options = params["options"];
  tabSize = options["tabSize"];
  insertSpaces = options["insertSpaces"];

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  text = entry["Text"];

  log[2, "before CodeConcreteParse"];

  cst = CodeConcreteParse[text, "TabWidth" -> tabSize];

  log[2, "after CodeConcreteParse"];

  (*
  Handle empty files gracefully
  *)
  If[Length[cst[[2]]] == 0,
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}]
  ];

  startLineCol = cst[[2, 1, 3, Key[Source], 1]];
  endLineCol = cst[[2, -1, 3, Key[Source], 2]];

  (*
  convert from 1-based to 0-based
  *)
  startLineCol--;
  endLineCol--;

  If[insertSpaces,
    indentationString = StringJoin[Table[" ", {tabSize}]]
    ,
    indentationString = "\t"
  ];

  (*
  Get line width from editor options or use default
  *)
  lineWidth = Lookup[options, "lineWidth", $DefaultFormattingOptions["LineWidth"]];

  (*
  Build formatting options
  *)
  formattingOptions = {
    "TabWidth" -> tabSize,
    "IndentationString" -> indentationString,
    "LineWidth" -> lineWidth
  };

  If[$Debug2,
    log["formatting with options: ", formattingOptions]
  ];

  {protectedText, protectedRanges} = protectFormattingCommentBlocks[text, cst];

  If[protectedRanges === {},
    formatted = CodeFormatCST[cst, Sequence @@ formattingOptions]
    ,
    protectedCST = CodeConcreteParse[protectedText, "TabWidth" -> tabSize];
    formatted = CodeFormatCST[protectedCST, Sequence @@ formattingOptions];
    If[StringQ[formatted],
      formatted = restoreFormattingCommentBlocks[formatted, protectedRanges]
    ]
  ];

  If[FailureQ[formatted],
    If[$Debug2,
      log["formatting failed: ", formatted]
    ];
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  textEdit = <| "range" -> <| "start" -> <| "line" -> startLineCol[[1]], "character" -> startLineCol[[2]] |>,
                              "end" ->   <| "line" -> endLineCol[[1]], "character" -> endLineCol[[2]] |> |>,
                "newText" -> formatted |>;

  log[1, "textDocument/formatting: exit"];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> { textEdit } |>}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/rangeFormatting"]] :=
Catch[
Module[{params, doc, uri, id, cst, protectedText, protectedRanges, formatted,
  textEdit, entry, text, options, tabSize, insertSpaces, rangeSource, lines,
  range, indentationString, lineWidth, formattingOptions},

  log[1, "textDocument/rangeFormatting: enter"];

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "$CancelMap: ", $CancelMap];
    
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];
  
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    log[2, "stale"];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];
  
  range = params["range"];
  
  rangeSource = { { range["start"]["line"], range["start"]["character"] },
                  { range["end"]["line"], range["end"]["character"] } };

  (* convert from 0-based to 1-based *)
  rangeSource+=1;

  options = params["options"];
  tabSize = options["tabSize"];
  insertSpaces = options["insertSpaces"];

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];
  
  text = entry["Text"];

  lines = StringSplit[text, {"\r\n", "\n", "\r"}, All];
  
  (*
  Validate range
  *)
  If[rangeSource[[1, 1]] > Length[lines] || rangeSource[[2, 1]] > Length[lines],
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];
  
  lines = lines[[rangeSource[[1, 1]];;rangeSource[[2, 1]]]];
  If[rangeSource[[1, 1]] == rangeSource[[2, 1]],
    (*
    single line selection
    *)
    If[rangeSource[[2, 2]] > rangeSource[[1, 2]] && rangeSource[[2, 2]] - 1 <= StringLength[lines[[1]]],
      text = StringTake[lines[[1]], {rangeSource[[1, 2]], rangeSource[[2, 2]] - 1}]
      ,
      text = ""
    ]
    ,
    (*
    multiple line selection
    *)
    If[rangeSource[[1, 2]] - 1 < StringLength[lines[[1]]],
      lines[[1]] = StringDrop[lines[[1]], rangeSource[[1, 2]] - 1]
    ];
    If[rangeSource[[2, 2]] - 1 <= StringLength[lines[[-1]]],
      lines[[-1]] = StringTake[lines[[-1]], rangeSource[[2, 2]] - 1]
    ];
    (*
    FIXME: use the correct newline
    *)
    text = StringJoin[Riffle[lines, "\n"]]
  ];

  (*
  Skip empty selections
  *)
  If[StringTrim[text] === "",
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}]
  ];

  If[insertSpaces,
    indentationString = StringJoin[Table[" ", {tabSize}]]
    ,
    indentationString = "\t"
  ];

  (*
  Get line width from editor options or use default
  *)
  lineWidth = Lookup[options, "lineWidth", $DefaultFormattingOptions["LineWidth"]];

  formattingOptions = {
    "TabWidth" -> tabSize,
    "IndentationString" -> indentationString,
    "LineWidth" -> lineWidth
  };

  cst = CodeConcreteParse[text, "TabWidth" -> tabSize];
  {protectedText, protectedRanges} = protectFormattingCommentBlocks[text, cst];

  formatted = CodeFormat[protectedText, Sequence @@ formattingOptions];

  If[StringQ[formatted] && protectedRanges =!= {},
    formatted = restoreFormattingCommentBlocks[formatted, protectedRanges]
  ];

  If[FailureQ[formatted],
    If[$Debug2,
      log["range formatting failed: ", formatted]
    ];
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  textEdit = <| "range" -> range,
                "newText" -> formatted |>;

  log[1, "textDocument/rangeFormatting: exit"];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> { textEdit } |>}
]]

End[]

EndPackage[]
