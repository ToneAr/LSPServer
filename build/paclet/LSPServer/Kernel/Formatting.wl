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

handleContent[content:KeyValuePattern["method" -> "textDocument/formatting"]] :=
Catch[
Module[{params, doc, uri, id, cst, formatted, startLineCol, endLineCol, textEdit, options, tabSize, insertSpaces,
  indentationString, entry, text, lineWidth, formattingOptions},

  If[$Debug2,
    log["textDocument/formatting: enter"]
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

  If[isStale[$ContentQueue, uri],
    
    If[$Debug2,
      log["stale"]
    ];

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

  If[$Debug2,
    log["before CodeConcreteParse"]
  ];

  cst = CodeConcreteParse[text, "TabWidth" -> tabSize];

  If[$Debug2,
    log["after CodeConcreteParse"]
  ];

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
    "LineWidth" -> lineWidth,
    (*
    Preserve existing newlines between top-level expressions
    *)
    "NewlinesBetweenTopLevelExpressions" -> $DefaultFormattingOptions["NewlinesBetweenTopLevelExpressions"]
  };

  If[$Debug2,
    log["formatting with options: ", formattingOptions]
  ];

  formatted = CodeFormatCST[cst, Sequence @@ formattingOptions];

  If[FailureQ[formatted],
    If[$Debug2,
      log["formatting failed: ", formatted]
    ];
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  textEdit = <| "range" -> <| "start" -> <| "line" -> startLineCol[[1]], "character" -> startLineCol[[2]] |>,
                              "end" ->   <| "line" -> endLineCol[[1]], "character" -> endLineCol[[2]] |> |>,
                "newText" -> formatted |>;

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> { textEdit } |>}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/rangeFormatting"]] :=
Catch[
Module[{params, doc, uri, id, formatted, textEdit, entry, text, options, tabSize,
  insertSpaces, rangeSource, lines, range, indentationString, lineWidth, formattingOptions},

  If[$Debug2,
    log["textDocument/rangeFormatting: enter"]
  ];

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    If[$Debug2,
      log["$CancelMap: ", $CancelMap]
    ];
    
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];
  
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    If[$Debug2,
      log["stale"]
    ];

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

  formatted = CodeFormat[text, Sequence @@ formattingOptions];

  If[FailureQ[formatted],
    If[$Debug2,
      log["range formatting failed: ", formatted]
    ];
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  textEdit = <| "range" -> range,
                "newText" -> formatted |>;

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> { textEdit } |>}
]]

End[]

EndPackage[]
