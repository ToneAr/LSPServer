BeginPackage["LSPServer`FoldingRange`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`DocumentSymbol`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


$FoldingRangeKind = <|
  (*
  pre-defined range kinds
  *)
	"Comment" -> "comment",
	"Imports" -> "imports",
	"Region" -> "region",
  (*
  custom range kinds - these map to "region" for clients that don't support custom kinds
  *)
  "Function" -> "region",
  "Block" -> "region",
  "Conditional" -> "region",
  "Loop" -> "region",
  "List" -> "region",
  "Association" -> "region"
|>


expandContent[content:KeyValuePattern["method" -> "textDocument/foldingRange"], pos_] :=
Catch[
Module[{params, id, doc, uri, res},

  log[1, "textDocument/foldingRange: enter expand"];

  id = content["id"];
  params = content["params"];
  
  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "canceled"];
    
    Throw[{<| "method" -> "textDocument/foldingRangeFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    log[2, "stale"];

    Throw[{<| "method" -> "textDocument/foldingRangeFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  res = <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/aggregateParse",
    "textDocument/abstractParse",
    "textDocument/documentNodeList",
    "textDocument/foldingRangeFencepost"
  };

  log[1, "textDocument/foldingRange: exit"];

  res
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/foldingRangeFencepost"]] :=
Catch[
Module[{id, params, doc, uri, cst, entry, foldingRange, outlineFolds, cstFolds,
  flatBag, comments, sorted, toInsert, completed, nodeList},

  log[1, "textDocument/foldingRangeFencepost: enter"];

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
    
  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];
  
  nodeList = Lookup[entry, "NodeList", Null];
  cst = Lookup[entry, "CST", Null];

  (*
  Collect folding ranges from the outline (sections, functions, etc.)
  *)
  outlineFolds = If[nodeList === Null, {}, Flatten[walkOutline /@ nodeList]];

  (*
  Collect additional folding ranges from the CST (expression blocks, comments, lists, etc.)
  *)
  cstFolds = If[cst === Null, {}, collectCSTFoldingRanges[cst]];

  (*
  Combine and deduplicate folding ranges
  *)
  foldingRange = DeleteDuplicatesBy[Join[outlineFolds, cstFolds], {#["startLine"], #["endLine"]}&];

  log[1, "textDocument/foldingRangeFencepost: exit"];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> foldingRange |>}
]]


(*
Collect folding ranges from CST for expression blocks
*)
collectCSTFoldingRanges[cst_] :=
Module[{ranges},
  ranges = Internal`Bag[];
  walkCSTForFolding[ranges, cst];
  Internal`BagPart[ranges, All]
]

(*
Walk the CST and collect multi-line expression blocks
*)
walkCSTForFolding[bag_, node_] :=
Module[{src, startLine, endLine, startChar, endChar, kind, children},
  
  (*
  Only consider nodes that span multiple lines
  *)
  If[MatchQ[node, _[_, _, KeyValuePattern[Source -> _]]],
    src = node[[3, Key[Source]]];
    startLine = src[[1, 1]];
    startChar = src[[1, 2]];
    endLine = src[[2, 1]];
    endChar = src[[2, 2]];
    
    If[endLine > startLine,
      kind = getFoldingKind[node];
      If[kind =!= None,
        Internal`StuffBag[bag, <|
          "kind" -> kind,
          "startLine" -> startLine - 1, (* Convert to 0-based *)
          "startCharacter" -> startChar - 1, (* Convert to 0-based *)
          "endLine" -> endLine - 1,
          "endCharacter" -> endChar - 1 (* Convert to 0-based *)
        |>]
      ]
    ]
  ];
  
  (*
  Recurse into children
  Different node types have different structures:
  - ContainerNode, GroupNode, InfixNode, etc.: node[[2]] is List of children
  - CallNode: node[[1]] is {head}, node[[2]] is GroupNode containing arguments
  *)
  Which[
    (* For CallNode, recurse into head list and body GroupNode's children directly *)
    (* This avoids duplicate folding ranges for CallNode and its GroupSquare body *)
    MatchQ[node, CallNode[headList_List, GroupNode[_, bodyChildren_List, _], _]],
      (* Recurse into head expressions (for complex heads like a[b][c]) *)
      Scan[walkCSTForFolding[bag, #]&, node[[1]]];
      (* Recurse directly into GroupNode's children, skipping the GroupNode itself *)
      Scan[walkCSTForFolding[bag, #]&, node[[2, 2]]],
    
    (* For most nodes, children are in node[[2]] as a List *)
    Length[node] >= 2 && ListQ[node[[2]]],
      children = node[[2]];
      Scan[walkCSTForFolding[bag, #]&, children],
    
    (* Default: no recursion *)
    True,
      Null
  ]
]

(*
Determine the folding kind based on the node type
*)

(*
Helper to get the symbol name from a CallNode head
The head is stored as either a Symbol or String depending on context
*)
getCallNodeSymbolName[CallNode[{LeafNode[Symbol, sym_, _]}, _, _]] := 
  If[StringQ[sym], sym, SymbolName[sym]]
getCallNodeSymbolName[_] := None

(* Sets of symbol names for categorization *)
$BlockSymbols = {"Module", "Block", "With", "DynamicModule", "InheritedBlock"};
$FunctionSymbols = {"Function"};
$ConditionalSymbols = {"If", "Which", "Switch", "Piecewise", "Condition"};
$LoopSymbols = {"Do", "For", "While", "Table", "Array", "Map", "MapThread", 
  "MapIndexed", "Scan", "Fold", "FoldList", "Nest", "NestList", "NestWhile", 
  "FixedPoint", "FixedPointList"};
$PatternSymbols = {"Cases", "Select", "DeleteCases", "Replace", "ReplaceAll", 
  "ReplaceRepeated", "MatchQ", "FreeQ"};
$ExceptionSymbols = {"Catch", "Check", "Quiet", "AbortProtect", "CheckAbort"};

(* Multi-line comments *)
getFoldingKind[LeafNode[Token`Comment, str_, data_]] /; 
  data[Source][[2, 1]] > data[Source][[1, 1]] := $FoldingRangeKind["Comment"]

(* CallNode-based folding - determine kind based on head symbol *)
getFoldingKind[node:CallNode[{LeafNode[Symbol, _, _]}, _, _]] := 
Module[{symName},
  symName = getCallNodeSymbolName[node];
  Which[
    MemberQ[$BlockSymbols, symName], $FoldingRangeKind["Block"],
    MemberQ[$FunctionSymbols, symName], $FoldingRangeKind["Function"],
    MemberQ[$ConditionalSymbols, symName], $FoldingRangeKind["Conditional"],
    MemberQ[$LoopSymbols, symName], $FoldingRangeKind["Loop"],
    MemberQ[$PatternSymbols, symName], $FoldingRangeKind["Block"],
    MemberQ[$ExceptionSymbols, symName], $FoldingRangeKind["Block"],
    True, $FoldingRangeKind["Region"]
  ]
]

(* Any other CallNode (complex heads, Part expressions, etc.) *)
getFoldingKind[CallNode[_, _, _]] := 
  $FoldingRangeKind["Region"]

(* CompoundExpression (;) - multi-statement blocks *)
getFoldingKind[InfixNode[CompoundExpression, _, _]] := 
  $FoldingRangeKind["Block"]

(* List literals {...} *)
getFoldingKind[GroupNode[List, _, _]] := 
  $FoldingRangeKind["List"]

(* Association literals <|...|> *)
getFoldingKind[GroupNode[Association, _, _]] := 
  $FoldingRangeKind["Association"]

(* Grouped expressions (...) that span multiple lines *)
getFoldingKind[GroupNode[GroupParen, _, _]] := 
  $FoldingRangeKind["Region"]

(* GroupNode with GroupSquare - standalone bracket groups or Part expressions [[...]] *)
getFoldingKind[GroupNode[GroupSquare, _, _]] := 
  $FoldingRangeKind["Region"]

(* SetDelayed/Set definitions - function bodies *)
getFoldingKind[BinaryNode[SetDelayed | Set | TagSetDelayed | UpSetDelayed, _, _]] := 
  $FoldingRangeKind["Function"]

(* Default: no folding for other nodes *)
getFoldingKind[_] := None



walkOutline[packageCommentNode[_, children_, data_]] :=
Catch[
Module[{walkedChildren, src},

  walkedChildren = Flatten[walkOutline /@ children];

  src = data[Source];
  src--;
  
  If[walkedChildren == {},
    Throw[{<|
      "kind" -> $FoldingRangeKind["Region"],
      "startLine" -> src[[1, 1]],
      "endLine" -> src[[2, 1]]
    |>}]
  ];

  {<|
    "kind" -> $FoldingRangeKind["Region"],
    "startLine" -> src[[1, 1]],
    "endLine" -> walkedChildren[[-1, Key["endLine"]]]
  |>, walkedChildren}
]]

walkOutline[sectionCommentNode[name_, children_, data_]] :=
Catch[
Module[{walkedChildren, src},

  walkedChildren = Flatten[walkOutline /@ children];

  src = data[Source];
  src--;
  
  If[walkedChildren == {},
    Throw[{<|
      "kind" -> $FoldingRangeKind["Region"],
      "startLine" -> src[[1, 1]],
      "endLine" -> src[[2, 1]]
    |>}]
  ];

  {<|
    "kind" -> $FoldingRangeKind["Region"],
    "startLine" -> src[[1, 1]],
    "endLine" -> walkedChildren[[-1, Key["endLine"]]]
  |>, walkedChildren}
]]

walkOutline[subsectionCommentNode[name_, children_, data_]] :=
Catch[
Module[{walkedChildren, src},

  walkedChildren = Flatten[walkOutline /@ children];

  src = data[Source];
  src--;
  
  If[walkedChildren == {},
    Throw[{<|
      "kind" -> $FoldingRangeKind["Region"],
      "startLine" -> src[[1, 1]],
      "endLine" -> src[[2, 1]]
    |>}]
  ];

  {<|
    "kind" -> $FoldingRangeKind["Region"],
    "startLine" -> src[[1, 1]],
    "endLine" -> walkedChildren[[-1, Key["endLine"]]]
  |>, walkedChildren}
]]

walkOutline[subsubsectionCommentNode[name_, children_, data_]] :=
Catch[
Module[{walkedChildren, src},

  walkedChildren = Flatten[walkOutline /@ children];

  src = data[Source];
  src--;
  
  If[walkedChildren == {},
    Throw[{<|
      "kind" -> $FoldingRangeKind["Region"],
      "startLine" -> src[[1, 1]],
      "endLine" -> src[[2, 1]]
    |>}]
  ];

  {<|
    "kind" -> $FoldingRangeKind["Region"],
    "startLine" -> src[[1, 1]],
    "endLine" -> walkedChildren[[-1, Key["endLine"]]]
  |>, walkedChildren}
]]

walkOutline[functionDefinitionNode[name_, _, data_]] :=
Catch[
Module[{src},

  src = data[Source];
  src--;
  
  <|
    "kind" -> $FoldingRangeKind["Function"],
    "startLine" -> src[[1, 1]],
    "endLine" -> src[[2, 1]]
  |>
]]

walkOutline[constantDefinitionNode[name_, _, data_]] :=
Module[{},
  {}
]

walkOutline[propertyDefinitionNode[name_, _, data_]] :=
Module[{},
  {}
]

walkOutline[titleComment[name_, _, data_]] :=
Module[{},
  {}
]

walkOutline[beginNode[name_, _, data_]] :=
Module[{},
  {}
]

walkOutline[endNode[Null, _, data_]] :=
Module[{},
  {}
]

walkOutline[beginPackageNode[name_, _, data_]] :=
Module[{},
  {}
]

walkOutline[endPackageNode[Null, _, data_]] :=
Module[{},
  {}
]

walkOutline[beginNewContextPathNode[ctxts_, _, data_]] :=
Module[{},
  {}
]

walkOutline[endNewContextPathNode[Null, _, data_]] :=
Module[{},
  {}
]

End[]

EndPackage[]
