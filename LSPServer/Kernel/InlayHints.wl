(* ::Package:: *)

BeginPackage["LSPServer`InlayHints`"]

(*
Inlay Hints for Wolfram Language

Provides Rust-style inline hints showing:
- Parameter names for function calls
- Variable contexts (System`, Global`, PackageName`, etc.)
- Expression head/pattern information
- Scoping construct annotations
- Return type hints for known functions
*)

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`PacletIndex`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


(*
Inlay Hint Kinds (LSP spec)
*)
$InlayHintKind = <|
  "Type" -> 1,      (* Type annotations *)
  "Parameter" -> 2  (* Parameter names *)
|>

(*
Maximum length for inlay hint labels to improve readability
*)
$MaxInlayHintLength = 40


(*
Known function signatures for parameter hints
Map from function name to list of parameter names
*)
$KnownFunctionSignatures = <|
  (* Core functions *)
  "Plot" -> {"f", "range", "options..."},
  "Plot3D" -> {"f", "xrange", "yrange", "options..."},
  "ListPlot" -> {"data", "options..."},
  "Table" -> {"expr", "iter..."},
  "Do" -> {"expr", "iter..."},
  "Sum" -> {"expr", "iter..."},
  "Product" -> {"expr", "iter..."},
  "Map" -> {"f", "expr", "levelspec?"},
  "MapThread" -> {"f", "lists", "levelspec?"},
  "Apply" -> {"f", "expr", "levelspec?"},
  "Fold" -> {"f", "init", "list"},
  "FoldList" -> {"f", "init", "list"},
  "NestList" -> {"f", "expr", "n"},
  "Nest" -> {"f", "expr", "n"},
  "FixedPoint" -> {"f", "expr", "max?"},
  "Select" -> {"list", "crit", "n?"},
  "Cases" -> {"expr", "pattern", "levelspec?"},
  "Position" -> {"expr", "pattern", "levelspec?"},
  "Replace" -> {"expr", "rules", "levelspec?"},
  "ReplaceAll" -> {"expr", "rules"},
  "StringReplace" -> {"string", "rules", "n?"},
  "StringCases" -> {"string", "pattern", "n?"},
  "If" -> {"cond", "then", "else?", "unknown?"},
  "Which" -> {"test1", "value1", "..."},
  "Switch" -> {"expr", "pattern1", "value1", "..."},
  "Module" -> {"vars", "body"},
  "Block" -> {"vars", "body"},
  "With" -> {"vars", "body"},
  "Function" -> {"vars", "body"},
  "Compile" -> {"vars", "expr", "options..."},
  "Part" -> {"expr", "spec..."},
  "Take" -> {"list", "spec"},
  "Drop" -> {"list", "spec"},
  "Sort" -> {"list", "ordering?"},
  "SortBy" -> {"list", "f"},
  "GroupBy" -> {"list", "f"},
  "Append" -> {"list", "elem"},
  "Prepend" -> {"list", "elem"},
  "Join" -> {"list1", "list2", "..."},
  "Flatten" -> {"list", "n?"},
  "Partition" -> {"list", "n", "d?"},
  "Thread" -> {"f", "args?"},
  "Through" -> {"expr", "head?"},
  "Outer" -> {"f", "list1", "list2", "..."},
  "Inner" -> {"f", "list1", "list2", "g?"},
  "StringJoin" -> {"strings..."},
  "StringSplit" -> {"string", "pattern?"},
  "ToExpression" -> {"input", "form?", "head?"},
  "ToString" -> {"expr", "form?"},
  "Import" -> {"file", "format?", "options..."},
  "Export" -> {"file", "expr", "format?", "options..."},
  "Read" -> {"stream", "type?"},
  "Write" -> {"stream", "expr..."},
  "OpenRead" -> {"file", "options..."},
  "OpenWrite" -> {"file", "options..."},
  "FileNameJoin" -> {"parts"},
  "FileNames" -> {"pattern?", "dir?", "depth?"},
  "Association" -> {"rules..."},
  "Lookup" -> {"assoc", "key", "default?"},
  "KeyValueMap" -> {"f", "assoc"},
  "AssociationMap" -> {"f", "list"},
  "Merge" -> {"assocs", "f"},
  "NDSolve" -> {"eqns", "vars", "range", "options..."},
  "DSolve" -> {"eqns", "vars", "indep"},
  "Solve" -> {"eqns", "vars", "domain?"},
  "NSolve" -> {"eqns", "vars", "domain?"},
  "Integrate" -> {"f", "var"},
  "NIntegrate" -> {"f", "range", "options..."},
  "D" -> {"f", "var..."},
  "Dt" -> {"f", "var?"},
  "Limit" -> {"expr", "rule"},
  "Series" -> {"f", "range"},
  "Normal" -> {"expr"},
  "Simplify" -> {"expr", "assumptions?"},
  "FullSimplify" -> {"expr", "assumptions?"},
  "Expand" -> {"expr", "pattern?"},
  "Factor" -> {"expr", "modulus?"},
  "Together" -> {"expr"},
  "Apart" -> {"expr", "var?"},
  "Cancel" -> {"expr"}
|>


(*
Return type detection using naming conventions and cached sets.
This works across all Wolfram Language versions without hardcoding.
*)

(* Cached sets for O(1) lookup - built lazily from system *)
$booleanFuncsSet := $booleanFuncsSet = Association[Thread[
  Select[Names["System`*Q"], StringLength[#] > 1 &] -> True
]]

$listFuncsSet := $listFuncsSet = Association[Thread[
  Join[
    Names["System`*List"],
    {"Range", "Table", "Array", "Keys", "Values", "Sort", "Union",
     "Intersection", "Complement", "Select", "Cases", "Position",
     "Flatten", "Partition", "Take", "Drop", "Most", "Rest", "Reverse",
     "Join", "Append", "Prepend", "DeleteCases", "DeleteDuplicates",
     "SortBy", "GatherBy", "SplitBy", "Tally", "Tuples",
     "Permutations", "Subsets", "Riffle", "Catenate", "ArrayReshape"}
  ] -> True
]]

$stringFuncsSet := $stringFuncsSet = Association[Thread[
  {"ToString", "StringJoin", "StringReplace", "StringTake",
   "StringDrop", "StringTrim", "StringReverse", "StringRepeat",
   "StringPadLeft", "StringPadRight", "StringInsert", "StringDelete",
   "ToLowerCase", "ToUpperCase", "Capitalize", "FromCharacterCode",
   "IntegerString", "CharacterName", "TextString", "StringRiffle",
   "StringReplacePart", "TemplateApply", "FileBaseName", "FileExtension",
   "DirectoryName", "FileNameTake", "FileNameDrop"} -> True
]]

$integerFuncsSet := $integerFuncsSet = Association[Thread[
  {"Length", "StringLength", "Depth", "LeafCount", "ByteCount",
   "Hash", "Floor", "Ceiling", "Round", "IntegerPart", "Quotient", "Mod",
   "BitLength", "DigitCount", "IntegerLength", "First", "Last",
   "Max", "Min", "Total", "Count", "StringCount"} -> True
]]

$assocFuncsSet := $assocFuncsSet = Association[Thread[
  {"Association", "Counts", "CountsBy", "Merge", "KeySort",
   "KeyTake", "KeyDrop", "AssociationMap", "AssociationThread",
   "PositionIndex", "GroupBy"} -> True
]]

(*
Infer return type from function name using naming conventions.
Returns None if type cannot be determined.
*)
inferReturnType[name_String] := Which[
  (* Predicates ending in Q return Boolean *)
  KeyExistsQ[$booleanFuncsSet, name], "Bool",

  (* Functions returning List *)
  KeyExistsQ[$listFuncsSet, name], "List",

  (* Functions returning String *)
  KeyExistsQ[$stringFuncsSet, name], "String",

  (* Functions returning Integer *)
  KeyExistsQ[$integerFuncsSet, name], "Int",

  (* Functions returning Association *)
  KeyExistsQ[$assocFuncsSet, name], "Assoc",

  (* Fallback: None means no hint *)
  True, None
]


(*
Cached system symbols set for context detection
*)
$systemSymbolsSet := $systemSymbolsSet = Association[Thread[
  Join[
    WolframLanguageSyntax`Generate`$builtinFunctions,
    WolframLanguageSyntax`Generate`$constants
  ] -> True
]]


(*
Truncate a label to maximum length for readability
*)
truncateLabel[label_String] :=
  If[StringLength[label] > $MaxInlayHintLength,
    StringTake[label, $MaxInlayHintLength - 1] <> "\[Ellipsis]"
    ,
    label
  ]


(*
Get the context for a symbol.
Handles:
- System` symbols (built-in functions and constants)
- Paclet-defined symbols (from workspace index)
- Dependency package symbols (from BeginPackage dependencies or Needs)
- Symbols with explicit context prefix (e.g., Developer`ToPackedArray)
- Symbols from loaded packages (checks actual Context[] at runtime)
*)
getSymbolContext[name_String] :=
Module[{explicitContext, symContext, deps},

  (*
  Check if symbol has explicit context (e.g., "Developer`ToPackedArray")
  *)
  If[StringContainsQ[name, "`"],
    (* Symbol already has context visible in code - no hint needed *)
    Return[None]
  ];

  (*
  Check if it's a System symbol
  *)
  If[KeyExistsQ[$systemSymbolsSet, name],
    Return[None]
  ];
  
  (*
  Check if symbol is defined in the paclet/workspace
  *)
  If[KeyExistsQ[$PacletIndex["Symbols"], name] &&
     Length[$PacletIndex["Symbols", name, "Definitions"]] > 0,
    Module[{defs, ctx},
      defs = $PacletIndex["Symbols", name, "Definitions"];
      ctx = defs[[1]]["context"];
      Return[If[StringQ[ctx], ctx, None]]
    ]
  ];
  
  (*
  Check if symbol is from a dependency package.
  Use Context[] to check the actual context of the symbol if it exists.
  This works for packages that have been loaded via Needs[].
  *)
  deps = GetDependencyContexts[];
  If[Length[deps] > 0,
    (* Try to get the symbol's context - will work if package is loaded *)
    symContext = Quiet[
      Check[
        Context[name],
        None,
        {Context::notfound}
      ],
      {Context::notfound}
    ];
    
    If[StringQ[symContext] && symContext =!= "Global`" && symContext =!= "System`",
      (* Check if symbol's context matches or is a subcontext of a dependency *)
      If[AnyTrue[deps, StringStartsQ[symContext, #] || symContext === # &],
        Return[symContext]
      ]
    ]
  ];
  
  (*
  Global variables starting with $ - no hint needed
  *)
  If[StringStartsQ[name, "$"],
    Return[None]
  ];
  
  (* Unknown symbol - no hint *)
  None
]


(*
Determine the pattern/head of an expression from AST
*)
getExpressionPattern[node_] :=
  Switch[node,
    (* Function call *)
    CallNode[LeafNode[Symbol, name_String, _], _, _],
      name <> "[...]"
    ,
    (* List *)
    CallNode[LeafNode[Symbol, "List", _], children_, _],
      "List[" <> ToString[Length[children]] <> "]"
    ,
    (* Association *)
    CallNode[LeafNode[Symbol, "Association", _], children_, _],
      "Association[" <> ToString[Length[children]] <> " rules]"
    ,
    (* Rule *)
    CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], _, _],
      "Rule"
    ,
    (* Pattern *)
    CallNode[LeafNode[Symbol, "Pattern", _], {LeafNode[Symbol, pname_, _], _}, _],
      pname <> "_"
    ,
    (* Blank *)
    CallNode[LeafNode[Symbol, "Blank", _], {}, _],
      "_"
    ,
    CallNode[LeafNode[Symbol, "Blank", _], {LeafNode[Symbol, head_, _]}, _],
      "_" <> head
    ,
    (* BlankSequence *)
    CallNode[LeafNode[Symbol, "BlankSequence", _], _, _],
      "__"
    ,
    (* BlankNullSequence *)
    CallNode[LeafNode[Symbol, "BlankNullSequence", _], _, _],
      "___"
    ,
    (* Integer *)
    LeafNode[Integer, _, _],
      "Integer"
    ,
    (* Real *)
    LeafNode[Real, _, _],
      "Real"
    ,
    (* String *)
    LeafNode[String, _, _],
      "String"
    ,
    (* Symbol *)
    LeafNode[Symbol, name_, _],
      "Symbol"
    ,
    _,
      None
  ]


(*
LSP handler for textDocument/inlayHint
*)
expandContent[content:KeyValuePattern["method" -> "textDocument/inlayHint"], pos_] :=
Catch[
Module[{params, id, doc, uri},

  If[$Debug2,
    log["textDocument/inlayHint: enter expand"]
  ];

  id = content["id"];
  params = content["params"];

  If[Lookup[$CancelMap, id, False],
    $CancelMap[id] =.;
    If[$Debug2, log["canceled"]];
    Throw[{<| "method" -> "textDocument/inlayHintFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
    If[$Debug2, log["stale"]];
    Throw[{<| "method" -> "textDocument/inlayHintFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/aggregateParse",
    "textDocument/abstractParse",
    "textDocument/inlayHintFencepost"
  }
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/inlayHintFencepost"]] :=
Catch[
Module[{id, params, doc, uri, entry, ast, cst, range, hints,
  startLine, startChar, endLine, endChar},

  If[$Debug2,
    log["textDocument/inlayHintFencepost: enter"]
  ];

  id = content["id"];

  (*
  If inlay hints are disabled, return empty results immediately.
  This handles the case where hints were toggled off after initialization
  but the client still sends requests (e.g. because the capability was
  advertised at startup).
  *)
  If[!TrueQ[$InlayHints],
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}]
  ];

  If[Lookup[$CancelMap, id, False],
    $CancelMap[id] =.;
    If[$Debug2, log["canceled"]];
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  range = params["range"];

  If[Lookup[content, "stale", False] || isStale[$ContentQueue, uri],
    If[$Debug2, log["stale"]];
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];

  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  ast = Lookup[entry, "AST", Null];
  cst = Lookup[entry, "CST", Null];

  If[ast === Null || FailureQ[ast],
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}]
  ];

  (*
  Extract visible range (convert from 0-based to 1-based)
  *)
  startLine = range["start"]["line"] + 1;
  startChar = range["start"]["character"] + 1;
  endLine = range["end"]["line"] + 1;
  endChar = range["end"]["character"] + 1;

  (*
  Collect all hints
  *)
  hints = {};

  (*
  1. Parameter name hints for function calls
  *)
  (* hints = Join[hints, getParameterHints[ast, {startLine, endLine}]]; *)

  (*
  2. Context hints for symbols
  *)
  hints = Join[hints, getContextHints[cst, {startLine, endLine}]];

  (*
  3. Return type hints for function calls
  *)
  hints = Join[hints, getReturnTypeHints[ast, uri, {startLine, endLine}]];

  If[$Debug2,
    log["inlayHint: returning ", Length[hints], " hints"]
  ];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> hints |>}
]]


(*
Get parameter name hints for function calls
*)
getParameterHints[ast_, {startLine_, endLine_}] :=
Module[{calls, hints},

  (*
  Find function calls in the visible range
  *)
  calls = Cases[ast,
    node:CallNode[
      LeafNode[Symbol, name_String, _],
      args_List,
      KeyValuePattern[Source -> {{line_, _}, _} /; startLine <= line <= endLine]
    ] /; KeyExistsQ[$KnownFunctionSignatures, name] && Length[args] > 0,
    Infinity
  ];

  hints = Flatten[Table[
    Module[{name, args, paramNames, argHints},
      name = call[[1, 2]];
      args = call[[2]];
      paramNames = $KnownFunctionSignatures[name];

      (*
      Create hints for each argument
      *)
      argHints = Table[
        Module[{arg, paramName, src},
          arg = args[[i]];
          paramName = If[i <= Length[paramNames],
            paramNames[[i]],
            If[StringEndsQ[paramNames[[-1]], "..."],
              StringTrim[paramNames[[-1]], "..."] <> ToString[i],
              Nothing
            ]
          ];

          If[paramName === Nothing,
            Nothing
            ,
            (*
            Get argument source position
            *)
            src = arg[[3, Key[Source]]];
            If[MissingQ[src] || !ListQ[src],
              Nothing
              ,
              <|
                "position" -> <|
                  "line" -> src[[1, 1]] - 1,
                  "character" -> src[[1, 2]] - 1
                |>,
                "label" -> paramName <> ":",
                "kind" -> $InlayHintKind["Parameter"],
                "paddingRight" -> True
              |>
            ]
          ]
        ],
        {i, Length[args]}
      ];

      DeleteCases[argHints, Nothing]
    ],
    {call, calls}
  ], 1];

  hints
]


(*
Get context hints for symbols (show context prefix)
*)
getContextHints[cst_, {startLine_, endLine_}] :=
Module[{symbols, hints},

  If[cst === Null || FailureQ[cst],
    Return[{}]
  ];

  (*
  Find all symbols in the visible range that don't already have context
  *)
  symbols = Cases[cst,
    LeafNode[Symbol, name_String, KeyValuePattern[Source -> src:{{line_, _}, _}]] /;
      startLine <= line <= endLine &&
      !StringContainsQ[name, "`"],  (* No context already *)
    Infinity
  ];

  hints = Table[
    Module[{name, src, ctx},
      name = sym[[2]];
      src = sym[[3, Key[Source]]];
      ctx = getSymbolContext[name];

      If[ctx === None || ctx === "Global`",
        Nothing
        ,
        <|
          "position" -> <|
            "line" -> src[[1, 1]] - 1,
            "character" -> src[[1, 2]] - 1
          |>,
          "label" -> truncateLabel[ctx],
          "kind" -> $InlayHintKind["Type"],
          "paddingRight" -> True
        |>
      ]
    ],
    {sym, symbols}
  ];

  DeleteCases[hints, Nothing]
]


(*
Get return type hints for function calls.
Uses DocComment ReturnPattern from the PacletIndex as primary source,
then falls back to inferReturnType (naming conventions).
*)
getReturnTypeHints[ast_, uri_String, {startLine_, endLine_}] :=
Module[{calls, hints},

  (*
  Find all function calls in the visible range
  *)
  calls = Cases[ast,
    node:CallNode[
      LeafNode[Symbol, name_String, _],
      _,
      KeyValuePattern[Source -> src:{{line_, _}, {_, endCol_}} /; startLine <= line <= endLine]
    ],
    Infinity
  ];

  hints = Table[
    Module[{name, src, retType, retStr},
      name = call[[1, 2]];
      src = call[[3, Key[Source]]];

      (*
      Primary: look up DocComment ReturnPatternString in PacletIndex.
      Try definitions in the current file first, then any file.
      *)
      retStr = Module[{allDefs, found},
        found = None;
        If[KeyExistsQ[$PacletIndex["Symbols"], name],
          allDefs = $PacletIndex["Symbols", name, "Definitions"];
          (* Prefer current-file definitions *)
          Catch[
            Scan[
              Function[{def},
                Module[{dc, rps},
                  If[def["uri"] === uri,
                    dc = Lookup[def, "DocComment", None];
                    If[AssociationQ[dc],
                      rps = Lookup[dc, "ReturnPatternString", None];
                      If[StringQ[rps] && StringLength[rps] > 0, Throw[rps]]
                    ]
                  ]
                ]
              ],
              allDefs
            ];
            (* No current-file hit: try all files *)
            Scan[
              Function[{def},
                Module[{dc, rps},
                  dc = Lookup[def, "DocComment", None];
                  If[AssociationQ[dc],
                    rps = Lookup[dc, "ReturnPatternString", None];
                    If[StringQ[rps] && StringLength[rps] > 0, Throw[rps]]
                  ]
                ]
              ],
              allDefs
            ];
            None
          ]
        ,
          None
        ]
      ];

      (*
      Secondary: naming-convention heuristic.
      Convert the short label (e.g. "Bool", "List") to a pattern string.
      *)
      If[retStr === None,
        retType = inferReturnType[name];
        retStr = Switch[retType,
          "Bool",   "_?(BooleanQ[#]&)",
          "List",   "_List",
          "String", "_String",
          "Int",    "_Integer",
          "Assoc",  "_Association",
          _,        None
        ]
      ];

      If[retStr === None,
        Nothing,
        <|
          "position" -> <|
            "line" -> src[[2, 1]] - 1,
            "character" -> src[[2, 2]] - 1
          |>,
          "label" -> " : " <> retStr,
          "kind" -> $InlayHintKind["Type"],
          "paddingLeft" -> True
        |>
      ]
    ],
    {call, calls}
  ];

  DeleteCases[hints, Nothing]
]


End[]

EndPackage[]
