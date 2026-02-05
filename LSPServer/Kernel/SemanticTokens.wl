BeginPackage["LSPServer`SemanticTokens`"]

$SemanticTokenTypes

$SemanticTokenModifiers

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`PacletIndex`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Scoping`"]
Needs["CodeParser`Utils`"]


(*
Semantic token types for LSP
Extended to support paclet-wide symbol classification
*)
$SemanticTokenTypes = <|
  "variable" -> 0,        (* Local variables from Module/Block *)
  "parameter" -> 1,       (* Function parameters *)
  "function" -> 2,        (* Functions (system or paclet-defined) *)
  "constant" -> 3,        (* Constants from With or system constants *)
  "class" -> 4,           (* Paclet-defined symbols *)
  "type" -> 5,            (* System symbols *)
  "property" -> 6,        (* Options *)
  "comment" -> 7,         (* Undefined/unknown symbols *)
  "string" -> 8           (* Reserved *)
|>

$SemanticTokenModifiers = <|
  "Module" -> 0,
  "Block" -> 1,
  "With" -> 2,
  "shadowed" -> 3,
  "unused" -> 4,
  "error" -> 5,           (* Error/undefined symbol *)
  "definition" -> 6,
  "readonly" -> 7,        (* System symbol - cannot be modified *)
  "defaultLibrary" -> 8   (* System/builtin symbol *)
|>

(*
Cached sets for fast O(1) lookup
*)
$systemSymbolsSet := $systemSymbolsSet = Association[Thread[
  Join[
    WolframLanguageSyntax`Generate`$builtinFunctions,
    WolframLanguageSyntax`Generate`$constants
  ] -> True
]]

$systemConstantsSet := $systemConstantsSet = Association[Thread[
  WolframLanguageSyntax`Generate`$constants -> True
]]

$systemOptionsSet := $systemOptionsSet = Association[Thread[
  WolframLanguageSyntax`Generate`$options -> True
]]

(*
Check if a symbol is defined in the paclet
*)
isPacletSymbol[name_String] := 
  KeyExistsQ[$PacletIndex["Symbols"], name] && 
  Length[$PacletIndex["Symbols", name, "Definitions"]] > 0

(*
Check if a symbol is a system symbol
*)
isSystemSymbol[name_String] := KeyExistsQ[$systemSymbolsSet, name]

(*
Check if a symbol is a system constant
*)
isSystemConstant[name_String] := KeyExistsQ[$systemConstantsSet, name]

(*
Check if a symbol is an option
*)
isSystemOption[name_String] := KeyExistsQ[$systemOptionsSet, name]

(*
Check if a symbol is from an external loaded dependency package.
This checks the actual Context[] of the symbol at runtime.
Works for both bare symbols (e.g., "OpenSQLConnection") and 
explicitly contexted symbols (e.g., "DatabaseLink`OpenSQLConnection").
*)
isExternalDependencySymbol[name_String] :=
Module[{symContext, deps, bareSymbol, explicitContext},
  
  (* Get dependency contexts *)
  deps = GetDependencyContexts[];
  
  If[Length[deps] == 0,
    Return[False]
  ];
  
  (* Check if symbol has explicit context *)
  If[StringContainsQ[name, "`"],
    (* Extract the context from the explicit name *)
    explicitContext = StringJoin[Riffle[Most[StringSplit[name, "`"]], "`"]] <> "`";
    
    (* Check if explicit context is a dependency *)
    Return[AnyTrue[deps, StringStartsQ[explicitContext, #] || explicitContext === # &]]
  ];
  
  (* For bare symbols, try to get the symbol's context at runtime *)
  symContext = Quiet[
    Check[Context[name], None, {Context::notfound}],
    {Context::notfound}
  ];
  
  If[!StringQ[symContext] || symContext === "Global`" || symContext === "System`",
    Return[False]
  ];
  
  (* Check if symbol's context is a dependency *)
  AnyTrue[deps, StringStartsQ[symContext, #] || symContext === # &]
]

(*
Classify a symbol that's not in local scope.
Handles both bare symbols and explicitly contexted symbols.
*)
classifyGlobalSymbol[name_String] :=
Module[{bareSymbol},
  (* Extract bare symbol name for system symbol checks *)
  bareSymbol = If[StringContainsQ[name, "`"],
    Last[StringSplit[name, "`"]],
    name
  ];
  
  Which[
    (* Check system symbols using bare name *)
    isSystemConstant[bareSymbol],
      {"constant", {"readonly", "defaultLibrary"}}
    ,
    isSystemSymbol[bareSymbol],
      {"function", {"readonly", "defaultLibrary"}}
    ,
    isSystemOption[bareSymbol],
      {"property", {"readonly"}}
    ,
    (* Check paclet symbols using bare name *)
    isPacletSymbol[bareSymbol],
      {"class", {"definition"}}
    ,
    (* Check if symbol is from an external dependency package *)
    isExternalDependencySymbol[name],
      {"class", {"definition", "defaultLibrary"}}
    ,
    (* Check if it's a context (ends with `) *)
    StringEndsQ[name, "`"],
      {"type", {}}
    ,
    (* Check if it starts with $ - likely a global variable *)
    StringStartsQ[name, "$"] || StringStartsQ[bareSymbol, "$"],
      {"variable", {}}
    ,
    True,
      (* Undefined symbol - mark as comment with error modifier *)
      {"comment", {"error"}}
  ]
]


expandContent[content:KeyValuePattern["method" -> "textDocument/semanticTokens/full"], pos_] :=
Catch[
Module[{params, id, doc, uri},

  If[$Debug2,
    log["textDocument/semanticTokens/full: enter expand"]
  ];

  id = content["id"];
  params = content["params"];
  
  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    If[$Debug2,
      log["canceled"]
    ];
    
    Throw[{<| "method" -> "textDocument/semanticTokens/fullFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    If[$Debug2,
      log["stale"]
    ];

    Throw[{<| "method" -> "textDocument/semanticTokens/fullFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/aggregateParse",
    "textDocument/abstractParse",
    "textDocument/runScopingData",
    "textDocument/semanticTokens/fullFencepost"
  }
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/semanticTokens/fullFencepost"]] :=
Catch[
Module[{id, params, doc, uri, entry, semanticTokens, scopingData, cst, allSymbols,
  scopedSources, globalSymbolTokens, localTokens, transformed,
  line, char, oldLine, oldChar},

  If[$Debug2,
    log["textDocument/semanticTokens/fullFencepost: enter"]
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

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  semanticTokens = Lookup[entry, "SemanticTokens", Null];

  If[semanticTokens =!= Null,
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> <| "data" -> semanticTokens |> |>}]
  ];

  scopingData = Lookup[entry, "ScopingData", Null];
  cst = Lookup[entry, "CST", Null];

  (*
  If no scoping data, still try to provide global symbol highlighting
  *)
  If[scopingData === Null || FailureQ[scopingData],
    scopingData = {}
  ];

  (*
  Transform local scoping data (from CodeParser`Scoping`)
  Related links: https://microsoft.github.io/language-server-protocol/specification#textDocument_semanticTokens
  *)
  localTokens =
    Function[{source, scope, modifiers},
      {#[[1, 1]], #[[1, 2]], #[[2, 2]] - #[[1, 2]],
        
        $SemanticTokenTypes[
          Switch[scope,
            {___, "Module" | "Block" | "DynamicModule" | "Internal`InheritedBlock"},
              "variable"
            ,
            {___, "With"},
              "constant"
            ,
            {___, "Defined"},
              "function"
            ,
            _,
              "parameter"
          ]
        ],
        
        BitOr @@ BitShiftLeft[1, Lookup[$SemanticTokenModifiers, modifiers ~Join~ (
            Replace[scope,
              {
                "Module" | "DynamicModule" -> "Module",
                "Block" | "Internal`InheritedBlock" -> "Block",
                "With" -> "With",
                _ :> Sequence @@ {}
              }
              ,
              {1}
            ]
          ), 0]]}&[source - 1]
    ] @@@ scopingData;

  (*
  Now add global symbol highlighting (paclet symbols, system symbols, undefined)
  Only if we have CST available
  *)
  globalSymbolTokens = {};
  
  If[cst =!= Null && !FailureQ[cst],
    (*
    Get all symbol sources that are already covered by local scoping
    Use start position (line, col) as key since that uniquely identifies a token position
    *)
    scopedSources = Association[
      Table[
        {data[[1, 1, 1]], data[[1, 1, 2]]} -> True,
        {data, scopingData}
      ]
    ];
    
    (*
    Find all symbols in the CST that aren't in local scope
    *)
    allSymbols = Cases[cst,
      LeafNode[Symbol, name_String, KeyValuePattern[Source -> src_]] :> {name, src},
      Infinity
    ];
    
    (*
    Filter to symbols not in local scoping data and classify them
    *)
    globalSymbolTokens = Table[
      Module[{name, src, tokenType, modifiers, classification, startPos},
        {name, src} = sym;
        startPos = {src[[1, 1]], src[[1, 2]]};
        
        (*
        Skip if this source position is already covered by local scoping
        *)
        If[KeyExistsQ[scopedSources, startPos],
          Nothing
          ,
          (*
          Classify the symbol
          *)
          classification = classifyGlobalSymbol[name];
          tokenType = classification[[1]];
          modifiers = classification[[2]];
          
          (*
          Create token: {line, char, length, tokenType, modifierBits}
          Convert to 0-based
          *)
          {
            src[[1, 1]] - 1,
            src[[1, 2]] - 1,
            src[[2, 2]] - src[[1, 2]],
            $SemanticTokenTypes[tokenType],
            If[modifiers === {},
              0,
              BitOr @@ BitShiftLeft[1, Lookup[$SemanticTokenModifiers, modifiers, 0]]
            ]
          }
        ]
      ],
      {sym, allSymbols}
    ];
    
    (*
    Remove Nothing entries
    *)
    globalSymbolTokens = DeleteCases[globalSymbolTokens, Nothing];
  ];

  (*
  Combine local and global tokens
  *)
  transformed = Join[localTokens, globalSymbolTokens];

  (*
  Relativize the tokens (LSP requires delta encoding)
  *)
  transformed = Sort[transformed];

  line = 0;
  char = 0;

  transformed = Function[{t},
    oldLine = line;
    oldChar = char;
    line = t[[1]];
    char = t[[2]];
    {line - oldLine, If[oldLine == line, char - oldChar, char], t[[3]], t[[4]], t[[5]]}
  ] /@ transformed;

  transformed = Flatten[transformed];

  semanticTokens = transformed;
  
  entry["SemanticTokens"] = semanticTokens;

  $OpenFilesMap[uri] = entry;

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> <| "data" -> semanticTokens |> |>}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/runScopingData"]] :=
Catch[
Module[{params, doc, uri, entry, ast, scopingData},

  If[$Debug2,
    log["textDocument/runScopingData: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];
  
  scopingData = Lookup[entry, "ScopingData", Null];

  If[scopingData =!= Null,
    Throw[{}]
  ];

  ast = entry["AST"];

  If[FailureQ[ast],
    Throw[{}]
  ];

  If[$Debug2,
    log["before ScopingData"]
  ];
  
  scopingData = ScopingData[ast];

  If[$Debug2,
    log["after ScopingData"]
  ];

  entry["ScopingData"] = scopingData;

  $OpenFilesMap[uri] = entry;

  {}
]]


End[]

EndPackage[]
