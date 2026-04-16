(* ::Package:: *)

BeginPackage["LSPServer`TypeWL`"]

PreprocessIPWL

ParametricRef  (* Head used to represent parametric type references, e.g. Blank[ParametricRef["in"]] *)

Begin["`Private`"]

(*
PreprocessIPWL[source_String]
  Pre-processes an .ipwl source string into valid WL.
  Returns {transformedSource, annotationList} where:
    - transformedSource is valid WL (function annotations stripped to plain :=)
    - annotationList is a List of Associations, each one of:
        <| "kind" -> "DeclaredType", "symbol" -> name, "DeclaredType" -> patExpr,
           "DeclaredInputPatterns" -> {}, "lhsStr" -> lhsStr, "line" -> n |>
      OR
        <| "kind" -> "IPWLSyntaxError", "message" -> str, "line" -> n |>

  Transformation rules:
    f[args]: ret := body  ->  f[args] := body  (in-place, DeclaredType extracted)
    f[args]: ret          ->  (* IPWLDeclare: f[args] / ret *)  (DeclaredType extracted)
    var: pat = val        ->  unchanged (already valid WL, DeclaredType extracted)
    f /: h[f[args]]: ret := body  ->  f /: h[f[args]] := body  (in-place, DeclaredType extracted)
*)
PreprocessIPWL[source_String] :=
Module[{lines, outLines, annotations, lineIdx, lineStr, trimmed,
        funcAnnotationRe, tagSetAnnotationRe, varAnnotationRe, declOnlyRe,
        match, retStr, retExpr, symName, argsStr},

  lines = StringSplit[source, {"\r\n", "\n", "\r"}, All];
  outLines = lines;
  annotations = {};
  lineIdx = 0;

  funcAnnotationRe    = RegularExpression["^(\\w+\\[.*?\\])\\s*:\\s*(.+?)\\s*(:=|=)(.*)$"];
  tagSetAnnotationRe  = RegularExpression["^(\\w+\\s*/:\\s*.+?\\])\\s*:\\s*(.+?)\\s*(:=|=)(.*)$"];
  varAnnotationRe     = RegularExpression["^(\\w+)\\s*:\\s*(.+?)\\s*=(.*)$"];
  declOnlyRe          = RegularExpression["^(\\w+(?:\\[.*?\\])?)\\s*:\\s*(.+)$"];

  Do[
    lineStr = lines[[lineIdx]];
    trimmed = StringTrim[lineStr];

    If[StringStartsQ[trimmed, "(*"],
      Continue[]
    ];

    Which[
      (* TagSet annotation: sym /: head[...]: ret := body *)
      StringMatchQ[trimmed, tagSetAnnotationRe],
        match   = StringCases[trimmed, tagSetAnnotationRe -> {"$1", "$2", "$3", "$4"}][[1]];
        retStr  = StringTrim[match[[2]]];
        retExpr = parseAnnotationPattern[retStr, lineIdx];
        If[AssociationQ[retExpr],
          AppendTo[annotations, retExpr],
          (* Replace annotation with spaces to preserve column positions of := and body *)
          Module[{lhsLen = StringLength[match[[1]]],
                  suffixPos = StringLength[trimmed] - StringLength[match[[3]]] - StringLength[match[[4]]] + 1,
                  indent = First[StringCases[lineStr, RegularExpression["^\\s*"]], ""]},
            outLines[[lineIdx]] = indent <> StringTake[trimmed, lhsLen] <>
                                  StringRepeat[" ", suffixPos - 1 - lhsLen] <>
                                  StringDrop[trimmed, suffixPos - 1]
          ];
          symName = StringCases[match[[1]], RegularExpression["^(\\w+)"] -> "$1"][[1]];
          AppendTo[annotations, <|"kind" -> "DeclaredType", "symbol" -> symName,
            "DeclaredType" -> retExpr, "DeclaredInputPatterns" -> {}, "lhsStr" -> match[[1]], "line" -> lineIdx|>]
        ],

      (* Function annotation with body: f[args]: ret := body *)
      StringMatchQ[trimmed, funcAnnotationRe],
        match   = StringCases[trimmed, funcAnnotationRe -> {"$1", "$2", "$3", "$4"}][[1]];
        retStr  = StringTrim[match[[2]]];
        retExpr = parseAnnotationPattern[retStr, lineIdx];
        If[AssociationQ[retExpr],
          AppendTo[annotations, retExpr],
          (* Replace annotation with spaces to preserve column positions of := and body *)
          Module[{lhsLen = StringLength[match[[1]]],
                  suffixPos = StringLength[trimmed] - StringLength[match[[3]]] - StringLength[match[[4]]] + 1,
                  indent = First[StringCases[lineStr, RegularExpression["^\\s*"]], ""]},
            outLines[[lineIdx]] = indent <> StringTake[trimmed, lhsLen] <>
                                  StringRepeat[" ", suffixPos - 1 - lhsLen] <>
                                  StringDrop[trimmed, suffixPos - 1]
          ];
          symName = StringCases[match[[1]], RegularExpression["^(\\w+)"] -> "$1"][[1]];
          AppendTo[annotations, <|"kind" -> "DeclaredType", "symbol" -> symName,
            "DeclaredType" -> retExpr, "DeclaredInputPatterns" -> {},
            "lhsStr" -> match[[1]], "line" -> lineIdx|>]
        ],

      (* Typed assignment: var: pat = val  - keep source unchanged, extract DeclaredType *)
      StringMatchQ[trimmed, varAnnotationRe] &&
          !StringContainsQ[trimmed, ":="] &&
          !StringStartsQ[trimmed, "("],
        match   = StringCases[trimmed, varAnnotationRe -> {"$1", "$2", "$3"}][[1]];
        retStr  = StringTrim[match[[2]]];
        retExpr = parseAnnotationPattern[retStr, lineIdx];
        If[!AssociationQ[retExpr],
          (* Source line unchanged - valid WL; just record the DeclaredType *)
          symName = match[[1]];
          AppendTo[annotations, <|"kind" -> "DeclaredType", "symbol" -> symName,
            "DeclaredType" -> retExpr, "DeclaredInputPatterns" -> {},
            "lhsStr" -> symName, "line" -> lineIdx|>]
        ],

      (* Declaration-only: f[args]: ret  (no := or = on this line) *)
      StringMatchQ[trimmed, declOnlyRe] &&
          !StringContainsQ[trimmed, ":="] &&
          !StringContainsQ[trimmed, "="] &&
          !StringStartsQ[trimmed, "("],
        match   = StringCases[trimmed, declOnlyRe -> {"$1", "$2"}][[1]];
        argsStr = match[[1]];
        retStr  = StringTrim[match[[2]]];
        retExpr = parseAnnotationPattern[retStr, lineIdx];
        If[AssociationQ[retExpr],
          AppendTo[annotations, retExpr],
          outLines[[lineIdx]] = "(* IPWLDeclare: " <> argsStr <> " / " <> retStr <> " *)";
          symName = StringCases[argsStr, RegularExpression["^(\\w+)"] -> "$1"][[1]];
          AppendTo[annotations, <|"kind" -> "DeclaredType", "symbol" -> symName,
            "DeclaredType" -> retExpr, "DeclaredInputPatterns" -> {},
            "lhsStr" -> argsStr, "line" -> lineIdx|>]
        ],

      True, Null
    ],
    {lineIdx, 1, Length[lines]}
  ];

  {StringRiffle[outLines, "\n"], annotations}
]

parseAnnotationPattern[str_String, lineNum_Integer] :=
Module[{expr, pat},
  Which[
    (* Parametric: _[n, m] - type of m-th part of n-th arg *)
    StringMatchQ[str, RegularExpression["_\\[(\\d+),\\s*(\\d+)\\]"]],
      With[{parts = StringCases[str, RegularExpression["_\\[(\\d+),\\s*(\\d+)\\]"] -> {"$1", "$2"}][[1]]},
        Blank[ParametricRef[{ToExpression[parts[[1]]], ToExpression[parts[[2]]]}]]
      ],
    (* Parametric: _[identifier] or _[integer] *)
    StringMatchQ[str, RegularExpression["_\\[(\\w+)\\]"]],
      With[{inner = StringCases[str, RegularExpression["_\\[(\\w+)\\]"] -> "$1"][[1]]},
        If[StringMatchQ[inner, RegularExpression["\\d+"]],
          Blank[ParametricRef[ToExpression[inner]]],
          Blank[ParametricRef[inner]]
        ]
      ],
    True,
      expr = Quiet[
        Check[
          ToExpression[str, InputForm, HoldComplete],
          None,
          {Syntax::sntxi, Syntax::sntxb, ToExpression::sntx}
        ]
      ];
      If[MatchQ[expr, HoldComplete[_]],
        pat = First[expr];
        (* Reject bare atoms (integers, reals, strings) as invalid patterns *)
        If[IntegerQ[pat] || MatchQ[pat, _Real] || StringQ[pat] || MatchQ[pat, True | False],
          <|"kind" -> "IPWLSyntaxError",
            "message" -> "Invalid pattern in annotation (bare literal is not a pattern): " <> str,
            "line" -> lineNum|>,
          pat
        ],
        <|"kind" -> "IPWLSyntaxError",
          "message" -> "Invalid pattern in annotation: " <> str,
          "line" -> lineNum|>
      ]
  ]
]

End[]
EndPackage[]
