BeginPackage["LSPServer`IgnorePatterns`"]

(*
Ignore Patterns Module for LSPServer

This module provides ESLint-style ignore patterns for diagnostics.
Users can suppress warnings/errors at different levels:

1. Line-level: (* wl-disable-line RuleName *)
   Disables the rule for the current line only

2. Next-line: (* wl-disable-next-line RuleName *)
   Disables the rule for the next line only

3. Block-level: (* wl-disable RuleName *) ... (* wl-enable RuleName *)
   Disables the rule for a block of code

4. File-level: (* wl-disable-file RuleName *) at the top of file
   Disables the rule for the entire file

5. Project-level: .wllintrc file in project root
   Configuration file for project-wide settings

Syntax:
  (* wl-disable-line *)              - Disable all rules for this line
  (* wl-disable-line RuleName *)     - Disable specific rule for this line
  (* wl-disable-line Rule1, Rule2 *) - Disable multiple rules for this line
  
  (* wl-disable-next-line *)         - Disable all rules for next line
  (* wl-disable-next-line RuleName *)

  (* wl-disable *)                   - Disable all rules until wl-enable
  (* wl-disable RuleName *)          - Disable specific rule until wl-enable
  (* wl-enable *)                    - Re-enable all rules
  (* wl-enable RuleName *)           - Re-enable specific rule

  (* wl-disable-file *)              - Disable all rules for entire file
  (* wl-disable-file RuleName *)     - Disable specific rule for file
*)

(* Public API *)
ParseIgnoreComments::usage = "ParseIgnoreComments[cst] extracts ignore directives from comments in the CST."
LoadProjectIgnoreConfig::usage = "LoadProjectIgnoreConfig[workspacePath] loads .wllintrc configuration."
ShouldIgnoreDiagnostic::usage = "ShouldIgnoreDiagnostic[diagnostic, ignoreData] returns True if the diagnostic should be ignored."
GetIgnoreData::usage = "GetIgnoreData[uri] returns the ignore data for a file."
UpdateIgnoreData::usage = "UpdateIgnoreData[uri, cst] parses and stores ignore data for a file."
ClearIgnoreData::usage = "ClearIgnoreData[uri] clears ignore data for a file."

Needs["CodeParser`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]

(*
Store ignore data per file
Structure: <|
  uri -> <|
    "LineDisables" -> <| lineNum -> {rules...} |>,
    "NextLineDisables" -> <| lineNum -> {rules...} |>,
    "BlockDisables" -> { <| "start" -> line, "end" -> line|Infinity, "rules" -> {rules...} |>, ... },
    "FileDisables" -> {rules...}
  |>
|>
*)
$IgnoreDataMap = <||>;

(*
Project-level configuration
Structure: <|
  "rules" -> <|
    "RuleName" -> "off" | "warn" | "error",
    ...
  |>,
  "ignorePatterns" -> { "pattern1", "pattern2", ... }  (* file patterns to ignore *)
|>
*)
$ProjectIgnoreConfig = <||>;

(*
Parse ignore comments from CST
Returns: <|
  "LineDisables" -> <| lineNum -> {rules...} |>,
  "NextLineDisables" -> <| lineNum -> {rules...} |>,
  "BlockDisables" -> { <| "start" -> line, "end" -> line|Infinity, "rules" -> {rules...} |>, ... },
  "FileDisables" -> {rules...}
|>
*)
ParseIgnoreComments[cst_] :=
Module[{comments, lineDisables, nextLineDisables, blockDisables, fileDisables,
  activeBlocks, comment, line, text, directive, rules},
  
  (* Extract all comments from CST *)
  comments = Cases[cst, 
    LeafNode[Token`Comment, text_String, KeyValuePattern[Source -> src_]] :> 
      <|"text" -> text, "line" -> src[[1, 1]]|>,
    Infinity
  ];
  
  lineDisables = <||>;
  nextLineDisables = <||>;
  blockDisables = {};
  fileDisables = {};
  activeBlocks = <||>;  (* rule -> startLine *)
  
  Do[
    text = comment["text"];
    line = comment["line"];
    
    (* Parse the comment for directives *)
    {directive, rules} = parseDirective[text];
    
    Switch[directive,
      (* wl-disable-line *)
      "disable-line",
        lineDisables[line] = Union[Lookup[lineDisables, line, {}], If[rules === {}, {"*"}, rules]],
      
      (* wl-disable-next-line *)
      "disable-next-line",
        nextLineDisables[line + 1] = Union[Lookup[nextLineDisables, line + 1, {}], If[rules === {}, {"*"}, rules]],
      
      (* wl-disable (block start) *)
      "disable",
        Do[
          activeBlocks[rule] = line,
          {rule, If[rules === {}, {"*"}, rules]}
        ],
      
      (* wl-enable (block end) *)
      "enable",
        Do[
          If[KeyExistsQ[activeBlocks, rule],
            AppendTo[blockDisables, <|
              "start" -> activeBlocks[rule],
              "end" -> line,
              "rules" -> {rule}
            |>];
            activeBlocks[rule] =.
          ];
          (* Also check for wildcard *)
          If[rules === {} && KeyExistsQ[activeBlocks, "*"],
            AppendTo[blockDisables, <|
              "start" -> activeBlocks["*"],
              "end" -> line,
              "rules" -> {"*"}
            |>];
            activeBlocks["*"] =.
          ],
          {rule, If[rules === {}, Keys[activeBlocks], rules]}
        ],
      
      (* wl-disable-file *)
      "disable-file",
        fileDisables = Union[fileDisables, If[rules === {}, {"*"}, rules]],
      
      (* Not a directive *)
      None,
        Null
    ],
    {comment, comments}
  ];
  
  (* Close any unclosed blocks (extend to end of file) *)
  Do[
    AppendTo[blockDisables, <|
      "start" -> activeBlocks[rule],
      "end" -> Infinity,
      "rules" -> {rule}
    |>],
    {rule, Keys[activeBlocks]}
  ];
  
  <|
    "LineDisables" -> lineDisables,
    "NextLineDisables" -> nextLineDisables,
    "BlockDisables" -> blockDisables,
    "FileDisables" -> fileDisables
  |>
]

(*
Parse a comment text for wl-disable directives
Returns: {directive, {rules...}} where directive is one of:
  "disable-line", "disable-next-line", "disable", "enable", "disable-file", None
*)
parseDirective[text_String] :=
Module[{trimmed, match, directive, rulesStr, rules},
  
  (* Remove comment delimiters *)
  trimmed = StringTrim[text];
  trimmed = StringReplace[trimmed, {
    StartOfString ~~ "(*" -> "",
    "*)" ~~ EndOfString -> ""
  }];
  trimmed = StringTrim[trimmed];
  
  (* Match wl-disable patterns *)
  (* Patterns: wl-disable-line, wl-disable-next-line, wl-disable, wl-enable, wl-disable-file *)
  
  match = StringCases[trimmed, 
    "wl-" ~~ directive:(
      "disable-next-line" | 
      "disable-line" | 
      "disable-file" |
      "disable" | 
      "enable"
    ) ~~ rest___ :> {directive, StringTrim[rest]},
    1
  ];
  
  If[match === {},
    Return[{None, {}}]
  ];
  
  {directive, rulesStr} = First[match];
  
  (* Parse rules list *)
  rules = If[rulesStr === "",
    {},
    StringTrim /@ StringSplit[rulesStr, ","]
  ];
  
  (* Filter out empty strings *)
  rules = Select[rules, StringLength[#] > 0 &];
  
  {directive, rules}
]

(*
Load project-level ignore configuration from .wllintrc
*)
LoadProjectIgnoreConfig[workspacePath_String] :=
Module[{configPath, configContent, parsed},
  
  configPath = FileNameJoin[{workspacePath, ".wllintrc"}];
  
  If[!FileExistsQ[configPath],
    (* Try .wllintrc.json *)
    configPath = FileNameJoin[{workspacePath, ".wllintrc.json"}];
  ];
  
  If[!FileExistsQ[configPath],
    $ProjectIgnoreConfig = <|"rules" -> <||>, "ignorePatterns" -> {}|>;
    Return[$ProjectIgnoreConfig]
  ];
  
  (* Read and parse the config file *)
  configContent = Quiet[Import[configPath, "Text"]];
  
  If[!StringQ[configContent],
    $ProjectIgnoreConfig = <|"rules" -> <||>, "ignorePatterns" -> {}|>;
    Return[$ProjectIgnoreConfig]
  ];
  
  (* Try to parse as JSON *)
  parsed = Quiet[ImportString[configContent, "RawJSON"]];
  
  If[!AssociationQ[parsed],
    (* Try to parse as Wolfram Language expression *)
    parsed = Quiet[ToExpression[configContent]];
  ];
  
  If[!AssociationQ[parsed],
    $ProjectIgnoreConfig = <|"rules" -> <||>, "ignorePatterns" -> {}|>;
    Return[$ProjectIgnoreConfig]
  ];
  
  $ProjectIgnoreConfig = <|
    "rules" -> Lookup[parsed, "rules", <||>],
    "ignorePatterns" -> Lookup[parsed, "ignorePatterns", {}]
  |>;
  
  $ProjectIgnoreConfig
]

(*
Check if a diagnostic should be ignored based on ignore data
InspectionObject has structure: InspectionObject[tag, message, severity, data]
*)
ShouldIgnoreDiagnostic[diagnostic_, ignoreData_] :=
Module[{tag, data, source, line, fileDisables, lineDisables, nextLineDisables, blockDisables},
  
  (* Extract diagnostic info - InspectionObject[tag, msg, severity, data] *)
  (* Tag is the first part, convert Symbol to String if needed *)
  tag = diagnostic[[1]];
  If[!StringQ[tag], tag = SymbolName[tag]];
  
  (* Get source location - handle both Source and CodeParser`Source keys *)
  data = diagnostic[[4]];
  source = Lookup[data, Source, Lookup[data, CodeParser`Source, {{1, 1}, {1, 1}}]];
  line = source[[1, 1]];
  
  (* Get ignore data components *)
  fileDisables = Lookup[ignoreData, "FileDisables", {}];
  lineDisables = Lookup[ignoreData, "LineDisables", <||>];
  nextLineDisables = Lookup[ignoreData, "NextLineDisables", <||>];
  blockDisables = Lookup[ignoreData, "BlockDisables", {}];
  
  (* Check file-level disables *)
  If[matchesRules[tag, fileDisables],
    Return[True]
  ];
  
  (* Check line-level disables *)
  If[KeyExistsQ[lineDisables, line] && matchesRules[tag, lineDisables[line]],
    Return[True]
  ];
  
  (* Check next-line disables (applied to current line) *)
  If[KeyExistsQ[nextLineDisables, line] && matchesRules[tag, nextLineDisables[line]],
    Return[True]
  ];
  
  (* Check block disables *)
  If[AnyTrue[blockDisables, 
    Function[{block},
      line >= block["start"] && line <= block["end"] && matchesRules[tag, block["rules"]]
    ]],
    Return[True]
  ];
  
  (* Check project-level config *)
  If[checkProjectConfig[tag],
    Return[True]
  ];
  
  False
]

(*
Check if a tag matches any of the rules in the list
Supports:
  - Exact match: "UndefinedSymbol"
  - Wildcard: "*" matches all
  - Prefix match: "Undefined*" matches UndefinedSymbol, UndefinedFunction, etc.
*)
matchesRules[tag_String, rules_List] :=
Module[{},
  AnyTrue[rules, matchesRule[tag, #]&]
]

matchesRule[tag_String, "*"] := True

matchesRule[tag_String, rule_String] :=
Module[{},
  If[StringEndsQ[rule, "*"],
    (* Prefix match *)
    StringStartsQ[tag, StringDrop[rule, -1]],
    (* Exact match *)
    tag === rule
  ]
]

(*
Check if a tag is disabled in project config
*)
checkProjectConfig[tag_String] :=
Module[{rules, setting, ruleKeys},
  rules = Lookup[$ProjectIgnoreConfig, "rules", <||>];
  
  (* Check exact match first *)
  If[KeyExistsQ[rules, tag],
    setting = rules[tag];
    If[setting === "off" || setting === False,
      Return[True]
    ]
  ];
  
  (* Check wildcard patterns - convert keys to strings if needed *)
  ruleKeys = ToString /@ Keys[rules];
  
  If[AnyTrue[ruleKeys, 
    Function[{rule},
      StringEndsQ[rule, "*"] && 
      StringStartsQ[tag, StringDrop[rule, -1]] && 
      (rules[rule] === "off" || rules[rule] === False)
    ]],
    Return[True]
  ];
  
  False
]

(*
Get ignore data for a file
*)
GetIgnoreData[uri_String] :=
  Lookup[$IgnoreDataMap, uri, <|
    "LineDisables" -> <||>,
    "NextLineDisables" -> <||>,
    "BlockDisables" -> {},
    "FileDisables" -> {}
  |>]

(*
Update ignore data for a file
*)
UpdateIgnoreData[uri_String, cst_] :=
Module[{ignoreData},
  ignoreData = ParseIgnoreComments[cst];
  $IgnoreDataMap[uri] = ignoreData;
  ignoreData
]

(*
Clear ignore data for a file
*)
ClearIgnoreData[uri_String] :=
Module[{},
  If[KeyExistsQ[$IgnoreDataMap, uri],
    $IgnoreDataMap[uri] =.
  ]
]

(*
Check if a file path matches any ignore pattern
*)
FileMatchesIgnorePattern[filePath_String] :=
Module[{patterns},
  patterns = Lookup[$ProjectIgnoreConfig, "ignorePatterns", {}];
  
  AnyTrue[patterns, StringMatchQ[filePath, #]&]
]

(*
Get project ignore config (for external access)
*)
GetProjectIgnoreConfig[] := $ProjectIgnoreConfig

End[]

EndPackage[]
