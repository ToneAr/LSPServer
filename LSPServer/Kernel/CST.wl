BeginPackage["LSPServer`CST`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["CodeParser`"]


(* ------------------------------------------------------------------ *)
(*  cstToJSON — recursively convert a CodeParser CST to a plain       *)
(*  association tree that ExportString[..., "JSON"] can serialize.     *)
(* ------------------------------------------------------------------ *)

cstToJSON[ContainerNode[kind_, children_, meta_]] :=
  <|"type" -> "ContainerNode",
    "kind" -> ToString[kind],
    "children" -> Map[cstToJSON, children],
    "source" -> meta[Source]|>

cstToJSON[CallNode[head_, children_, meta_]] :=
  <|"type" -> "CallNode",
    "head" -> cstToJSON[head],
    "children" -> Map[cstToJSON, children],
    "source" -> meta[Source]|>

cstToJSON[LeafNode[kind_, value_, meta_]] :=
  <|"type" -> "LeafNode",
    "kind" -> ToString[kind],
    "value" -> value,
    "source" -> meta[Source]|>

cstToJSON[InfixNode[op_, children_, meta_]] :=
  <|"type" -> "InfixNode",
    "op" -> ToString[op],
    "children" -> Map[cstToJSON, children],
    "source" -> meta[Source]|>

cstToJSON[BinaryNode[op_, children_, meta_]] :=
  <|"type" -> "BinaryNode",
    "op" -> ToString[op],
    "children" -> Map[cstToJSON, children],
    "source" -> meta[Source]|>

cstToJSON[PrefixNode[op_, children_, meta_]] :=
  <|"type" -> "PrefixNode",
    "op" -> ToString[op],
    "children" -> Map[cstToJSON, children],
    "source" -> meta[Source]|>

cstToJSON[PostfixNode[op_, children_, meta_]] :=
  <|"type" -> "PostfixNode",
    "op" -> ToString[op],
    "children" -> Map[cstToJSON, children],
    "source" -> meta[Source]|>

cstToJSON[CompoundNode[op_, children_, meta_]] :=
  <|"type" -> "CompoundNode",
    "op" -> ToString[op],
    "children" -> Map[cstToJSON, children],
    "source" -> meta[Source]|>

cstToJSON[GroupNode[kind_, children_, meta_]] :=
  <|"type" -> "GroupNode",
    "kind" -> ToString[kind],
    "children" -> Map[cstToJSON, children],
    "source" -> meta[Source]|>

cstToJSON[other_] :=
  <|"type" -> "Unknown", "wl" -> ToString[other, InputForm]|>


(* ------------------------------------------------------------------ *)
(*  wolfram/cst request handler                                        *)
(*                                                                     *)
(*  Accepts either an open file URI (reads from $OpenFilesMap cache)  *)
(*  or a raw source string passed as params["source"].                 *)
(* ------------------------------------------------------------------ *)

handleContent[content : KeyValuePattern["method" -> "wolfram/cst"]] :=
Catch[
Module[{params, id, uri, source, tabSize, cst, entry, json},

  log[1, "wolfram/cst: enter"];

  id = content["id"];
  params = content["params"];
  uri = Lookup[params["textDocument"], "uri", Null];
  source = Lookup[params, "source", Null];
  tabSize = Lookup[params, "tabSize", 2];

  (* Prefer cached CST from open file *)
  Which[
    uri =!= Null && KeyExistsQ[$OpenFilesMap, uri],
      entry = $OpenFilesMap[uri];
      cst = Lookup[entry, "CST", Null];
      If[cst === Null,
        cst = CodeConcreteParse[entry["Text"], "TabWidth" -> tabSize]
      ],

    source =!= Null,
      cst = CodeConcreteParse[source, "TabWidth" -> tabSize],

    True,
      Throw[{<|"jsonrpc" -> "2.0", "id" -> id,
               "error" -> <|"code" -> -32602,
                            "message" -> "wolfram/cst requires textDocument.uri or source"|>|>}]
  ];

  If[FailureQ[cst],
    Throw[{<|"jsonrpc" -> "2.0", "id" -> id, "result" -> Null|>}]
  ];

  json = ExportString[cstToJSON[cst], "JSON"];

  log[1, "wolfram/cst: exit"];

  {<|"jsonrpc" -> "2.0", "id" -> id, "result" -> json|>}
]]


End[]

EndPackage[]
