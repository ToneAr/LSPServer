(* $identityOverrides entries — File System, I/O, Association/Key
   Verified against live WL kernel (2026-04-07).
   Format: "Name" -> { {{argTypes...}, "ReturnType"}, ... }

   Arg vocab : "String" "Integer" "Real" "List" "Association"
               "Symbol" "Rule" "RuleDelayed" None (any) "Type..." "..."
   Ret vocab : "_String" "_Integer" "_Real" "_List" "_Association"
               "_?BooleanQ" "_?NumericQ" "_Image" "_Audio" "_Video"
               "_Graph" "_SparseArray" "_Quantity" "_[1]"
               "_Graphics" "_Graphics3D"  None (opaque/varies)
*)

<|

(* ================================================================ *)
(* PATH MANIPULATION                                                 *)
(* ================================================================ *)

"AbsoluteFileName" -> {
  {{"String"}, "_String"}          (* canonical absolute path *)
},

"ExpandFileName" -> {
  {{"String"}, "_String"}          (* expands ~, env-vars, relative refs *)
},

"DirectoryName" -> {
  {{"String"}, "_String"},         (* parent directory *)
  {{"String", "Integer"}, "_String"} (* n levels up *)
},

"FileNameJoin" -> {
  {{"List"}, "_String"}            (* join path component list *)
},

"FileNameSplit" -> {
  {{"String"}, "_List"}            (* split path into component list *)
},

"FileNameTake" -> {
  {{"String"}, "_String"},         (* last component *)
  {{"String", "Integer"}, "_String"}, (* first/last n components *)
  {{"String", "List"}, "_String"}  (* span of components *)
},

"FileNameDrop" -> {
  {{"String"}, "_String"},
  {{"String", "Integer"}, "_String"} (* drop first/last n components *)
},

"FileBaseName" -> {
  {{"String"}, "_String"}          (* filename without extension *)
},

"FileExtension" -> {
  {{"String"}, "_String"}          (* extension without leading dot *)
},

"FileNames" -> {
  {{}, "_List"},                   (* all files in current dir *)
  {{"String"}, "_List"},           (* pattern in current dir *)
  {{"List"}, "_List"},             (* list of patterns *)
  {{"String", "String"}, "_List"}, (* pattern, dir *)
  {{"String", "List"}, "_List"},   (* pattern, list of dirs *)
  {{"String", "String", "Integer"}, "_List"}, (* pattern, dir, depth *)
  {{"String", "List", "Integer"}, "_List"}
},

(* ================================================================ *)
(* FILE METADATA & PREDICATES                                        *)
(* ================================================================ *)

"FileByteCount" -> {
  {{"String"}, "_Integer"}         (* file size in bytes *)
},

(* FileDate returns DateObject (not in vocab) — use None *)
"FileDate" -> {
  {{"String"}, None},              (* modification DateObject by default *)
  {{"String", "String"}, None}     (* specific property: "Creation", etc. *)
},

(* Default and named-method forms return Integer;
   third-arg "HexString" returns String *)
"FileHash" -> {
  {{"String"}, "_Integer"},
  {{"String", "String"}, "_Integer"},  (* explicit method *)
  {{"String", "String", "String"}, None} (* with output type arg; may be _String or ByteArray *)
},

(* Returns a List of rules (Rule[key,val]...), NOT an Association *)
"FileInformation" -> {
  {{"String"}, "_List"}
},

(* Returns a Symbol: File | Directory | SymbolicLink | None *)
"FileType" -> {
  {{"String"}, None}
},

"FindFile" -> {
  {{"String"}, "_String"}          (* full path of named file/paclet *)
},

"DirectoryQ" -> {
  {{"String"}, "_?BooleanQ"}
},

"FileExistsQ" -> {
  {{"String"}, "_?BooleanQ"}
},

(* ================================================================ *)
(* CREATE / DELETE / COPY / MOVE                                     *)
(* ================================================================ *)

"CreateDirectory" -> {
  {{}, "_String"},                  (* temp dir; returns path *)
  {{"String"}, "_String"}
},

"CreateFile" -> {
  {{}, "_String"},                  (* temp file; returns path *)
  {{"String"}, "_String"}
},

"DeleteFile" -> {
  {{"String"}, None},               (* returns Null *)
  {{"List"}, None}                  (* list of paths *)
},

"DeleteDirectory" -> {
  {{"String"}, None}                (* returns Null *)
},

"CopyFile" -> {
  {{"String", "String"}, "_String"} (* src, dst -> dst path *)
},

"MoveFile" -> {
  {{"String", "String"}, "_String"}
},

"CopyDirectory" -> {
  {{"String", "String"}, "_String"}
},

"MoveDirectory" -> {
  {{"String", "String"}, "_String"}
},

(* ================================================================ *)
(* DIRECTORY NAVIGATION                                              *)
(* ================================================================ *)

"SetDirectory" -> {
  {{"String"}, "_String"},          (* set dir, return previous *)
  {{}, "_String"}                   (* reset to $HomeDirectory *)
},

"ResetDirectory" -> {
  {{}, "_String"}                   (* pop dir stack; return restored dir *)
},

"HomeDirectory" -> {
  {{}, "_String"}
},

"ParentDirectory" -> {
  {{}, "_String"},                  (* parent of current dir *)
  {{"String"}, "_String"}           (* parent of given path *)
},

"DirectoryListing" -> {
  {{}, "_List"},                    (* files/dirs in current directory *)
  {{"String"}, "_List"}             (* files/dirs in given directory *)
},

(* ================================================================ *)
(* IMPORT / EXPORT                                                   *)
(* ================================================================ *)

(* Import return depends on format and data — use None *)
"Import" -> {
  {{"String"}, None},
  {{"String", "String"}, None},     (* explicit format *)
  {{"String", "List"}, None}        (* element specifier list *)
},

"ImportString" -> {
  {{"String", "String"}, None},     (* string, format *)
  {{"String", "String", "List"}, None}
},

"Export" -> {
  {{"String", None}, "_String"},    (* file, data -> file path *)
  {{"String", None, "String"}, "_String"} (* explicit format *)
},

"ExportString" -> {
  {{None, "String"}, "_String"},    (* data, format -> string *)
  {{None, "String", "List"}, "_String"}
},

(* ================================================================ *)
(* READING                                                           *)
(* ================================================================ *)

"ReadList" -> {
  {{"String"}, "_List"},
  {{"String", None}, "_List"},      (* with type spec *)
  {{"String", None, "Integer"}, "_List"} (* with count limit *)
},

"ReadString" -> {
  {{"String"}, "_String"},          (* entire file as string *)
  {{None}, "_String"}               (* from stream (InputStream object) *)
},

"ReadLine" -> {
  {{None}, "_String"}               (* one line from stream *)
},

"BinaryReadList" -> {
  {{"String"}, "_List"},            (* all bytes *)
  {{None}, "_List"},                (* from stream *)
  {{"String", None}, "_List"},      (* with type spec *)
  {{None, None}, "_List"},
  {{"String", None, "Integer"}, "_List"}, (* with count *)
  {{None, None, "Integer"}, "_List"}
},

(* Returns typed value (e.g. Integer for "Byte"); EndOfFile at EOF -> None *)
"BinaryRead" -> {
  {{None}, None},
  {{None, None}, None}              (* with type spec *)
},

(* ================================================================ *)
(* WRITING (side-effects, return Null)                               *)
(* ================================================================ *)

"BinaryWrite" -> {
  {{None, None}, None}
},

"Write" -> {
  {{None, "..."}, None}             (* stream, expr... *)
},

"WriteString" -> {
  {{None, "String"}, None},
  {{None, "..."}, None}             (* variadic string segments *)
},

(* ================================================================ *)
(* STREAMS                                                           *)
(* ================================================================ *)

(* OpenRead/Write/Append return InputStream/OutputStream objects — None *)
"OpenRead" -> {
  {{"String"}, None}
},

"OpenWrite" -> {
  {{}, None},                       (* temp file *)
  {{"String"}, None}
},

"OpenAppend" -> {
  {{"String"}, None}
},

(* Close returns the stream name string *)
"Close" -> {
  {{None}, "_String"}
},

"Streams" -> {
  {{}, "_List"},                    (* all open streams *)
  {{"String"}, "_List"}             (* streams matching name *)
},

(* ================================================================ *)
(* ASSOCIATION / KEY FUNCTIONS                                        *)
(* ================================================================ *)

"Association" -> {
  {{}, "_Association"},             (* empty *)
  {{"Rule"}, "_Association"},
  {{"Rule", "..."}, "_Association"}, (* variadic rules *)
  {{"List"}, "_Association"}        (* list of rules *)
},

"AssociationMap" -> {
  {{None, "List"}, "_Association"},  (* f, {keys} *)
  {{"Symbol", "List"}, "_Association"},
  {{None, "Association"}, "_Association"} (* f, assoc — maps over keys *)
},

"AssociationThread" -> {
  {{"List", "List"}, "_Association"}, (* keys, values *)
  {{"Rule"}, "_Association"},         (* keys -> values rule form *)
  {{"List"}, "_Association"}          (* list of key->val pairs *)
},

"Counts" -> {
  {{"List"}, "_Association"}         (* element -> count map *)
},

"CountsBy" -> {
  {{"List", None}, "_Association"},
  {{"List", "Symbol"}, "_Association"}
},

"GroupBy" -> {
  {{"List", None}, "_Association"},
  {{"List", "Symbol"}, "_Association"},
  {{"List", None, None}, "_Association"} (* with post-aggregation function *)
},

(* SQL-style join; returns list of merged associations *)
"JoinAcross" -> {
  {{"List", "List", "String"}, "_List"},  (* single key by name *)
  {{"List", "List", "List"}, "_List"},    (* multiple keys *)
  {{"List", "List", "String", "String"}, "_List"} (* with join type *)
},

"KeyDrop" -> {
  {{"Association", None}, "_Association"},
  {{"Association", "List"}, "_Association"},
  {{"List", None}, "_List"},              (* list-of-assocs form *)
  {{"List", "List"}, "_List"}
},

"KeyExistsQ" -> {
  {{"Association", None}, "_?BooleanQ"}
},

"KeyMap" -> {
  {{None, "Association"}, "_Association"},
  {{"Symbol", "Association"}, "_Association"}
},

"KeyMemberQ" -> {
  {{"Association", None}, "_?BooleanQ"},
  {{"List", None}, "_?BooleanQ"}          (* list-of-assocs *)
},

"KeySelect" -> {
  {{"Association", None}, "_Association"},
  {{"Association", "Symbol"}, "_Association"}
},

"KeySort" -> {
  {{"Association"}, "_Association"}       (* sort by canonical key order *)
},

"KeySortBy" -> {
  {{"Association", None}, "_Association"},
  {{"Association", "Symbol"}, "_Association"}
},

"KeyTake" -> {
  {{"Association", "List"}, "_Association"},
  {{"Association", None}, "_Association"},
  {{"List", "List"}, "_List"}             (* list-of-assocs -> list *)
},

"KeyValueMap" -> {
  {{None, "Association"}, "_List"},       (* f[k,v] for each pair *)
  {{"Symbol", "Association"}, "_List"}
},

(* Value type depends on association contents — None *)
"Lookup" -> {
  {{"Association", None}, None},
  {{"Association", None, None}, None},    (* with default value *)
  {{"List", None}, "_List"},              (* list-of-assocs -> list of values *)
  {{"List", None, None}, "_List"}
},

"Merge" -> {
  {{"List", None}, "_Association"},       (* {assoc1,...}, combiner-f *)
  {{"List", "Symbol"}, "_Association"}
}

|>
