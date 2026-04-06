(* Builtin variadic pattern test fixture *)

(** Return: _Integer *)
addThreeInts[x_Integer, y_Integer, z_Integer] := Plus[x, y, z]

(** Return: _String *)
joinThreeStrings[s1_String, s2_String, s3_String] := StringJoin[s1, s2, s3]

(** Return: _?BooleanQ *)
andThree[a_, b_, c_] := And[a, b, c]

(** Return: _Integer *)
sumList[xs_List] := Total[xs]
