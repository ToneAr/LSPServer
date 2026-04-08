(* Test fixture for doc-comment hover tests. *)
(* Line numbers below are 1-based; LSP positions are 0-based. *)

(*  Case 1: single overload with full doc-comment (Description + Return)  *)
(* Description: Compute the square of an integer.
 * Return: _Integer
 *)
computeSquare[x_Integer] := x * x

(*  Case 2: two overloads; first has full doc-comment, second has Return-only.
      Second overload should inherit the Description from the first.  *)


greet[name_String] := "Hello, " <> name


greet[x_] := 2;

(*  Case 3: variable assigned a literal integer  *)
myIntVar = 42

(*  Case 4: variable assigned a literal string  *)
myStrVar = "hello"

(*  Case 5: variable assigned a List literal  *)
myListVar = {1, 2, 3}

(*  Case 6: variable assigned a call to a doc-commented function.
      The inferred type should come from computeSquare's ReturnPattern.  *)

resultVar = computeSquare[5]


f[x_Integer, y_Integer] := x + y;
(* Return: _Real *)
f[{ x_Real, y_List }] := x + y;

f[KeyValuePattern[{"x" -> x_Integer, "y" -> y_Integer}]] := x + y;

f[ <| "x" -> 1 |> ]



var = Echo @ f[ { 1., 1. } ]

(* Return: _String *)
j /: Plot[ j ] := "plotting f"
a (* a should be inferred as _String *) = Plot[ j ]

b;

var2 = StringLength[ 3. ]

a = "123";

b = RandomInteger[{1, 10}];
a = If[ MatchQ[b, pattern],
	b; (* Should infer as pattern *)
	1.
	,
	b; (* Should infer as Except[pattern] *)
	2.
]
a = If[ IntegerQ[b],
	b; (* Should infer as _Integer *)
	1.
	,
	b; (* Should infer as Except[_Integer] *)
	2.
]
b = RandomInteger[{1, 10}];
a = If[ b > 5,
	b (* Should infer as _Integer?(# > 5)& *);
	b
	,
	b; (* Should infer as _Integer?(# <= 5)& *)
	2.
];
a = If[ b == 5,
	b (* Should infer as _Integer?( # == 5 &) *);
	b
	,
	b; (* Should infer as _Integer?( # != 5 &) *)
	2.
];
a; (* Should infer as _Real | _Integer?(#1 > 5 & ) *)

(* a should be inferred a _[1] | _[2], which means the inferred head of its first argument (its body) or the failure callback *)
a = Enclose[
	1 + 2,
	Function[e (* e should be inferred as _Failure *),
		e
	]
]

b = ToString @ computeSquare[ 5 ]
computeSquare[ 5. ];
computeSquare[ myStrVar ];

(*  Case 9: variable assigned from a purely builtin call  *)
builtinStrLen = StringLength[ 3 ]
builtinSort = Accumulate[ {3, 1, 2} ]

builtinEcho = Echo["world"]

a = Map[ {##} &, {1, 2., 3} ]
a = Table[ i, {i, {0, 0., 10, 1}} ]

(* Sequential re-assignment: type should reflect the MOST RECENT assignment before each use *)
seqVar = 42              (* line 70: Integer *)
computeSquare[ seqVar ]  (* line 71: seqVar is Integer -> no warn *)
seqVar = "reassigned"    (* line 72: String *)
computeSquare[ seqVar ]  (* line 73: seqVar is now String -> WARN *)

(* Case 10: DocCommentReturnMismatch - literal body type does not match declared Return *)
(* Return: _Integer *)
returnsMismatch[x_] := "wrong type"

(* Case 11: DocCommentReturnMismatch - correct literal body type, no warning *)

returnsCorrect[x_] := "ok"

a = Plus[2 + 2]

a = returnsCorrect[5]
a = Which[
	"54",                  (* Should warn *)
		43,
	computeSquare[4],      (* Should warn *)
		34,
	unknownFunc[4],        (* Should warn *)
		3,
	nonInferredFunc[3],    (* Should warn *)
		3,
	Plus[2 + 2],           (* Should warn *)
		34,
	computeSquare[4] > 10, (* Shouldn't warn *)
		34,
	True,
		"other"
];


(* Case 12: DocCommentReturnMismatch - callee return type does not match declared Return *)
(* Return: _Integer *)
returnsStrFromCall[x_] := greet[x]

(* Case 13: DocCommentReturnMismatch - callee return type matches declared Return, no warning *)
(* Return: _String *)
returnsStrFromCallOK[x_] := greet[x]

(* Case 14: DocCommentReturnMismatch - unknown callee, no warning (Missing["Unknown"]) *)
returnsUnknown[x_] := someUnknownFunction[x]

(* Module/Block/With tests *)

(* Module closure return type: body is a String literal -> inferred _String *)
modReturn = Module[{}, "hello"]

(* Module local variable: t = 42 -> inferred _Integer *)
Module[{t = 42}, t]

(* With local variable: s = "abc" -> inferred _String *)
With[{s = "abc"}, s]

(* Block closure return type: body is an Integer literal -> inferred _Integer *)
blockReturn = Block[{}, 99]
