(* ::Package:: *)

BeginPackage["CrossFileTest`"]

crossFileFunc::usage = "crossFileFunc[x] computes x squared.";
crossFileConst

Begin["`Private`"]

crossFileFunc[x_] := x^2;

crossFileFunc[x_, y_] := x^2 + y^2;

crossFileConst = 42;

helperFunc[x_] := x + 1;

End[]
EndPackage[]
