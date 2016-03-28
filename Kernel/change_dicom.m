(* Mathematica Source File  *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: mynameis_ *)
(* :Date: 28.03.16 *)

(* additional row in image *)
fix[im_] := Module[{dim, dat},
  dat = Transpose[imdat[im]];
  dim = Dimensions[dat][[2]];
  dat = Prepend[dat, Array[0&, dim]];
  Image@Transpose[dat]
];

(* intermediate func to fix dicom metainfo *)
changeMeta[meta_, dim_] := Module[{some},
  some = DeleteCases[meta[[All, 1]], _?(# == "Rows"\[Or]# == "Columns"&)];
  Join[
    Thread[some -> (some /. meta)],
    Thread[{"Rows", "Columns"} -> dim]
  ]
];