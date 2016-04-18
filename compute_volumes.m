(* Mathematica Source File  *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: mynameis_ *)
(* :Date: 2016-04-18 *)

volume = Association[];
Dynamic[pid]
Do[
  Import[fj[pid, "params.mx"]];
  Import[fj[pid, "mask.mx"]];

  pos = Position[params, Missing[]];

  (params[[##]] = {1, -1, -1, -3, -2, 2, -1, 2, 2, -2, -2}) & @@@ pos;

  e = Map[classify, params, {2}];

  e[[All, -1]] = 0; e[[All, 1]] = 0;

  col = Table[Range[Length[imgrid[[1]]]], {i, Length[imgrid]}];

  meta = Table[getMetainfo[pid], {dim[imgrid][[1]]}, {dim[imgrid][[2]]}];

  cent = ComponentMeasurements[imgrid[[1, 6]], "Centroid"][[1, 2]];

  length = Length[imgrid[[1]]];

  points = ParallelMap[
    func[
      imgrid[[ #[[2]], #[[1]] ]],
      e[[ #[[2]], #[[1]] ]],
      meta[[ #[[2]], #[[1]] ]],
      col[[ #[[2]], #[[1]] ]],
      cent,
      length
    ] &,

    Table[{i, j}, {j, Length[imgrid]}, {i, Length[imgrid[[1]]]}], {2}];

  AssociateTo[
    volume,
    pid -> Quiet@Map[Volume,
      Table[DelaunayMesh[Flatten[points[[i]], 1]], {i, 1, 30, 1}]]
  ];
  , {pid, SortBy[FileNames[__], te]}
];