(* Mathematica Source File  *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: mynameis_ *)
(* :Date: 28.03.16 *)



(* some short-names for often used functions *)
LaunchKernels[8];
SetDirectory@NotebookDirectory[];
fj[x__] := FileNameJoin[{x}];
diap = ToString /@ Join[Range[500], Range[701, 1140]];
t = AbsoluteTiming;
l = Length;
tf = TableForm;
fs = FileNameSplit;
ss = StringSplit;
te = ToExpression;
k = Keys;
v = Values;
ca = ClearAll;
sj = StringJoin;
rm = Remove;
nd = NotebookDirectory;
imdim = ImageDimensions;
ima = ImageAdjust;
dim = Dimensions;
imdat = ImageData;
cl = Colorize;
cn = ColorNegate;
ap[data_] := ArrayPlot[data, PixelConstrained -> 1];
select[list_, del_] := Pick[list, StringFreeQ[list, {__ ~~ del ~~ ___}]];



(*temp fu, from boinding boxes list to areas *)
sboxsome[box_] := box[[1]] -> Times @@ Subtract @@ box[[2]];



(* null border values *)
padnull[data2d_] := Module[{data = data2d, dim = Dimensions[data2d], size = 5},
  data[[;; size, All]] = 0.;
  data[[-size ;;, All]] = 0.;
  data[[All, ;; size]] = 0.;
  data[[All, -size ;;]] = 0.;
  data
];

(* maxbb - max bounding box, correct axes direction *)
maxbb[bin_] := Module[{pre, dim = Dimensions[bin], col, row},
  {col, row} = Transpose@Round[SortBy[ComponentMeasurements[bin, "BoundingBox"], -sboxsome[#][[2]]&][[1,2]] ];
  {dim[[1]] - Reverse[row], col}
];



(* values on border. for replace purpuses, del border components *)
border2d[dat_] := Module[{s = 5, rule},
  rule = Composition[
    Map[# -> 0&],
    Union,
    Flatten,
    dat - #&,
    ArrayPad[#, {{s}, {s}}]&,
    ArrayPad[#, {{-s}, {-s}}]&
  ]@dat;
  dat /. rule
];

border3d[dat_] := Module[{s = 3, rule},
  rule = Composition[
    Map[# -> 0&],
    Union,
    Flatten,
    dat - #&,
    ArrayPad[#, {{0}, {s}, {s}}]&,
    ArrayPad[#, {{0}, {-s}, {-s}}]&
  ]@dat;
  dat /. rule
];



(* metainfo for predict purpuse, lazy remember function result after evaluation *)
(* create several features from dicom meta-information *)
getMetainfo[PID_] := (getMetainfo[PID] = Module[{root, one, two, sax, length, a, min},
(* change root directory *)
  root = If[te@PID <= 500, "train", "test"];
  sax = select[FileNames[__, fj[root, ToString[PID], "study"]], "ch"];
  sax = SortBy[sax, te[Last[ss[#, "_"]]]&]; (*sort by sax number, reason is lexicographic order from FileNames*)
  length = Length[sax];
  sax = sax[[;; 2]];
  a = Table[{"PatientSex", "PatientAge", "Rows", "Columns", "PixelSpacing", "ImagePosition"} /. Import[FileNames[__, sax[[i]]][[1]], "MetaInformation"], {i, 3}];

  (*try to fix errors in determining between slices distance*)
  min = Sort[{EuclideanDistance[a[[1, 6]], a[[2, 6]]], EuclideanDistance[a[[1, 6]], a[[3, 6]]], EuclideanDistance[a[[2, 6]], a[[3, 6]]]}];
  min = If[min[[1]] < 1, If[min[[2]] < 1, If[min[[3]] < 1, 7, min[[3]]], min[[2]]], min[[1]]];
  If[min > 20, min /= 2;];                      (* last try to fix possible errors *)
  N@{
    a[[1, 1]] /. {"M" -> 1, "F" -> 0}, (* patient sex *)
    a[[1, 2]].{1, 1 / 12., 1 / 365.}, (* age in years *)
    Log[Times @@ (a[[1, 3 ;; 4]] * a[[1, 5]])], (* log(normilize image area in pixel^2) *)
    min, (* min image intensity *)
    length, (* sax series length, partialy equivalent to heart height *)
    a[[1, 5, 1]]                                (* pixel spacing *)
  };
]);



(* import train volumes *)
vol = Module[{csv, pid, min, max, dict},
  {pid, min, max} = Transpose[Drop[Import["train.csv", "Data"], 1]];
  dict = AssociationThread[pid, Thread[{min, max}]];
  AssociateTo[dict, "min" -> min];
  AssociateTo[dict, "max" -> max];
  dict
];



(* compute most probable left ventricle position from 2D-histogram centers position *)
(* assuming that cent - is (n x m x 2) array over all images in sax series *)
computeTrueCenter[cent_] := Module[{x, y, bin, some, dict},
  some = HistogramList[Flatten[cent, 1]];
  x = MovingAverage[N@some[[1, 1]], 2];
  y = MovingAverage[N@some[[1, 2]], 2];
  bin = some[[2]];
  dict = AssociationThread[Flatten[Outer[List, x, y], 1], Flatten[bin, 1]];
  Keys[Sort[dict]][[-1]]
];



(* compute params of 2dim image grid {{im11, im12,..},{im21,im22,..},..}(n x m) -> (n x m x 11)  *)
computeParams[imgrid_] := Module[{imDim = imdim@imgrid[[1, 1]], gridDim = dim@imgrid, params, trueCenter, cent},
  cent = Map[ComponentMeasurements[#, "Centroid"][[1, 2]]&, imgrid, {2}];
  trueCenter = computeTrueCenter[cent];
  ParallelTable[
    params = ComponentMeasurements[imgrid[[time, spat]], {"Centroid", "Count", "Count", "AreaRadiusCoverage", "BoundingDiskCoverage", "CaliperElongation", "Circularity", "Eccentricity", "Elongation"}][[1, 2]];
    params = params * {1., 1., 1. / Apply[Times, imDim], 1., 1., 1., 1., 1., 1.};
    params[[1]] = EuclideanDistance[params[[1]], trueCenter] / Sqrt[Apply[Times, imDim]];
    AppendTo[params, N[(time - 1) / (gridDim[[2]] - 1)]];
    AppendTo[params, N[(spat - 1) / (gridDim[[1]] - 1)]];
    params = If[params[[2]] < 4, Missing[], params];
    params,
    {time, gridDim[[1]]}, {spat, gridDim[[2]]}
  ]
];


(* import image grid {{im11, im12,..},{im21,im22,..},..} *)
importgrid[PID_] := Module[{root, sax, imgrid},
  root = If[PID <= 500, "train", "test"];
  sax = select[FileNames[__, fj[root, ToString[PID], "study"]], "ch"];
  sax = SortBy[sax, te[ss[#, "_"][[-1]]]&];
  imgrid = ParallelMap[ima@Import[#, "Image"]&, Map[FileNames[__, #]&, sax], {2}];
  imgrid
];



(* ROC-AUC calculate *)
(* work fast, (0.4s for 1M data length, i5 single thread, WVM compile), source is http://www.machinelearning.ru/wiki/index.php?title=ROC-%D0%BA%D1%80%D0%B8%D0%B2%D0%B0%D1%8F  *)
auc = Compile[{{train, _Integer, 1}, {guess, _Real, 1}},
  Module[{mm, mp, sorted, y, auc, tpr, fpr, some},
    y = train[[Reverse[Ordering[guess]]]];
    mp = 1 / N[Count[y, 1]];
    mm = 1 / N[Count[y, 0]];
    {fpr, tpr} = Transpose[Array[{0., 0.}&, Length[y] + 1]];
    auc = 0.;
    Do[
      If[y[[i - 1]] == 0,
        fpr[[i]] = fpr[[i - 1]] + mm;
        tpr[[i]] = tpr[[i - 1]];
        auc = auc + tpr[[i]] * mm;
        ,
        fpr[[i]] = fpr[[i - 1]];
        tpr[[i]] = tpr[[i - 1]] + mp;
      ],
      {i, 2, Length[y] + 1}
    ];
    some = Ordering[fpr];
    (*fpr=fpr[[some]];*)
    (*tpr=tpr[[some]];*)
    (*{Transpose[{fpr,tpr}],auc}*)
    auc;
  ], CompilationTarget -> If[StringFreeQ[$System, "Windows"], "C", "WVM", "WVM"]
];



(* FUNCTIONAL STYLE, twenty times slower, can not be Compiled well *)
(* 10s for 1M data length, i5 single thread, WVM compile *)
myauc[train_, guess_] := Module[{y, tpr, fpr, auc, list},
(* 4 lines do main job, without explicit cycles *)
  y = SortBy[Transpose[{train, guess}], -#[[2]]&][[All, 1]];
  tpr = Accumulate[y] / N[Count[y, 1]];
  fpr = Accumulate[y /. {1 -> 0, 0 -> 1}] / N[Count[y, 0]];
  list = Prepend[Transpose[{fpr, tpr}], {0., 0.}];
  auc = Composition[
    Total,
  (*calc areas under all point pairs*)
    Map[Apply[(#1[[2]] + #2[[2]])(#2[[1]] - #1[[1]]) / 2&]],
    Partition[#, 2, 1]&
  ]@list;
  {list, auc}
];




(* import listmax & listmin variables, *)
(* generate empirical distribution to generate CDF from it later *)
(* need for patients, which LV volume can not be determined from other methods, and only way is predict LV volume from DICOM metainfo *)

(* !!!! T!O!D!O, create .mx file with trainMetaInfo *)
(*trainMetaInfo = ParallelMap[getMetainfo, Range[500]]; // t*)


(*what kind of error we can expect, get res from cross validation parts*)
(*listmin={};listmax={};
Do[
  Module[{s, pred, true},
  *)(* split train/test *)(*
    s = RandomSample[Range[500], 400];
    stest = Complement[Range[500], s];

    minPredict = Predict[trainMetaInfo[[s]] -> vol["min"][[s]], Method -> "RandomForest"];
    maxPredict = Predict[trainMetaInfo[[s]] -> vol["max"][[s]], Method -> "RandomForest"];

    pred = minPredict /@ trainMetaInfo[[stest]];    true = vol["min"][[stest]];
    AppendTo[listmin, pred / true - 1];

    pred = maxPredict /@ trainMetaInfo[[stest]];    true = vol["max"][[stest]];
    AppendTo[listmax, pred / true - 1];
  ],{26}
];*)

(* serialize listmin&max to disk *)
(*DumpSave["montecarlo-min-max-volumes.mx",{listmin,listmax}];*)

(* import empirical data to build distribution *)
Import[fj[nd[],"montecarlo-min-max-volumes.mx"]];
minPredict = Predict[trainMetaInfo -> vol["min"], Method -> "RandomForest"];
maxPredict = Predict[trainMetaInfo -> vol["max"], Method -> "RandomForest"];

(* stupid predict CDF *)
predict[PID_] := Module[{info = getMetainfo[PID], max, min, kernel, minCDF, maxCDF},
  max = maxPredict@info;
  min = minPredict@info;
  kernel = SmoothKernelDistribution[listmax // Flatten];
  maxCDF = Table[CDF[kernel, ((x - max) / max)], {x, 0., 599., 1.}];
  kernel = SmoothKernelDistribution[listmin // Flatten];
  minCDF = Table[CDF[kernel, ((x - min) / min)], {x, 0., 599., 1.}];
  {minCDF, maxCDF}
];



(* temp function, flatten and write segmented image to csv *)
(*im2csv[imdatset_, fname_, pid_] := Module[{rows, columns, SPAT, TIME, str, header},
  {rows, columns} = dim@imdatset[[1, 1]];
  {SPAT, TIME} = Dimensions[imdatset][[;; 2]];
  header = Composition[
    StringJoin["pid,spatial,time,rows,columns,", #]&,
    Apply[StringJoin],
    Riffle[#, ","]&,
    Flatten
  ]@Table[StringJoin["r" <> ToString[r], "_", "c" <> ToString[c]], {r, rows}, {c, columns}];
  str = OpenWrite[fname];
  WriteLine[str, header];
  Do[WriteLine[str, StringJoin @@ Riffle[Map[ToString]@Join[{pid, spatial, time, rows, columns}, Flatten[imdatset[[spatial, time]]]], ","]];,
    {spatial, SPAT}, {time, TIME}];
  Close[str];
];*)
