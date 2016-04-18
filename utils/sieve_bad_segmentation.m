(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: mynameis_ *)
(* :Date: 31.03.16 13:35 *)


(* load HAND-MADE binary classification for 52 patient images, ~16000 images with 0 or 1 *)
maskDict =
    Module[{files = FileNames[__, "train_clusters/csv"]},
      AssociationThread[
        ss[files, {"/", "."}][[All, -2]] // te,
        Import[#, "Data"] & /@ files
      ]
    ];


(* load computer vision params for every image among this 16k *)
(* 11 params for every image *)
paramsDict =
    AssociationThread[
      Keys[maskDict],
      Table[Import[fj["train_params", ToString[PID] <> "params.mx"]];params, {PID, Keys[maskDict]}]
    ];

(*create features - labels matrix*)
some = Flatten@Table[
  pos = Position[paramsDict[pid], Missing[]];
  Thread@Rule[
    Flatten[Delete[paramsDict[pid], pos], 1],
    Flatten[Delete[maskDict[pid], pos], 1]
  ];
  , {pid, Keys[maskDict]}];

(*CHECK ROC-AUC quality for binary classification*)
(*s = RandomSample[Range[Length[some]], 13000];*)
(*stest = Complement[Range[Length[some]], s];*)
(*classify = Classify[some[[s]], Method -> "RandomForest"];*)
(*prediction = #[1] & /@ classify[some[[stest, 1]], "Probabilities"];*)
(*auc[some[[stest, 2]], prediction]*)
(*>>>0.995662*)