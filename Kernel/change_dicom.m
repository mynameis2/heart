(* Mathematica Source File  *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: mynameis_ *)
(* :Date: 26.03.16 *)

(* additional row in image *)
fix::usage = "fix[image] - add zero column from the left side to image"
fix[im_] := Module[{dim, dat},
  dat = Transpose[imdat[im]];
  dim = Dimensions[dat][[2]];
  dat = Prepend[dat, Array[0&, dim]];
  Image@Transpose[dat]
];

(* intermediate func to fix dicom metainfo *)
changeMeta::usage = "changeMeta[meta, dim] - change metainfo(arg: meta) changeMeta[{some.., Rows -> N, Columns -> M}, {M,N} ] -> {some.., Rows -> M, Columns -> N}";
changeMeta[meta_, dim_] := Module[{some},
  some = DeleteCases[meta[[All, 1]], _?(# == "Rows"\[Or]# == "Columns"&)];
  Join[
    Thread[some -> (some /. meta)],
    Thread[{"Rows", "Columns"} -> dim]
  ]
];

(*fix some dicom files(to uniform across Patient): rotate it*)
(*saxSeriesList - list of full paths to sax directory*)
rotateDICOM::usage = "rotateDICOM[saxSeriesList] in place rotate images for all dicom files in all directories in this saxSeriesList list, ex: saxSeriesList = {\"test/1109/study/sax_54\", \"test/1109/study/sax_55\", \"test/1109/study/sax_56\"}";
rotateDICOM[saxSeriesList_] := Module[{temp},
  Do[
    ParallelDo[
      (*import dicom file*)
      temp = Import[file,
        {{"BitDepth", "ColorMap", "ColorSpace", "Data", "Graphics", "GraphicsList", "Image", "ImageList", "ImageSize",
          "MetaInformation", "Overlays"}}];
      (*export dicom file*)
      Export[file,
        {
        (*rotate image counter-clock-wise*)
          {Reverse[Transpose[temp[[4, 1]]]]},
          changeMeta[temp[[10]], Reverse@temp[[9]]]
        },
        {{"Data", "MetaInformation"}}
      ];

      , {file, FileNames[__, dir]}];

    , {dir, saxSeriesList}]
];

padDICOM::usage = "padDICOM[saxSeriesList,{horizontalPad,verticalPad}, {height, width}] in place pad images to uniform image sizes in all patient sax directories.
For all dicom files in all directories in saxSeriesList list, ex: saxSeriesList = {\"test/1109/study/sax_54\", \"test/1109/study/sax_55\", \"test/1109/study/sax_56\"}
{horizontalPad,verticalPad} - positive integers, how much pad images in pixels
{height, width} - resulting image dimensions(can be calculated inside padDICOM, i know), be carefull with changeMeta function arguments";
padDICOM[saxSeriesList_,{horizontalPad_,verticalPad_}, {height_, width_}] := Module[{temp},
  Do[
    ParallelDo[
    (*import dicom*)
      tmp = Import[file,
        {{"BitDepth", "ColorMap", "ColorSpace", "Data", "Graphics",
          "GraphicsList", "Image", "ImageList", "ImageSize",
          "MetaInformation", "Overlays"}}];
      (*export dicom file*)
      Export[file,
        {
        (*pad image with 0*)
          {ArrayPad[tmp[[4, 1]], {{verticalPad, verticalPad}, {horizontalPad, horizontalPad}}]},
        (*change metainfo according to previous padding*)
          changeMeta[tmp[[10]], {height, width}]
        },
        {{"Data", "MetaInformation"}}
      ];

      , {file, FileNames[__, sax]}];

    , {sax, saxSeriesList}]
];