(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: mynameis_ *)
(* :Date: 31.03.16 14:31 *)



func[image_, bool_, metainfo_, col_, center_, length_] :=
    Module[{pts, cent = center},
      If[bool == 1,
        pts =
            Composition[
              Transpose,
              Prepend[#,Array[(100 - metainfo[[-3]] * col) &, Length[#[[1]]]]] &,
              Transpose,
              Map[metainfo[[-1]] * # &, #] &,
              Apply[Append],
              ComponentMeasurements[#, {"ConvexVertices", "Centroid"}][[1,2]] &,
              imdat
            ]@image;
        Join[
          pts,
          {
            Join[{(100 - metainfo[[-3]]*1)}, cent*metainfo[[-1]]],
            Join[{(100 - metainfo[[-3]]*length)}, cent*metainfo[[-1]]]
          }
        ],

        {
          Join[{(100 - metainfo[[-3]]*1)}, cent*metainfo[[-1]]],
          Join[{(100 - metainfo[[-3]]*length)}, cent*metainfo[[-1]]]
        }
      ]

    ];
