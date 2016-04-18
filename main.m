(* Mathematica Source File  *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: mynameis_ *)
(* :Date: 28.03.16 *)

NotebookEvaluate["utils/functions.m"];
NotebookEvaluate["utils/cleaning.m"];







SetDirectory@NotebookDirectory[];


Print[" start data cleaning, fix dicom files "];
NotebookEvaluate["fix_dicom_files.m"];


Print[" start Python script to segment images "];
Run["python segment.py ."];


Print[" compute CV params for every segment, mark images valid/invalid "];
NotebookEvaluate["compute_params_and_filter.m"];





Print["Success"];
