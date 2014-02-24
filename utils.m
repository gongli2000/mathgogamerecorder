(* ::Package:: *)

showImageLines[image_,lines_]:=Show[image,Graphics[{Thick,Orange,Line/@ lines}]]
showImagePoints[image_,points_]:= Show[image,Graphics[{PointSize[Large],Black,Point /@ points}]]
showImagePolygon[image_,poly_]:= Show[image, Graphics[poly]]

getBoardBoundary[boardimage_]:=Module[{k,aa,board,src},
src=ColorConvert[boardimage,"Grayscale"];
k = MorphologicalComponents[src,.5];
board = SelectComponents[k,"Area",-1]//Colorize;
aa = MorphologicalPerimeter[board];
SelectComponents[MorphologicalComponents[aa],"Length",-1]// Colorize
]
