(* ::Package:: *)

showImageLines[image_,lines_]:=Show[image,Graphics[{Thick,Orange,Line/@ lines}]]
showImagePoints[image_,points_]:= Show[image,Graphics[{PointSize[Large],Black,Point /@ points}]]
showImagePolygon[image_,poly_]:= Show[image, Graphics[poly]]
intersectSegments[{{p1_,p2_},{p3_,p4_}}]:=  {{{p1,p2},{p3,p4}},LinearSolve[Transpose[{p1-p2,p4-p3}],p4-p2]}
inUnitInterval[x_]:=( x >= 0 && x <= 1) || Abs[x]<.01 || Abs[1-x] < .01;

getBoardBoundaryLines[boardimage_]:=Module[{k,aa,board,src,boundaryimage},
	src=ColorConvert[boardimage,"Grayscale"];
	k = MorphologicalComponents[src,.5];
	board = SelectComponents[k,"Area",-1]//Colorize;
	aa = MorphologicalPerimeter[board];
	boundaryimage=SelectComponents[MorphologicalComponents[aa],"Length",-1]// Colorize;
    ImageLines[boundaryimage]

]

computePoint[{{{p1_,p2_},{p3_,p4_}},{t_,s_}}]:= t*p1+(1-t)*p2;

findBoundaryVertices[lines_]:=Module[{segments,intersections,pointsraw},
	segments = Subsets[lines,{2}];
	intersections  =Map[intersectSegments,segments];
    points = computePoint/@ Select[intersections,inUnitInterval[#[[2]][[1]]] && inUnitInterval[#[[2]][[2]]]&];
    Map[points[[#]]&, ConvexHull[points]]
]
