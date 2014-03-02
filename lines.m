(* ::Package:: *)

<<"~/Documents/mathematica app/mathgogamerecorder/utils.m"
<<ComputationalGeometry`


computePoint[{{{p1_,p2_},{p3_,p4_}},{t_,s_}}]:= t*p1+(1-t)*p2;

isHorizontal[{{x1_,y1_},{x2_,y2_}},eps_]:=Abs[y1-y2]< eps
isVertical[{{x1_,y1_},{x2_,y2_}},eps_]:=Abs[x1-x2]< eps

getBoardBoundaryLines[boardimage_]:=Module[{k,aa,board,src,boundaryimage},
	src=ColorConvert[boardimage,"Grayscale"];
	k = MorphologicalComponents[src,.5];
	board = SelectComponents[k,"Area",-1]//Colorize;
	aa = MorphologicalPerimeter[board];
	boundaryimage=SelectComponents[MorphologicalComponents[aa],"Length",-1]// Colorize;
    ImageLines[boundaryimage]

]

geoTransform[img_,boundingpoly_]:=Module[{nx,ny,pts},
   {nx,ny}=img//ImageDimensions;
   pts = {{0,0},{nx,0},{nx,ny},{0,ny}};
 FindGeometricTransform[pts,boundingpoly]]


findBoundaryVertices[lines_]:=Module[{segments,intersections,pointsraw},
	segments = Subsets[lines,{2}];
	intersections  =Map[intersectSegments,segments];
    points = computePoint/@ Select[intersections,inUnitInterval[#[[2]][[1]]] && inUnitInterval[#[[2]][[2]]]&];
    Map[points[[#]]&, ConvexHull[points]]
]

getBoardBoundingPolygon[img_]:=Module[{},
   img // getBoardBoundaryLines // findBoundaryVertices 
]


isHorizontal[{{x1_,y1_},{x2_,y2_}},eps_]:=Abs[y1-y2]< eps
isVertical[{{x1_,y1_},{x2_,y2_}},eps_]:=Abs[x1-x2]< eps

getGridBoundingPolygon[rectImage_]:=Module[{hlines,vlines,lb,rb,tb,bb},
	lines =getBoardLines[rectImage];
	hlines = Select[lines,isHorizontal[#,20]&];

	
	vlines =Select[lines,isVertical[#,20]&];
	lb = MinBy[vlines,#[[1]][[1]]&];
	rb = MaxBy[vlines,#[[1]][[1]]&];
	tb = MaxBy[hlines,#[[1]][[2]]&];
	bb= MinBy[hlines,#[[1]][[2]]&];
    findBoundaryVertices[{lb,rb,tb,bb}]


]

getBoardLines[rectImage_]:= Module[{},
   src=ColorConvert[rectImage,"Grayscale"];
   white=Closing[src,DiskMatrix[5]];
   srcAdjusted=Image[ImageData[src]/ImageData[white]];
   components=ComponentMeasurements[ColorNegate@Binarize[srcAdjusted],{"ConvexArea","Mask"}][[All,2]];
   largestComponent=Image[SortBy[components,First][[-1,2]]];
   ImageLines[EdgeDetect[largestComponent]]
]

