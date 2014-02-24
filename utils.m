(* ::Package:: *)

showImageLines[image_,lines_]:=Show[image,Graphics[{Thick,Orange,Line/@ lines}]]

showImagePoints[image_,points_]:= Show[image,Graphics[{PointSize[Large],Black,Point /@ points}]]

showImagePolygon[image_,poly_]:= Show[image, Graphics[poly]]

intersectSegments[{{p1_,p2_},{p3_,p4_}}]:=  {{{p1,p2},{p3,p4}},LinearSolve[Transpose[{p1-p2,p4-p3}],p4-p2]}

inUnitInterval[x_]:=( x >= 0 && x <= 1) || Abs[x]<.01 || Abs[1-x] < .01;

makefilename[n_]:="image"<>IntegerString[n] <> ".jpg";

makefilenameSnow[n_]:="IMG_" <> IntegerString[n] <> ".JPG"

getfile[filename_]:=ColorConvert[Import[filename],"Grayscale"];

getfileColor[filename_]:=Import[filename];

getNImagesInDir[dir_,n_]:= Module[{filenames},
   SetDirectory[dir];
   SortBy[FileNames["*.JPG"],ToExpression[StringCases[#,RegularExpression["\\d+"]][[1]]]&] // Take[#,n]& // Map[getfileColor[#]&,#]&]

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

computePoint[{{{p1_,p2_},{p3_,p4_}},{t_,s_}}]:= t*p1+(1-t)*p2;

findBoundaryVertices[lines_]:=Module[{segments,intersections,pointsraw},
	segments = Subsets[lines,{2}];
	intersections  =Map[intersectSegments,segments];
    points = computePoint/@ Select[intersections,inUnitInterval[#[[2]][[1]]] && inUnitInterval[#[[2]][[2]]]&];
    Map[points[[#]]&, ConvexHull[points]]
]

getBoardBoundingPolygon[img_]:=Module[{},
   img // getBoardBoundaryLines // findBoundaryVertices 
]

maskBoardImage[img_,boardBoundingPoly_]:=Module[{},
	{nx,ny}=ImageDimensions[img];
	 mask=Image[Graphics[{White,poly},
                          Background->Black,
                          ImageSize->{nx,ny},
                          PlotRange->{{0,nx},{0,ny}}],
                           ColorSpace->"Grayscale"];
     ImageMultiply[img,mask]]
