(* ::Package:: *)

MaxBy[list_, fun_] := list[[First@Ordering[fun /@ list, -1]]]
MinBy[list_, fun_] := list[[First@Ordering[fun /@ list, 1]]]

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



maskBoardImage[img_,boardBoundingPoly_]:=Module[{},
	{nx,ny}=ImageDimensions[img];
	 mask=Image[Graphics[{White,poly},
                          Background->Black,
                          ImageSize->{nx,ny},
                          PlotRange->{{0,nx},{0,ny}}],
                           ColorSpace->"Grayscale"];
     ImageMultiply[img,mask]]






exportImages[dir_,rectimages_]:=Module[{i},
         Do[Export[dir <> "/rectimage_" <> ToString[i] <> ".jpg" ,
                     rectimages[[i]]],{i,1,Length[rectimages]}]
]
