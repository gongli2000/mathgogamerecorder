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



getNImagesInDir[dir_,ext_,n_]:= Module[{filenames},
   SetDirectory[dir];
   filenames = SortBy[FileNames["*." <> ext ],ToExpression[StringCases[#,RegularExpression["\\d+"]][[1]]]&] ;
   n = If[n < 0 , Length[filenames],n];
   filenames // Take[#,n]& // Map[getfileColor[#]&,#]&]

getNImageFileNames[dir_,ext_]:= Module[{filenames},
   SetDirectory[dir];
   filenames = SortBy[FileNames["*." <> ext ],ToExpression[StringCases[#,RegularExpression["\\d+"]][[1]]]&] ;
   n = If[n < 0 , Length[filenames],n];
   filenames ]



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

exportRawImages[dir_,rectfilenames_]:=
  Do[Export[dir <> "/rectimage_" <> ToString[i] <> ".jpg" ,
                     getfileColor[rectfilenames[[i]]]],{i,1,Length[rectfilenames]}]

applyTransform[image_,boundingpoly_]:=
Module[{e,t},
  {e,t}=geoTransform[images[[1]],boundingpoly];
  Table[ImagePerspectiveTransformation[images[[k]],t,DataRange->Full]// ImageRotate // ImageResize[#,{500,520}]&,{k,1,Length[images]}]
]

cleanupimage2[img_,{x_,y_}]:= Binarize [img,{x,y}]// DeleteSmallComponents[#,Method->"Mean" ]&// ColorNegate // DeleteSmallComponents[#,10]& ;

cleanupimage[img_,{x_,y_}]:= Binarize [FillingTransform@ColorNegate@img,Method->"MinimumError"]// DeleteSmallComponents[#,Method->"Mean" ]&// ColorNegate // DeleteSmallComponents[#,10]&;

cleanupimage3[img_,{x_,y_}]:= Binarize [FillingTransform@ColorNegate@img,Method->"MinimumError"]// DeleteSmallComponents[#,Method->"Mean" ]&//ColorNegate
cleanupimage4[img_,{x_,y_}]:= Binarize [FillingTransform@img,{x,y}]//ColorNegate

blackstones[img_]:=Binarize[img,{.3,1}]//ColorNegate//Closing[#,3]& //ColorNegate;
whitestones[img_]:=Binarize[img,{0,.9}]//ColorNegate;

diffImage2[img_,colorfn_,i_,j_]:= ImageDifference[img[[j]]//colorfn,img[[i]]//colorfn ];

exportRectImages[dir_,images_]:=Module[{i},
Do[Export[dir <> "/rectimage_" <> ToString[i] <> ".jpg" ,
 GraphicsGrid[{
{images[[i]],images[[i+1]]}, 
{diffImage2[images,whitestones,i,i+1] ,diffImage2[images,blackstones,i,i+1] }},
ImageSize->{400,400}]],
{i,1,Length[images]-5}]
]

manipulateImages[images_]:=Module[{i},
Manipulate[
 GraphicsGrid[{
{images[[i]],images[[i+1]]}, 
{diffImage2[images,whitestones,i,i+1] ,diffImage2[images,blackstones,i,i+1] }}],
{i,1,Length[images]-5}]]
