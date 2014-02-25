(* ::Package:: *)

getBoardLines[rectImage_]:= Module[{},
   src=ColorConvert[rectImage,"Grayscale"];
   white=Closing[src,DiskMatrix[5]];
   srcAdjusted=Image[ImageData[src]/ImageData[white]];
   components=ComponentMeasurements[ColorNegate@Binarize[srcAdjusted],{"ConvexArea","Mask"}][[All,2]];
   largestComponent=Image[SortBy[components,First][[-1,2]]];
   ImageLines[EdgeDetect[largestComponent]]
]

isHorizontal[{{x1_,y1_},{x2_,y2_}},eps_]:=Abs[y1-y2]< eps
isVertical[{{x1_,y1_},{x2_,y2_}},eps_]:=Abs[x1-x2]< eps
