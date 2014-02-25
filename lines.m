(* ::Package:: *)

getBoardLines[rectImage_]:= Module[{},
   src=ColorConvert[rectImage,"Grayscale"];
   white=Closing[src,DiskMatrix[5]];
   srcAdjusted=Image[ImageData[src]/ImageData[white]];
   components=ComponentMeasurements[ColorNegate@Binarize[srcAdjusted],{"ConvexArea","Mask"}][[All,2]];
   largestComponent=Image[SortBy[components,First][[-1,2]]];
   ImageLines[EdgeDetect[largestComponent]]
]
(*
 
   components=ComponentMeasurements[ColorNegate@Binarize[srcAdjusted],{"ConvexArea","Mask"}][[All,2]];
   largestComponent=Image[SortBy[components,First][[-1,2]]];
   ImageLines[EdgeDetect[largestComponent]]*)



ff[x_]:= x+1;

