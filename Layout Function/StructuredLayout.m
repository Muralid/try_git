(* ::Package:: *)

BeginPackage["StructuredLayout`"]
createGridList[rows_,columns_]:=ConstantArray["",{rows,columns}]

layoutGrid[list_,meshFactor_]:=(noOfRows=Length[list];noOfColumns=Length[list[[1]]];Panel[GraphicsGrid[list,ImageSize-> {((noOfColumns*meshFactor)),((noOfRows*meshFactor))},ItemAspectRatio-> 1,ContentSelectable-> True,ImagePadding-> None,Spacings-> {0,0},Alignment-> {Center,Center},Background-> RGBColor[.745,.886,1]],ImageSize-> {((noOfColumns*meshFactor)+20),((noOfRows*meshFactor)+20)},Alignment-> {Center,Center},Background-> RGBColor[.745,.886,1]])

placeControl[yCoOrdinate_,xCoOrdinate_,controlID_,controlWidth_,controlHeight_,list_,meshFactor_]:=
(rowNumber=yCoOrdinate/meshFactor;
columnNumber=xCoOrdinate/meshFactor;
rowSpan=controlWidth/meshFactor;
columnSpan=controlHeight/meshFactor;
updatedList={};For[i=1,i<= Length[list],i++,
          (individualRow={};
           For[j=1,j<= Length[list[[i]]],j++,
              (If[i==rowNumber,
                 (If[j==columnNumber,
                     (control=controlID;individualRow=Append[individualRow,(list[[i,j]]=control)];For[k=(j+1),k<=((j+rowSpan)-1),k++,(individualRow=Append[individualRow,(list[[i,k]]=SpanFromLeft)];)];j=(k-2);),
                     (individualRow=Append[individualRow,list[[i,j]]])
                    ]
                  ),
                 (If[((rowNumber<i)&&(i<=((rowNumber+columnSpan)-1))),(If[j==columnNumber,(individualRow=Append[individualRow,(list[[i,j]]=SpanFromAbove)];For[k=(j+1),k<=((j+rowSpan)-1),k++,(individualRow=Append[individualRow,(list[[i,k]]=SpanFromBoth)];)];j=(k-1);),(individualRow=Append[individualRow,list[[i,j]]])]),(individualRow=Append[individualRow,list[[i,j]]];)])
                 ]
               )
              ];
           updatedList=Append[updatedList,individualRow]
           )
         ];
updatedList
)
EndPackage[]

