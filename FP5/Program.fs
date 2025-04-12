open System

let rec searchElement (currNode : List<obj>) (target: int) =
    if currNode.IsEmpty then raise(SystemException "This element not contained in tree")
    if target < (currNode[0] :?> int)
    then
        searchElement (currNode[1] :?> List<obj>) target
    elif target > (currNode[0] :?> int)
    then
        searchElement (currNode[2] :?> List<obj>) target  
    else
        currNode[0] :?> int

let tree : list<obj> = [box 5;  
                            box [box 3; 
                                    box [box 2; box []; box []]; 
                                    box [box 4; box []; box []]
                            ]; 
                            box [box 7;
                                    box [box 6; box []; box []];
                                    box [box 8; box []; box []]
                            ]
                        ]
try
    printfn "%A" (searchElement tree 2)
with
    | :? SystemException as ex -> printfn "%s" ex.Message
