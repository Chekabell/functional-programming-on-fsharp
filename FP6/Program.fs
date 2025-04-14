
let rec traverseTree (currNode : obj[]) (func : int -> int) =
    if currNode.Length = 0 then 
        ()
    else 
        traverseTree (currNode[1] :?> obj[]) func
        currNode.[0] <- box (func (unbox currNode[0]))
        traverseTree (currNode[2] :?> obj[]) func


let tree = [|box 5;  
                box [|box 3; 
                        box [|box 2; box [||]; box [||]|]; 
                        box [|box 4; box [||]; box [||]|]
                |]; 
                box [|box 7;
                        box [|box 6; box [||]; box [||]|];
                        box [|box 8; box [||]; box [||]|]
                |]
            |]

printfn "%A" tree

traverseTree tree (fun x -> x*2)

printfn "%A" tree