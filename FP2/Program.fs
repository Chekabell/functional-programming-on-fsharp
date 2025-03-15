
printfn "%f second" ((fun (lst: List<double>) ->
    [lst[0]/(lst[1]*lst[2])]
 ) [5;1;2]).Head
