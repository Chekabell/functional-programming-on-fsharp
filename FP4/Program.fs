open System

let rnd = Random()

let inCircle (radius: float) (x : float) (y: float) =
    x**2 + y**2 <= radius**2

let getRandomNumber (radius: float) =
    radius * (2.0 * rnd.NextDouble() - 1.0)

let areaAssessment (radius: float) (targetQuanityPoint: int) =
    let rec _areaAssessment (quantityPointIn: int) (quantityPoint: int) =
        if quantityPoint < targetQuanityPoint
        then
            if inCircle radius (getRandomNumber radius) (getRandomNumber radius) then 
                _areaAssessment (quantityPointIn+1) (quantityPoint+1) 
            else 
                _areaAssessment quantityPointIn (quantityPoint+1) 
        else 
            float quantityPointIn/ float quantityPoint * Math.PI * radius**2.0

    _areaAssessment 0 0

let calculateArea (arg: List<float>) =
    //arg[0] - radius, arg[1] - accuracy
    let rec _calculateArea (prevAssessment: float) (currAssessment: float) (quantityPoint: int) =
        if abs(currAssessment - prevAssessment) > arg[1] then 
            _calculateArea currAssessment (areaAssessment arg[0] (quantityPoint + quantityPoint)) (quantityPoint + quantityPoint) 
        else 
            currAssessment

    _calculateArea 0.0 (areaAssessment arg[0] 100) 100

let largerElementOfMatrix (matrixA : int[,]) (matrixB : int[,]) (x: int) (y : int) =
    if matrixA[x,y] > matrixB[x,y] then matrixA[x,y]
    else matrixB[x,y]

let smallestElementOfMatrix (matrixA : int[,]) (matrixB : int[,]) (x: int) (y : int) =
    if matrixA[x,y] < matrixB[x,y] then matrixA[x,y]
    else matrixB[x,y]

let rec formMoreMatrix (matrixX : int[,]) (matrixA : int[,]) (matrixB : int[,]) =
    let rec _formMoreMatrix x y =
        matrixX.[x, y] <- largerElementOfMatrix matrixA matrixB x y

        if x < Array2D.length1 matrixX - 1 then
            _formMoreMatrix (x + 1) y
        elif y < Array2D.length2 matrixX - 1 then
            _formMoreMatrix 0 (y + 1)
        else
            ()

    _formMoreMatrix 0 0
    matrixX

let formLessMatrix (matrixX : int[,]) (matrixA : int[,]) (matrixB : int[,]) =
    let rec _formLessMatrix x y =
        matrixX.[x, y] <- smallestElementOfMatrix matrixA matrixB x y

        if x < Array2D.length1 matrixX - 1 then
            _formLessMatrix (x + 1) y
        elif y < Array2D.length2 matrixX - 1 then
            _formLessMatrix 0 (y + 1)
        else
            ()

    _formLessMatrix 0 0
    matrixX
 
let formMatrix (arg: List<int[,]>) =
    if Array2D.length1 arg[0] <= Array2D.length1 arg[1] then
        if Array2D.length2 arg[0] <= Array2D.length2 arg[1] then
            [formLessMatrix (Array2D.zeroCreate<int> (Array2D.length1 arg[0]) (Array2D.length2 arg[0])) arg[0] arg[1];
            formMoreMatrix (Array2D.zeroCreate<int> (Array2D.length1 arg[0]) (Array2D.length2 arg[0])) arg[0] arg[1]]
        else
            [formLessMatrix (Array2D.zeroCreate<int> (Array2D.length1 arg[0]) (Array2D.length2 arg[1])) arg[0] arg[1];
            formMoreMatrix (Array2D.zeroCreate<int> (Array2D.length1 arg[0]) (Array2D.length2 arg[1])) arg[0] arg[1]]
    else
        if Array2D.length2 arg[0] <= Array2D.length2 arg[1] then
            [formLessMatrix (Array2D.zeroCreate<int> (Array2D.length1 arg[1]) (Array2D.length2 arg[0])) arg[0] arg[1];
            formMoreMatrix (Array2D.zeroCreate<int> (Array2D.length1 arg[1]) (Array2D.length2 arg[0])) arg[0] arg[1]]
        else
            [formLessMatrix (Array2D.zeroCreate<int> (Array2D.length1 arg[1]) (Array2D.length2 arg[1])) arg[0] arg[1];
            formMoreMatrix (Array2D.zeroCreate<int> (Array2D.length1 arg[1]) (Array2D.length2 arg[1])) arg[0] arg[1]]

    
    


printfn $"Площадь круга - {calculateArea [4.0; 0.1]}"

let listMatrix = formMatrix [array2D [[1; 2; 4]; 
                                    [3; 4; 6];
                                    [2; 3; 10]];
                            array2D [[4; 3; 4]; 
                                    [2; 1; 5]]]

printfn "%A" listMatrix[0]
printfn "%A" listMatrix[1]

// printf "%A" (formMoreMatrix (Array2D.zeroCreate<int> 3 2) (array2D [[1; 2]; [3; 4]; [3; 4]]) (array2D [[4; 3]; [2; 1]; [3; 4]]))



