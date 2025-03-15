let areEquals (a: float) (b : float) =
    abs(a-b) < 1e-9

let getLengthSide (firstCoord : List<float>) (secondCoord : List<float>) = 
    sqrt((secondCoord[0] - firstCoord[0]) ** 2 + (secondCoord[1] - firstCoord[1]) ** 2)

let isEquilateral (coord:List<List<float>>) =
    areEquals (getLengthSide coord.[0] coord[1]) (getLengthSide coord.[0] coord[2]) &&
    areEquals (getLengthSide coord.[0] coord[2]) (getLengthSide coord.[1] coord[2])

let isIsosceles (coord: List<List<float>>) =
    areEquals (getLengthSide coord.[0] coord[1]) (getLengthSide coord.[0] coord[2])  ||
    areEquals (getLengthSide coord.[0] coord[1]) (getLengthSide coord.[1] coord[2])  ||
    areEquals (getLengthSide coord.[0] coord[2]) (getLengthSide coord.[1] coord[2])

let isRightAngled (coord: List<List<float>>) =
    areEquals (getLengthSide coord[0] coord[2] ** 2 + getLengthSide coord[1] coord[2] ** 2) (getLengthSide coord[0] coord[1] ** 2) ||
    areEquals (getLengthSide coord[0] coord[1] ** 2 + getLengthSide coord[1] coord[2] ** 2) (getLengthSide coord[0] coord[2] ** 2) ||
    areEquals (getLengthSide coord[0] coord[1] ** 2 + getLengthSide coord[0] coord[2] ** 2) (getLengthSide coord[1] coord[2] ** 2)

let getTypeTriangle (lst: List<List<float>>) =
    if isEquilateral lst
    then "равносторонний"
    elif isIsosceles lst && isRightAngled lst
    then "прямоугольный и равнобедренный"
    elif isIsosceles lst
    then "равнобедренный"
    elif isRightAngled lst
    then "прямоугольный"
    else "разносторонний"


printfn $"Этот треугольник - {getTypeTriangle [[0;0];[3;0];[1;2]]}"
