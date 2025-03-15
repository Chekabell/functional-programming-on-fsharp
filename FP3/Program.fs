let areEquals (a: float) (b : float) =
    abs(a-b) < 1e-9

let getLengthSide (x1 : float, y1 : float) (x2 : float, y2 : float) = 
    sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2)

let isEquilateral (coord : List<(float * float)>) =
    areEquals (getLengthSide coord.[0] coord[1]) (getLengthSide coord.[0] coord[2]) &&
    areEquals (getLengthSide coord.[0] coord[2]) (getLengthSide coord.[1] coord[2])

let isIsosceles (coord : List<(float * float)>) =
    areEquals (getLengthSide coord.[0] coord[1]) (getLengthSide coord.[0] coord[2])  ||
    areEquals (getLengthSide coord.[0] coord[1]) (getLengthSide coord.[1] coord[2])  ||
    areEquals (getLengthSide coord.[0] coord[2]) (getLengthSide coord.[1] coord[2])

let isRightAngled (coord : List<(float * float)>) =
    areEquals (getLengthSide coord[0] coord[2] ** 2 + getLengthSide coord[1] coord[2] ** 2) (getLengthSide coord[0] coord[1] ** 2) ||
    areEquals (getLengthSide coord[0] coord[1] ** 2 + getLengthSide coord[1] coord[2] ** 2) (getLengthSide coord[0] coord[2] ** 2) ||
    areEquals (getLengthSide coord[0] coord[1] ** 2 + getLengthSide coord[0] coord[2] ** 2) (getLengthSide coord[1] coord[2] ** 2)

let getTypeTriangle (lst: List<(float * float)>) =
    if isEquilateral lst
    then "равносторонний"
    elif isIsosceles lst && isRightAngled lst
    then "прямоугольный и равнобедренный"
    elif isIsosceles lst
    then "равнобедренный"
    elif isRightAngled lst
    then "прямоугольный"
    else "разносторонний"


printfn $"Этот треугольник - {getTypeTriangle [ 0,0; 2,0; 0,2 ]}"
