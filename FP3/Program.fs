let areEquals (a: float) (b : float) =
    abs(a-b) < 1e-9

let getLengthSide (x1 : float, y1 : float) (x2 : float, y2 : float) = 
    sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2)

let isEquilateral (AB : float) (BC : float) (AC : float) =
    areEquals AB AC &&
    areEquals AC BC

let isIsosceles (AB : float) (BC : float) (AC : float) =
    areEquals AB AC  ||
    areEquals AB BC  ||
    areEquals AC BC

let isRightAngled (AB : float) (BC : float) (AC : float) =
    areEquals (AC ** 2 + BC ** 2) (AB ** 2) ||
    areEquals (AB ** 2 + BC ** 2) (AC ** 2) ||
    areEquals (AB ** 2 + AC ** 2) (BC ** 2)

let getTypeTriangle (lst: List<(float * float)>) =
    if isEquilateral (getLengthSide lst[0] lst[1]) (getLengthSide lst[1] lst[2]) (getLengthSide lst[0] lst[2])
    then "равносторонний"
    elif isIsosceles (getLengthSide lst[0] lst[1]) (getLengthSide lst[1] lst[2]) (getLengthSide lst[0] lst[2]) && 
        isRightAngled (getLengthSide lst[0] lst[1]) (getLengthSide lst[1] lst[2]) (getLengthSide lst[0] lst[2])
    then "прямоугольный и равнобедренный"
    elif isIsosceles (getLengthSide lst[0] lst[1]) (getLengthSide lst[1] lst[2]) (getLengthSide lst[0] lst[2])
    then "равнобедренный"
    elif isRightAngled (getLengthSide lst[0] lst[1]) (getLengthSide lst[1] lst[2]) (getLengthSide lst[0] lst[2])
    then "прямоугольный"
    else "разносторонний"


printfn $"Этот треугольник - {getTypeTriangle [ 1,1.5; 2,0; 0,2 ]}"
