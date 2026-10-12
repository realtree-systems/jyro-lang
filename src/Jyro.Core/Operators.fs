namespace RealTree.Jyro

// Binary operators supported by Jyro
type internal BinaryOp =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulo
    | Concat
    | Equal
    | NotEqual
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | And
    | Or
    | Coalesce

// Unary operators supported by Jyro
type internal UnaryOp =
    | Negate
    | Not

// Assignment operators supported by Jyro
type internal AssignOp =
    | Assign
    | AddAssign
    | SubtractAssign
    | MultiplyAssign
    | DivideAssign
    | ModuloAssign
    | ConcatAssign

module internal Operators =
    let binaryOpToString = function
        | Add -> "+"
        | Subtract -> "-"
        | Multiply -> "*"
        | Divide -> "/"
        | Modulo -> "%"
        | Concat -> "&"
        | Equal -> "=="
        | NotEqual -> "!="
        | LessThan -> "<"
        | LessThanOrEqual -> "<="
        | GreaterThan -> ">"
        | GreaterThanOrEqual -> ">="
        | And -> "and"
        | Or -> "or"
        | Coalesce -> "??"

    let unaryOpToString = function
        | Negate -> "-"
        | Not -> "not"

    let assignOpToString = function
        | Assign -> "="
        | AddAssign -> "+="
        | SubtractAssign -> "-="
        | MultiplyAssign -> "*="
        | DivideAssign -> "/="
        | ModuloAssign -> "%="
        | ConcatAssign -> "&="

    let binaryOpForAssign = function
        | Assign -> None
        | AddAssign -> Some Add
        | SubtractAssign -> Some Subtract
        | MultiplyAssign -> Some Multiply
        | DivideAssign -> Some Divide
        | ModuloAssign -> Some Modulo
        | ConcatAssign -> Some Concat
