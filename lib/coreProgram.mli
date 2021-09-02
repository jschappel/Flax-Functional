type expression =
    AddExpr of expression * expression
    | SubExpr of expression * expression
    | MultExpr of expression * expression
    | DivExpr of expression * expression
    | NumExpr of float