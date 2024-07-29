module PE3 where

data Expression = Variable String 
    | Constant Float
    | Sine Expression
    | Cosine Expression
    | Negation Expression
    | Addition Expression Expression
    | Multiplication Expression Expression

class Differential a where
    diff :: a -> a -> a

instance Show Expression where
    
    show (Variable x) = "Variable '" ++ x ++ "'"
    show (Constant c) = "Constant " ++ show c
    show (Sine e) = "sin " ++ show e
    show (Cosine e) = "cos " ++ show e
    show (Negation e) = "-" ++ show e
    show (Addition e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Multiplication e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"

instance Eq Expression where
    (==) (Variable x) (Variable y) = x == y
    (==) (Constant a) (Constant b) = a == b
    (==) (Sine e1) (Sine e2) = e1 == e2
    (==) (Cosine e1) (Cosine e2) = e1 == e2
    (==) (Negation e1) (Negation e2) = e1 == e2
    (==) (Addition e11 e12) (Addition e21 e22) = e11 == e21 && e12 == e22
    (==) (Multiplication e11 e12) (Multiplication e21 e22) = e11 == e21 && e12 == e22
    (==) _ _ = False
    
instance Num Expression where
    abs x = x
    signum x = x
    fromInteger x = Constant (fromInteger x)
    negate (Constant c) = Constant (-c)
    negate e = Negation e
    (+) e1 e2 = Addition e1 e2
    (*) e1 e2 = Multiplication e1 e2



instance Differential Expression where
    diff (Constant _) _ = Constant 0
    diff (Variable v) (Variable dv)
        | v == dv = Constant 1
        | otherwise = Constant 0
    diff (Sine expr) (Variable dv) = Multiplication (Cosine expr) (diff expr (Variable dv))
    diff (Cosine expr) (Variable dv) = (Multiplication (Negation (Sine expr)) (diff expr (Variable dv)))
    diff (Negation expr) (Variable dv) = Negation (diff expr (Variable dv))
    diff (Addition lhs rhs) (Variable dv) = Addition (diff lhs (Variable dv)) (diff rhs (Variable dv))
    diff (Multiplication lhs rhs) (Variable dv) =
        Addition (Multiplication (diff lhs (Variable dv)) rhs) (Multiplication lhs (diff rhs (Variable dv)))


isOperator :: String -> Bool
isOperator x = x `elem` ["+", "-", "*", "/", "sin", "cos"]

precedence :: String -> Int
precedence "-" = 4  -- Unary minus
precedence "sin" = 3
precedence "cos" = 3
precedence "*" = 2
precedence "/" = 2
precedence "+" = 1
precedence _   = 0  -- Default precedence for other tokens


shuntingyard :: [String] -> [String] -> [String] -> [String]
shuntingyard [] [] outputQueue = outputQueue
shuntingyard [] (op:ops) outputQueue = shuntingyard [] ops (outputQueue ++ [op])
shuntingyard (token:tokens) operatorStack outputQueue
    | not (isOperator token) && token /= "(" && token /= ")" = shuntingyard tokens operatorStack (outputQueue ++ [token])
    | token == "(" = shuntingyard tokens (token : operatorStack) outputQueue
    | token == ")" = case break (== "(") operatorStack of
                        (_, []) -> error "Mismatched parentheses"
                        (ops, _:rest) -> shuntingyard tokens rest (outputQueue ++ ops)
    | isOperator token =
        let (operatorsToPop, restOperators) = span (\op -> precedence op >= precedence token) operatorStack
        in shuntingyard tokens (token : restOperators) (outputQueue ++ operatorsToPop)
    | otherwise = error "Invalid token"





isVariable :: String -> Bool
isVariable s = all (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) s

isNumber :: String -> Bool
isNumber s = all (\c -> c >= '0' && c <= '9') s

