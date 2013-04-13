module Main where

import Text.ParserCombinators.Parsec

--infixl 9 :&
--infixl 8 :|
--infixl 7 :>

preprocess :: String -> String
preprocess s =
    wrap $ noSpace s
        where
            noSpace (' ':xs) = noSpace xs
            noSpace (x:xs) = x:noSpace xs
            noSpace [] = []
            wrap f = if head f == '(' && last f == ')'
                then f
                else '(' : f ++ ")"

atom :: Parser LogicExpr
atom = do
    c <- oneOf ['A'..'Z']
    return $ Atom c

truth :: Parser LogicExpr
truth = do
    x <- oneOf "tf"
    case x of
        't' -> return Tau
        'f' -> return Con
        _   -> error "parse error"

negation :: Parser LogicExpr
negation = do
    _ <- char '~'
    x <- expr
    return $ Not x

conj :: Parser LogicExpr
conj = between (char '(') (char ')') $ do
    x <- expr
    _ <- char '&'
    y <- expr
    return (x :& y)

disj :: Parser LogicExpr
disj = between (char '(') (char ')') $ do
    x <- expr
    _ <- char '|'
    y <- expr
    return (x :| y)

imply :: Parser LogicExpr
imply = between (char '(') (char ')') $ do
    x <- expr
    _ <- string "=>"
    y <- expr
    return (x :> y)

expr :: Parser LogicExpr
expr = atom
    <|> truth
    <|> negation
    <|> try conj
    <|> try disj
    <|> try imply

readExpr :: String -> LogicExpr
readExpr s = case parse expr "logic" s of
    Left err -> error $ show err
    Right val -> val

main :: IO ()
main = do
    s <- getLine
    let input = preprocess s
    putStrLn input
    prf $ readExpr input

data LogicExpr = Atom Char              -- Atom
               | LogicExpr :& LogicExpr -- Conjunction
               | LogicExpr :| LogicExpr -- Disjunction
               | Not LogicExpr          -- Negation
               | LogicExpr :> LogicExpr -- Implication
               | Tau                    -- Tautology
               | Con                    -- Contradiction
               deriving(Read, Eq)

instance Show LogicExpr where
    show (Atom x) = [x]
    show (x :& y) = '(' : show x ++ " & " ++ show y ++ ")"
    show (x :| y) = '(' : show x ++ " V " ++ show y ++ ")"
    show (x :> y) = '(' : show x ++ " => " ++ show y ++ ")"
    show (Not x)  = '~' : show x
    show Tau      = "t"
    show Con      = "f"

rf :: LogicExpr -> String
rf = formula . simplify where
    f = eliminate . refactor
    simplify x = if f x == f (f x) then f x else simplify (f x)
    --simplify x = if length (formula $ f x) == length (formula $ f (f x)) then f x else simplify (f x)

--rf = formula . eliminate . refactor

--rrf :: LogicExpr -> String
--rrf = formula . eliminate . refactor . eliminate . refactor

prf :: LogicExpr -> IO ()
prf x = do
    putChar '\n'
    print x
    putStrLn $ rf x
    --putStrLn $ rrf x

-- format
formula :: LogicExpr -> String
formula (Atom x) = [x]
formula (x :& y) = '(' : formula x ++ " & " ++ formula y ++ ")"
formula (x :| y) = '(' : formula x ++ " V " ++ formula y ++ ")"
formula (x :> y) = '(' : formula x ++ " => " ++ formula y ++ ")"
formula (Not x)  = '~' : formula x
formula Tau      = show Tau
formula Con      = show Con

refactor :: LogicExpr -> LogicExpr
-- :&
refactor w@(Not (Atom x) :& Atom y) = if x == y then Con else w
refactor w@(Atom x :& Not (Atom y)) = if x == y then Con else w
    -- rules
--refactor (x :& (y :| z)) = refactor (x :& y) :| refactor (x :& z)
--refactor ((y :| z) :& x) = refactor (y :& x) :| refactor (z :& x)
refactor (x :& y)
    | x == Tau = refactor y
    | y == Tau = refactor x
    | x == Con || y == Con = Con
    | x == y = refactor x
    | refactor x == x && refactor y == y = refactor x :& refactor y
    | otherwise = refactor (refactor x :& refactor y)
-- :|
refactor w@(Not (Atom x) :| Atom y) = if x == y then Tau else w
refactor w@(Atom x :| Not (Atom y)) = if x == y then Tau else w
    -- rules
refactor (x :| (y :& z)) = refactor (x :| y) :& refactor (x :| z)
refactor ((y :& z) :| x) = refactor (y :| x) :& refactor (z :| x)
refactor (x :| y)
    | x == Con = refactor y
    | y == Con = refactor x
    | x == Tau || y == Tau = Tau
    | x == y = refactor x
    | refactor x == x && refactor y == y = refactor x :| refactor y
    | otherwise = refactor (refactor x :| refactor y)
-- :>
refactor (x :> y)
    | refactor x == Con = Tau
    | otherwise = refactor (Not x :| y)
-- Not
refactor (Not (Not x))  = refactor x
refactor (Not Tau)      = Con
refactor (Not Con)      = Tau
refactor (Not (x :| y)) = refactor (Not x) :& refactor (Not y)
refactor (Not (x :& y)) = refactor (Not x) :| refactor (Not y)
refactor (Not x)        = Not (refactor x)
-- Catch all
refactor x = x

eliminate :: LogicExpr -> LogicExpr
eliminate w@(_ :| _) = toOr $ fromOr w []
eliminate w@(_ :& _) = toAnd $ fromAnd w []
eliminate (Not x) = Not (eliminate x)
eliminate x = x

fromOr :: LogicExpr -> [LogicExpr] -> [LogicExpr]
fromOr (x@(Atom _) :| y@(_ :| _)) xs =
    fromOr y (addTerm x xs)
fromOr (x@(_ :| _) :| y@(Atom _)) xs =
    fromOr x (addTerm y xs)
fromOr (x@(Not (Atom _)) :| y@(_ :| _)) xs =
    fromOr y (addTerm x xs)
fromOr (x@(_ :| _) :| y@(Not (Atom _))) xs =
    fromOr x (addTerm y xs)
fromOr (x@(_ :| _) :| y@(_ :| _)) xs =
    let yy = fromOr y xs
    in [xx | xx <- fromOr x xs, xx `notElem` yy] ++ yy
fromOr (Not x@(_ :| _) :| Not y@(_ :| _)) xs =
    let yy = fromOr (Not y) xs
    in [xx | xx <- fromOr (Not x) xs, xx `notElem` yy] ++ yy
fromOr (x :| y) xs = endList x y xs
fromOr _ _ = []

fromAnd :: LogicExpr -> [LogicExpr] -> [LogicExpr]
fromAnd (x@(Atom _) :& y@(_ :& _)) xs =
    fromAnd y (addTerm x xs)
fromAnd (x@(_ :& _) :& y@(Atom _)) xs =
    fromAnd x (addTerm y xs)
fromAnd (x@(Not (Atom _)) :& y@(_ :& _)) xs =
    fromAnd y (addTerm x xs)
fromAnd (x@(_ :& _) :& y@(Not (Atom _))) xs =
    fromAnd x (addTerm y xs)
fromAnd (x@(_ :& _) :& y@(_ :& _)) xs =
    let xx = fromAnd x xs
    in xx ++ [yy | yy <- fromAnd y xs, yy `notElem` xx]
fromAnd (Not x@(_ :& _) :& Not y@(_ :& _)) xs =
    let xx = fromAnd (Not x) xs
    in xx ++ [yy | yy <- fromAnd (Not y) xs, yy `notElem` xx]
fromAnd (x :& y) xs = endList x y xs
fromAnd _ _ = []

addTerm :: LogicExpr -> [LogicExpr] -> [LogicExpr]
addTerm x xs
    | refactor (Not x) `elem` xs = Tau : xs
    | x `elem` xs = xs
    | otherwise = x : xs

endList :: LogicExpr -> LogicExpr -> [LogicExpr] -> [LogicExpr]
endList x y xs = zz
    where z = addTerm yy xs
          zz = addTerm xx z
          xx = eliminate x
          yy = eliminate y

toOr :: [LogicExpr] -> LogicExpr
toOr = foldl1 (:|)

toAnd :: [LogicExpr] -> LogicExpr
toAnd = foldl1 (:&)

testCases :: IO ()
testCases = do
    let expr1 = Tau :> ((Atom 'Q' :& Not (Atom 'Q') ) :| Tau)
        expr2 = (Atom 'P' :& (Atom 'P' :| Atom 'P')) :> Atom 'Q'
        expr3 = (Atom 'P' :| Atom 'Q') :| (Atom 'P' :| Atom 'R')
        expr4 = ((Atom 'P' :> Atom 'Q') :> Atom 'P') :> Atom 'P'
        exprs = [expr1, expr2, expr3, expr4]
    mapM_ prf exprs