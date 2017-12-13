module Enumeration where

import Data.List
import Types
import Parser
import Helper

-- |The 'Expr' type is for expression expansion in enumeration
data Expr = EDone
          | ESkip Expr
          | EYield Expr Expr
          | Var String
          | App Expr Expr
          | Tuple Expr Expr
          | Fun String [Expr] Expr
          | Stream String String
          | Where Expr [Expr]
          | Switch Expr [(Expr,Expr)]
          | Bin Binop Expr Expr
          | Const Int

data Binop = Plus | Minus | Mul

-- |The 'listToStr' converts a state list into a string 
-- in the form of tuple in the program
listToTuple :: [String] -> String
listToTuple l = case l of
                []        -> ""
                [x]       -> x
                x:xs      -> x ++ "," ++ (listToTuple xs)

-- |The 'exprToStr' converts an expression into a string 
-- to be replaced in program
exprToStr :: Expr -> String
exprToStr e = case e of
    EDone       -> "Done"
    ESkip t     -> "Skip (" ++ (exprToStr t) ++ ")"
    EYield e0 t -> "Yield " ++ (exprToStr e0) ++ " (" ++ (exprToStr t) ++ ")"
    Var v       -> v
    App e1 e2   -> "(" ++ (exprToStr e1) ++ " " ++ (exprToStr e2) ++ ")"
    Fun name e1 e2 -> name ++ " " ++ (paramsToStr e1) ++ "=" ++ (exprToStr e2)
    Stream e1 e2 -> "(Stream " ++ e1 ++ " " ++ e2 ++ ")"
    Tuple e1 e2 -> "(" ++ (exprToStr e1) ++ "," ++ (exprToStr e2) ++ ")"
    Where e1 e2 -> (exprToStr e1) ++ "\n  where\n" ++ (paramsToStr e2)
    Switch e1 e2 -> "case " ++ (exprToStr e1) ++ " of\n" ++ (casesToStr e2)

paramsToStr :: [Expr] -> String
paramsToStr el = unwords (map ( (++) "\t") (map exprToStr el))

casesToStr :: [(Expr,Expr)] -> String
casesToStr [] = ""
casesToStr ((c,r):xs) = "\t\t"++(exprToStr c) ++ " -> " ++ (exprToStr r) ++ "\n" ++ (casesToStr xs)

-- |The 'expand' function enumerate the expressions top down 
-- It takes two arguments of type 'Int' and '[String]' 
-- The first argument stands for the parametric expression index
expand :: Int -> [(Expr,TypeExpr)] -> Expr
expand idx comps = fst $ nth idx comps

makeTuple :: Expr -> [Expr] -> [(Expr,TypeExpr)]
makeTuple _ [] = []
makeTuple e1 (e2:es) = (Tuple e1 e2, Term (TVar "state")):(makeTuple e1 es)

stateExpand :: [(Expr,TypeExpr)] -> [(Expr,TypeExpr)]
stateExpand comps = case scomps of 
    [] -> []
    ((e1,t1):xs) -> (makeTuple e1 suffix)++(stateExpand xs)
  where
    scomps = stateExpandHelper comps
    suffix = [Var "True",Var "False", Var "Just x",Var "Just y", Var "Nothing"]

stateExpandHelper :: [(Expr,TypeExpr)] -> [(Expr,TypeExpr)]
stateExpandHelper [] = []
stateExpandHelper ((e,t):xs) = case t of
    Term (TVar "state") -> (e,t):(stateExpandHelper xs)
    _ -> stateExpandHelper xs

getParamTerm :: Int -> TExp -> [(Expr,TypeExpr)]
getParamTerm idx texp = case texp of
    TVar v -> [(Var v, Term texp)]
    TParen e -> (case e of
        TFun e1 e2 -> [(Var ("f_"++(show idx)), e)]
        )
    TList t -> [(Stream ("next_"++(show idx)) ("s_"++(show idx)),Term (TStream t))]
    TTuple t1 t2 -> [(Stream ("next_"++(show idx)) ("s_"++(show idx)),Term (TStream t1))]

getParams :: Int -> TypeExpr -> [(Expr,TypeExpr)]
getParams idx texp = case texp of
    Term t -> getParamTerm (idx+1) t
    TFun e1 e2 -> (getParamTerm (idx+1) e1) ++ (getParams (idx+2) e2)

generateFun :: String -> Expr
generateFun str = 
    Fun name params (generateFunBody params)
  where
    name = getFunName str
    params = fst $ unzip (popBack (getParams 0 (getFunType str)))

typeTransform :: TypeExpr -> TypeExpr
typeTransform texp = case texp of
    Term t -> Term $ typeExp1Transform t
    TFun t1 t2 -> TFun (typeExp1Transform t1) (typeTransform t2)

typeExp1Transform :: TExp -> TExp
typeExp1Transform texp = case texp of
    TList t -> TStream t
    _ -> texp

getIdxFromParams :: [Expr] -> [(String,String)]
getIdxFromParams [] = []
getIdxFromParams (x:xs) = case x of
    Stream f st -> (f,st):(getIdxFromParams xs)
    _ -> getIdxFromParams xs

getStreamNum :: [Expr] -> Int
getStreamNum [] = 0
getStreamNum (x:xs) = case x of
    Stream f st -> 1 + (getStreamNum xs)
    _ -> getStreamNum xs

generateWhereBody :: Int -> [Expr] -> [Expr]
generateWhereBody n params
    | n == 0 || n == 1  = [(Fun "next" [Var "?"] cases)]
    | otherwise = (Fun "next" [Var "?"] cases):(generateWhereBody (n-1) params)
  where
    states = getIdxFromParams params
    slen  = length states
    currSt = nth (slen-n) states
    cases = Switch (App (Var (fst currSt)) (Var "?")) (dcase:scase:ycase:[])
    dcase = (EDone,Var "?")
    scase = (ESkip (Var "?"),Var "?")
    ycase = (EYield (Var "?") (Var "?"),Var "?")

generateFunBody :: [Expr] -> Expr
generateFunBody params = 
    Where (Stream "next" "?") whereBody
  where
    states = getIdxFromParams params
    whereBody = generateWhereBody (getStreamNum params) params

getNestParams :: [(Expr,TypeExpr)] -> [(Expr,TypeExpr)]
getNestParams [] = []
getNestParams (x:xs) = case x of
    (Stream f st,_) -> (Var f,TFun (TVar "next") (Term (TVar "Step"))):(Var st, Term (TVar "state")):remains
    _ -> remains 
  where
    remains = getNestParams xs

getStateParams :: [(Expr,TypeExpr)] -> [(Expr,TypeExpr)]
getStateParams [] = []
getStateParams (x:xs) = case x of
    (_,Term (TVar "state")) -> x:(getStateParams xs)
    _ -> getStateParams xs

isDup :: Expr -> Expr -> Bool
isDup e1 e2 = case (e1,e2) of
    (Var n1, Var n2) -> n1==n2
    _ -> False

basicComponents :: String -> [(Expr,TypeExpr)]
basicComponents str = 
    coupleState ++ params ++ nestParams ++ (EDone, Term (TVar "Step")):((Var "x",Term (TVar "T")):(Var "y",Term (TVar "T")):[])
  where
    params = popBack (getParams 0 (getFunType str))
    nestParams = getNestParams params
    stateParams = fst $ unzip (getStateParams nestParams)
    num = getStreamNum (fst $ unzip params)
    coupleState = if num > 1 then [(Tuple e1 e2,Term (TVar "state")) | e1<- [Var "x"]++stateParams++[Var "y"], e2 <- [Var "x"]++stateParams++[Var "y"], not (isDup e1 e2)] else []

makeYield :: (Expr,String) -> Expr
makeYield (e1,st) = EYield (e1) (Var st)

buildYieldComp :: String -> Expr -> [(Expr,TypeExpr)]
buildYieldComp str e= 
    (map (\t -> (makeYield t,Term (TVar "Step"))) [(e,y) | y<-states])
  where
    params = fst $ unzip (popBack (getParams 0 (getFunType str)))
    states = snd $ unzip (getIdxFromParams params)

buildStepComp :: String -> [(Expr,TypeExpr)] -> [(Expr,TypeExpr)]
buildStepComp str [] = []
buildStepComp str (x:xs) = case x of
    (e,Term (TVar "state")) -> (ESkip e,Term (TVar "Step")):(EYield (Var "x") e,Term (TVar "Step")):(buildStepComp str xs)
    (e,TFun (TVar "next") _) ->  (buildStepComp str xs)
    (e,TFun _ _) -> (buildStepComp str xs)++(buildYieldComp str (App e (Var "x")))
    (e,Term (TVar "FV")) -> (buildStepComp str xs)++(buildYieldComp str e)
    _ -> buildStepComp str xs

getComponents :: String -> [(Expr,TypeExpr)] -> [(Expr,TypeExpr)]
getComponents [] _ = []
getComponents str es =
    states ++ es ++ bc ++ (buildStepComp str (states ++ bc ++ es))
  where
    bc = basicComponents str
    params = fst $ unzip (popBack (getParams 0 (getFunType str)))
    num = getStreamNum params
    states = if num > 1 then (stateExpand bc) else []

printComponents :: [(Expr,TypeExpr)] -> [String]
printComponents [] = [""]
printComponents ((e,t):xs) = (exprToStr e):(printComponents xs)

selectStateComp :: [(Expr,TypeExpr)] -> Int -> [Int]
selectStateComp [] _ = []
selectStateComp ((e,t):xs) idx = case (e,t) of
    (_,Term (TVar "state")) -> idx:(selectStateComp xs (idx+1))
    _ -> selectStateComp xs (idx+1)

selectParamState :: [(Expr,TypeExpr)] -> Int -> [Int]
selectParamState [] _ = []
selectParamState ((e,t):xs) idx = case (e,t) of
    (Tuple (Tuple (Var n1) (Var n2)) _ , Term (TVar "state")) | n1=="x"||n1=="x"||n2=="y"||n2=="y"->idx:(selectParamState xs (idx+1))
        | otherwise -> (selectParamState xs (idx+1))
    _ -> selectParamState xs (idx+1)

selectState :: [(Expr,TypeExpr)] -> Int -> [Int]
selectState [] _ = []
selectState ((e,t):xs) idx = case (e,t) of
    (Var n, Term (TVar "state")) -> idx:(selectState xs (idx+1))
    _ -> selectState xs (idx+1)

selectSpecialState :: [(Expr,TypeExpr)] -> Int -> [Int]
selectSpecialState [] _= []
selectSpecialState ((e,t):xs) idx = case (e,t) of
    ((Tuple (Tuple (Var n1) (Var n2)) _),Term (TVar "state")) -> if n1!!0=='s'&&n2!!0=='s' then idx:(selectSpecialState xs (idx+1)) else selectSpecialState xs (idx+1)
    _ -> selectSpecialState xs (idx+1)

selectScalarComp :: [(Expr,TypeExpr)] -> Int -> [Int]
selectScalarComp [] _ = []
selectScalarComp ((e,t):xs) idx = case (e,t) of
    (_,Term (TVar "T")) -> idx:(selectScalarComp xs (idx+1))
    (_,Term (TVar "state")) -> idx:(selectScalarComp xs (idx+1))
    --(_,Term (TStream _)) -> idx:(selectScalarComp xs (idx+1))
    _ -> selectScalarComp xs (idx+1)

selectStepComp :: [(Expr,TypeExpr)] -> Int -> [Int]
selectStepComp [] _ = []
selectStepComp ((e,t):xs) idx = case (e,t) of
    (_,Term (TVar "Step")) -> idx:(selectStepComp xs (idx+1))
    _ -> selectStepComp xs (idx+1)

strHasHole :: String -> Bool
strHasHole str = case elemIndex '?' str of
    Just i -> True
    Nothing -> False

containsHole :: Expr -> Bool
containsHole expr = case expr of
    EDone -> False
    ESkip e -> containsHole e
    EYield e1 e2 -> (containsHole e1) || (containsHole e2)
    Var v -> strHasHole v
    App e1 e2 -> (containsHole e1) || (containsHole e2)
    Tuple e1 e2 -> (containsHole e1) || (containsHole e2)
    Fun name params e -> (foldl (||) False (map containsHole params)) || (containsHole e)
    Stream s1 s2 -> (strHasHole s1) || (strHasHole s2)
    Where e1 es -> (foldl (||) False (map containsHole es)) || (containsHole e1)
    Switch e es -> (containsHole e) || (foldl (||) False (map containsHole (fst (unzip es)))) || (foldl (||) False (map containsHole (snd (unzip es))))

assignSwitchHole :: [(Expr,TypeExpr)] -> [(Expr,Expr)] -> [[Int]]
assignSwitchHole _ [] = []
assignSwitchHole comps ((c1,c2):cs) = case c1 of
    EDone -> otherHoles
    ESkip e -> [singleState] ++ otherHoles
    EYield e1 e2 | containsHole e1 -> (assignHoles comps e1) ++ [singleState] ++ otherHoles
                 | otherwise -> [singleState] ++ otherHoles
  where
    stepComps = selectStepComp comps 0
    singleState = selectState comps 0
    otherHoles = [stepComps] ++ (assignSwitchHole comps cs)

assignHoles :: [(Expr,TypeExpr)] -> Expr -> [[Int]]
assignHoles comps expr = case expr of
    EDone -> []
    ESkip e -> [stateComps]
    EYield e1 e2 -> [scalarComps]++[stateComps]
    Var v -> if (strHasHole v) then [scalarComps] else []
    App e1 e2 -> (assignHoles comps e1) ++ (assignHoles comps e2)
    Tuple e1 e2 -> (assignHoles comps e1) ++ (assignHoles comps e2)
    Fun name params e -> (if name=="next" then [if ((getStreamNum params)>1) then paramStates else stateComps] else (foldl (++) [] (map (assignHoles comps) params))) ++ (assignHoles comps e)
    Stream s1 s2 -> (if (strHasHole s1) then [scalarComps] else []) ++ (if (strHasHole s2) then [if ((length specialState)>0) then specialState else stateComps] else [])
    Where e es -> (assignHoles comps e) ++ (foldl (++) [] (map (assignHoles comps) es))
    Switch e es -> (assignHoles comps e) ++ (assignSwitchHole comps es)
  where
    stateComps = selectStateComp comps 0
    singleState = selectState comps 0
    specialState = selectSpecialState comps 0
    scalarComps = selectScalarComp comps 0
    paramStates = selectParamState comps 0

validSwitchEnv :: [String] -> [(Expr,Expr)] -> Bool
validSwitchEnv env [] = True
validSwitchEnv env ((e1,e2):xs) = 
    (validInEnv (addParamToEnv [e1] env) e2) &&
    (validSwitchEnv env xs)

validInEnv :: [String] -> Expr -> Bool
validInEnv env e = case e of
    EDone -> True
    ESkip e -> validInEnv env e
    EYield e1 e2 -> (validInEnv env e1) && (validInEnv env e2)
    Var v -> (case env of
        [] -> False
        (x:xs)-> ((v==x) || (validInEnv xs e)))
    App e1 e2 -> (validInEnv env e1) && (validInEnv env e2)
    Tuple e1 e2 -> (validInEnv env e1) && (validInEnv env e2)
    Fun name params e -> validInEnv (name:(addParamToEnv params env)) e
    Stream s1 s2 -> True
    Where e es -> (foldl (&&) True (map (validInEnv env) es)) && (validInEnv (addParamToEnv es env) e)
    Switch e es -> (validInEnv env e) && (validSwitchEnv (addParamToEnv [e] env) es)

addParamToEnv :: [Expr] -> [String] -> [String]
addParamToEnv [] env = env
addParamToEnv (x:xs) env = case x of
    Var v -> addParamToEnv xs (v:env)
    EYield e1 e2 -> addParamToEnv (e1:e2:xs) env
    ESkip e1 -> addParamToEnv (e1:xs) env
    Stream next st -> addParamToEnv xs (next:st:env)
    Tuple e1 e2 -> addParamToEnv (e1:e2:xs) env
    Fun name params e -> addParamToEnv xs (name:(addParamToEnv params env))
    _ -> env

generateAssignments :: [[Int]] -> [[Int]]
generateAssignments [x] = splitList x
generateAssignments (x:xs) =
    [y:ys | y<-x, ys <- (generateAssignments xs)]

getFreeNames :: [(Expr,TypeExpr)] -> [String]
getFreeNames [] = []
getFreeNames (x:xs) = case x of
    (Var name,_) -> name:(getFreeNames xs)
    _ -> getFreeNames xs

validateExpr :: Expr -> [(Expr,TypeExpr)] -> Bool
validateExpr expr fv = validInEnv (freeNames) expr
  where
    freeNames = getFreeNames fv