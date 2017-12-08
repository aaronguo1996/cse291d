data Typ = TypInt 
    | TypBool 
    | TypLam Typ Typ 
    | TypList Typ 
    | TypStream Typ 
    | TypStep String String
    | TypState String
    | TypUNK
    deriving (Show, Eq)

data Exp = ExpLam Var Typ Exp Typ
    -- production rules for user input
    | ExpApp Exp Exp Typ
    | ExpLet Var Typ Exp Exp Typ
    | ExpCase Var Exp Var Var Exp Typ
    | ExpIf Exp Exp Exp Typ
    | ExpInt Int
    | ExpBool Bool
    | ExpNil Typ
    | ExpAdd Exp Exp Typ
    | ExpLT Exp Exp Typ
    | ExpAnd Exp Exp Typ
    | ExpCons Exp Exp Typ
    | ExpVar Var Typ 
    -- production rules for output
    | ExpDone
    | ExpSkip [Exp]
    | ExpYield Exp [Exp] [Exp]
    | ExpStepCase Var Exp Var Var Exp Typ
    | ExpStream Var [Var] [Var] Int Exp
    | ExpUNK
    deriving (Eq)

data Var = Var String | StreamVar String | StepVar String String
    deriving (Eq)

blnks :: Int -> String
blnks n = concat (replicate n "  ")

instance Show Exp where 
    show exp0 = go exp0 0
        where 
            go exp d = if unaryexp then estr else (concat ["(",estr,")"])
                where
                    unaryexp = case exp of 
                        ExpVar _ _ -> True 
                        ExpInt _ -> True 
                        ExpBool _ -> True 
                        ExpNil _ -> True 
                        ExpDone -> True 
                        otherwise -> False
                    estr = case exp of 
                        ExpLam v1 _ e1 _ -> concat ["\\",show v1," -> ",show e1,""]
                        ExpApp e1 e2 _ -> concat [go e1 d," ",go e2 d]
                        ExpCase v1 e1 v2 v3 e2 _ -> concat [s1,s2,s3]
                            where
                                s1 = concat ["case ",show v1," of "]
                                s2 = concat ["[] -> ",go e1 d,";"]
                                s3 = concat [show v2,":",show v3," -> ",go e2 d,";"]
                        ExpStepCase v1 e1 v2 v3 e2 _ -> concat [s1,s2,s3]
                            where
                                s1 = concat ["case ",show v1," of "]
                                s2 = concat ["Done -> ",go e1 d,";"]
                                s3 = concat ["Yield ",show v2," ",show v3," -> ",go e2 d,";"]
                        ExpLet v1 _ e1 e2 _ -> concat ["let ",show v1,"=",go e1 d,"; in ", go e2 d]
                        ExpIf e1 e2 e3 _ -> concat [s1,s2,s3]
                            where 
                                s1 = concat ["if ",go e1 d,""]
                                s2 = concat [" then ",go e2 d,""]
                                s3 = concat [" else ",go e3 d,""]
                        ExpInt n -> show n
                        ExpBool b -> show b 
                        ExpNil _ -> "[]"
                        ExpAdd e1 e2 _ -> concat [go e1 d," + ",go e2 d]
                        ExpAnd e1 e2 _ -> concat [go e1 d," && ",go e2 d]
                        ExpLT e1 e2 _  -> concat [go e1 d," < ",go e2 d]
                        ExpCons e1 e2 _ -> concat [go e1 d,":",go e2 d]
                        ExpVar v _ -> show v
                        ExpDone -> "Done"
                        ExpSkip e1l -> concat ["Skip (",concat (listgo e1l d True),")"]
                            where 
                                listgo explist d b = case explist of 
                                    []          -> replicate d ",Nothing"
                                    exp:exps    -> (concat [comma,s1]):(listgo exps d False)
                                        where 
                                            comma = if b then "" else ","
                                            s1 = go exp d
                        ExpYield e1 e2l adls -> concat ["Yield ",go e1 d," (",concat (listgo e2l d True),")"]
                            where 
                                listgo explist d b = case explist of 
                                    []          -> adslgo adls
                                        where 
                                            adslgo adls1 = case adls1 of 
                                                []      -> replicate (d-(length adls)) ",Nothing"
                                                ads:adsl' -> (concat [",",go ads d]):(adslgo adsl')
                                    exp:exps    -> (concat [comma,s1]):(listgo exps d False)
                                        where 
                                            comma = if b then "" else ","
                                            s1 = go exp d
                        ExpStream v1 vsin vsf n e1 -> case e1 of 
                            --ExpUNK -> concat ["(Stream next_",show v1," ",getStateStr vsf,")"]
                            ExpUNK    -> error "try to show ExpUNK (of ExpStream)"
                            otherwise -> concat [s0,s1,s2,s3]
                                where 
                                    s0 = "let "
                                    s1 = getStackedStreamValueStr v1 vsf d
                                    s2 = concat ["next_",show v1, " (",getStateStr vsf,concat(replicate d ",Nothing"),") = ",getStreamSkipBranch vsin (go e1 d)]
                                    s3 = concat [" in (Stream next_", show v1," (",getStateStr vsin,concat(replicate d ",Nothing"),"))"]
                        ExpUNK -> error "Try to show ExpUNK" 

getStreamSkipBranch :: [Var] -> String -> String 
getStreamSkipBranch vsin sexp = go 1
    where 
        go i = if (i > (length vsin))
            then sexp
            else case vsin!!(i-1) of 
                StreamVar v -> concat ["(case ",s1," of Skip ",s2," -> Skip (",s3,"); otherwise ->",go (i+1),";)"]
                    where 
                        sv1 = concat ["next_",v]
                        sv2 = concat ["nstate_",v]
                        s1 = concat [sv1," ",sv2]
                        s2 = concat ["n",sv2]
                        s3 = concat (gosub 1)
                            where 
                                gosub k = if (k > (length vsin))
                                    then []
                                    else (concat [prefix1,prefix2,varstr]):(gosub (k+1))
                                        where
                                            prefix1 = if k==1 then "" else ","
                                            prefix2 = if k==i then "n" else ""
                                            varstr = case vsin!!(k-1) of 
                                                StepVar v1 v2 -> error "unexpected"
                                                StreamVar v1 -> concat ["nstate_",v1]
                                                otherwise -> show (vsin!!(k-1))
                StepVar _ _ -> error "unexpected"
                otherwise -> go (i+1)

getStackedStreamValueStr :: Var -> [Var] -> Int -> String
getStackedStreamValueStr f vsf d0 = concat (go 1)
    where 
        go d = if d > d0 
            then []
            else s:(go (d+1))
                where 
                    sargs = concat (getargs 1)
                        where 
                            getargs i = if i < d
                                then ",Nothing":(getargs (i+1))
                                else if i == d 
                                    then ",Just a":(getargs (i+1))
                                    else if i < d0 
                                        then ",_":(getargs (i+1))
                                        else []
                    s = concat ["next_",show f," (",getStateStr vsf,sargs,") = a; "]

getStateStr :: [Var] -> String
getStateStr vl = concat (go vl True)
    where 
        go listvar b = case listvar of 
            []      -> []
            v:vs    -> (concat [prefix,varstr]):(go vs False)
                where 
                    prefix = if b then "" else ","
                    varstr = case v of 
                        StepVar v1 v2 -> v2
                        StreamVar v1 -> concat ["state_",v1]
                        otherwise -> show v
                
instance Show Var where
    show var0 = case var0 of 
        Var v -> v 
        StreamVar v -> concat ["(Stream next_",v," state_",v,")"]
        StepVar v1 v2 -> concat [v1," ",v2]

getExpType :: Exp -> Typ
getExpType e0 = case e0 of 
    ExpLam _ _ _ te  -> te 
    ExpApp _ _ te -> te 
    ExpLet _ _ _ _ te -> te 
    ExpCase _ _ _ _ _ te -> te 
    ExpIf _ _ _ te -> te
    ExpInt n -> TypInt 
    ExpBool b -> TypBool 
    ExpNil te -> te 
    ExpAdd _ _ te -> te 
    ExpLT _ _ te -> te 
    ExpAnd _ _ te -> te 
    ExpCons _ _ te -> te 
    ExpVar _ te -> te 
    otherwise -> error "unimplemented: getExpType for stream exp"

getVarType :: [(String, Typ)] -> String -> Typ
getVarType env v = case env of 
    []      -> error (concat ["Untyped variable: ", v])
    x:xs    -> if (fst x) == v
        then snd x
        else getVarType xs v

isfixpoint :: Exp -> Bool
isfixpoint e = case e of 
    ExpLet (Var v) typeOfv e1 e2 te0 -> 
        case typeOfv of
            TypLam st1 st2 -> True -- TODO
            otherwise -> False
    otherwise   -> False

nofixpoint :: Exp -> Bool
nofixpoint e0 = go e0 
    where 
        go e = case e of 
            ExpLet v typeOfv e1 e2 te   -> (not (isfixpoint e)) && (go e1) && (go e2)
            ExpLam v typeOfv e1 te      -> go e1 
            ExpApp e1 e2 te             -> (go e1) && (go e2)
            ExpCase _ e1 _ _ e2 te      -> (go e1) && (go e2)
            ExpIf e1 e2 e3 te           -> (go e1) && (go e2) && (go e3)
            ExpInt n                    -> True 
            ExpBool b                   -> True 
            ExpNil te                   -> True 
            ExpAdd e1 e2 te             -> (go e1) && (go e2)
            ExpLT e1 e2 te              -> (go e1) && (go e2)
            ExpAnd e1 e2 te             -> (go e1) && (go e2)
            ExpCons e1 e2 te            -> (go e1) && (go e2)
            ExpVar v te                 -> True 
            otherwise                   -> error "error in func nofixpoint"

replaceUNK :: (String, Exp) -> Exp -> (String, Exp)
replaceUNK fixp e0 = (fst fixp, go (snd fixp) e0)
    where
        go e1 e2 = case e1 of
            ExpUNK      -> e0
            ExpLam v typeOfv e1sub te0 -> ExpLam v typeOfv (go e1sub e0) te0

rewrite :: Exp -> [(String, Typ)] -> (String, Exp) -> Exp 
rewrite e0 env fixp = 
    case getExpType e0 of
        TypStream st1
            | (nofixpoint e0) && (not ((fst fixp)=="_")) -> constructStream e0 env fixp
        otherwise 
            -> case e0 of 
                ExpLam (Var v) typeOfv e1 te0 -> ExpLam newvar typeOfv e1' te0
                    where
                        newvar = case typeOfv of 
                            TypStream stOfv -> StreamVar v
                            otherwise       -> Var v
                        e1' = case te0 of 
                            TypLam st1 st2  -> rewrite e1 ((v,typeOfv):env) fixp'
                                where 
                                    fixp' = if (fst fixp) == "_" 
                                        then fixp
                                        else replaceUNK fixp (ExpLam newvar typeOfv ExpUNK te0) 
                            otherwise      -> error "unexpected: rewrite ExpLam"
                ExpApp e1 e2 te0 -> ExpApp e1' e2' te0
                    where 
                        e2' = rewrite e2 env fixp -- rewrite e2 first because e1 may contain stream var
                        e1' = rewrite e1 env fixp
                        -- TODO: what if fst fixp != "_"
                ExpLet (Var v) typeOfv e1 e2 te0 -> ExpLet newvar typeOfv e1' e2' te0
                    where
                        newvar = case typeOfv of 
                            TypStream stOfv -> StreamVar v
                            otherwise       -> Var v
                        e1' = if isfixpoint e0 
                            then rewrite e1 ((v,typeOfv):env) (v, ExpUNK)
                            else rewrite e1 ((v,typeOfv):env) fixp          -- TODO
                        e2' = if isfixpoint e0 
                            then rewrite e2 ((v,typeOfv):env) ("_",ExpUNK)
                            else rewrite e2 ((v,typeOfv):env) fixp          -- TODO
                ExpCase (Var v1) e1 (Var v2) (Var v3) e2 te0 -> e0'
                    where 
                        tv1 = getVarType env v1
                        (tv2, tv3) = case tv1 of 
                            TypList st  -> (st, tv1)
                            otherwise   -> error "Unhandled case: rewrite ExpCase" 
                        e1' = rewrite e1 env fixp -- TODO: handle fixp
                        e2' = rewrite e2 ((v2,tv2):(v3,tv3):env) fixp
                        e0' = ExpCase (Var v1) e1' (Var v2) (Var v3) e2' te0
                ExpIf e1 e2 e3 te0 -> ExpIf e1' e2' e3' te0
                    where
                        e1' = rewrite e1 env fixp
                        e2' = rewrite e2 env fixp -- TODO: handle fixp
                        e3' = rewrite e3 env fixp
                ExpAdd e1 e2 te0 -> ExpAdd e1' e2' te0 
                    where 
                        e1' = rewrite e1 env fixp 
                        e2' = rewrite e2 env fixp -- TODO: handle fixp
                ExpAnd e1 e2 te0 -> ExpAnd e1' e2' te0 
                    where 
                        e1' = rewrite e1 env fixp 
                        e2' = rewrite e2 env fixp -- TODO: handle fixp
                ExpInt n -> ExpInt n 
                ExpBool b -> ExpBool b 
                ExpNil te0 -> case te0 of 
                    TypStream _ -> error "unexpected: [] of TypStream Outside stream"
                    otherwise -> ExpNil te0 -- TODO
                ExpLT e1 e2 te0 -> ExpLT e1' e2' te0 
                    where 
                        e1' = rewrite e1 env fixp 
                        e2' = rewrite e2 env fixp -- TODO: handle fixp
                ExpCons e1 e2 te0 -> ExpCons e1' e2' te0 -- TODO 
                    where 
                        e1' = rewrite e1 env fixp 
                        e2' = rewrite e2 env fixp -- TODO: handle fixp
                ExpVar (Var v) te0 -> case te0 of 
                    TypStream _ -> ExpVar (StreamVar v) te0
                    otherwise   -> ExpVar (Var v) te0 
                otherwise -> error "Unhandled error: rewrite e0"

constructStream :: Exp -> [(String, Typ)] -> (String, Exp) -> Exp
constructStream e0 env0 fixp = ExpStream vstrm ivlstrm nvlstrm nads expStream
    where 
        tstream = (getExpType e0)
        vstrm = Var (fst fixp) 
        vlstrm = getAllStates fixp
        renaming0 = getRenaming vlstrm
        ivlstrm = pickStates vlstrm
        nvlstrm = renameStreamArgs renaming0 vlstrm
        expFixp = consFixp (snd fixp)
            where 
                consFixp e = case e of
                    ExpLam var1 typeOfv1 e1 te0 -> case var1 of 
                        StreamVar v1    -> ExpLam (getRename renaming0 v1) typeOfv1 e1' te0
                            where 
                                e1' = consFixp e1
                        Var v1          -> ExpLam (getRename renaming0 v1) typeOfv1 e1' te0
                            where e1' = consFixp e1
                        otherwise       -> error "Unexpected: stepvar outside stream"
                    --ExpUNK -> ExpStream vstrm vlstrm nvlstrm ExpUNK -- Stream Value
                    ExpUNK -> ExpSkip (vars2exps nvlstrm)
                    otherwise       -> error "unhandled case: fixpoint contains productions other than lam" --TODO 
        (expStream, nads) = subcons e0 env0 renaming0
            where 
                subcons e env renaming = case e of 
                    ExpLam (Var v) typeOfv e1 te0 -> error "Unhandled case: constructing stream for ExpLam"
                    ExpApp e1 e2 te0 -> (e0',nads')
                        where 
                            (e1',nads1) = subcons e1 env renaming
                            (e2',nads2) = subcons e2 env renaming -- TODO: reduce?
                            e0' = if reducible e1' e2'
                                then reduceApp e1' e2'
                                else ExpApp e1' e2' te0
                            nads' = maximum [nads1,nads2]
                    ExpLet (Var v) typeOfv e1 e2 te0 -> error "Unhandled case: constructing stream for ExpLam"
                    ExpCase (Var v1) e1 (Var v2) (Var v3) e2 te0 -> (e0',nads')
                        where 
                            tv1 = getVarType env v1
                            (e0',nads') = case hasRename renaming v1 of
                                True -> (ExpStepCase rv1 e1' (Var v2) (Var v3') e2' te0, nads')
                                    where 
                                        rv1 = getRename renaming v1
                                        (v3', rv3) = case rv1 of 
                                            StepVar v11 v12 -> (concat ["n",v12], StepVar v11 (concat ["n",v12]))
                                            otherwise       -> error "Unexpected: renamed var is not stepvar"
                                        (e1',nads1) = subcons e1 env renaming
                                        (e2',nads2) = subcons e2 env ((v3, rv3):renaming)
                                        nads' = maximum [nads1,nads2]
                                False -> case tv1 of 
                                    TypStream _     -> error (concat ["Unexpected: undefined stream var ",v1])
                                    TypStep _ _     -> error "Unexpected: undefined stream var"
                                    TypState _      -> error "Unexpected: undefined stream var"
                                    TypList st1     -> (ExpCase (Var v1) e1' (Var v2) (Var v3) e2' te0, nads')
                                        where 
                                            (e1',nads1) = subcons e1 env renaming
                                            (e2',nads2) = subcons e2 ((v2,st1):(v3,tv1):env) renaming
                                            nads' = maximum [nads1,nads2]
                                    otherwise       -> error "Syntax error: case exp for list var only"
                    ExpIf e1 e2 e3 te0 -> (ExpIf e1' e2' e3' te0, nads')
                        where 
                            (e1',nads1) = subcons e1 env renaming
                            (e2',nads2) = subcons e2 env renaming
                            (e3',nads3) = subcons e3 env renaming
                            nads' = if nads1>0 
                                then error "Unimplemented: " 
                                else maximum [nads2,nads3]
                    ExpAdd e1 e2 te0 -> (ExpAdd e1' e2' te0, nads')
                        where 
                            (e1',nads1) = subcons e1 env renaming
                            (e2',nads2) = subcons e2 env renaming
                            nads' = maximum [nads1,nads2]
                    ExpAnd e1 e2 te0 -> (ExpAnd e1' e2' te0, nads')
                        where 
                            (e1',nads1) = subcons e1 env renaming
                            (e2',nads2) = subcons e2 env renaming
                            nads' = maximum [nads1,nads2]
                    ExpInt n -> (ExpInt n, 0)
                    ExpBool b -> (ExpBool b, 0)
                    ExpNil te0 -> case te0 of 
                        TypStream _ -> (ExpDone, 0)
                        otherwise   -> (ExpNil te0, 0)
                    ExpLT e1 e2 te0 -> (ExpLT e1' e2' te0, nads')
                        where 
                            (e1',nads1) = subcons e1 env renaming
                            (e2',nads2) = subcons e2 env renaming
                            nads' = maximum [nads1,nads2]
                    ExpCons e1 e2 te0 -> case te0 of 
                        TypStream _ -> (e0', nads')
                            where 
                                (e1',nads1) = subcons e1 env renaming
                                (e2',nads2) = subcons e2 env renaming -- TODO: try reducing e2
                                (e0',nads') = case e2' of
                                    ExpSkip e2l -> (ExpYield e1' e2l [], 0)
                                    ExpYield e21 e22 adsl -> (ExpYield e1' e22 (e21:adsl), (length adsl)+1)
                                    otherwise -> error "unimplemented"
                        otherwise   -> (ExpCons e1' e2' te0, nads')
                            where 
                                (e1',nads1) = subcons e1 env renaming
                                (e2',nads2) = subcons e2 env renaming
                                nads' = maximum [nads1,nads2]
                    ExpVar (Var v) te0 -> (e0',0)
                        where 
                            e0' = if v == (fst fixp)
                                then expFixp -- TODO 
                                else if hasRename renaming v
                                    then ExpVar (getRename renaming v) te0
                                    else e 
{- judge if an application (\x.e1 e2) can be reduced by e1[x|->e2] -}
reducible :: Exp -> Exp -> Bool
reducible func e0 = case func of 
    ExpLam var tvar e1 tfunc    -> case var of 
        Var v -> reducibleByVar e1 v
        StepVar v1 v2 -> case e0 of 
            ExpVar (StepVar v01 v02) _ | v01==v1 -> reducibleByVar e1 v2 -- TODO: decide type
            otherwise -> False
    otherwise -> False 

{- judge if not any let/case in e -}
reducibleByVar :: Exp -> String -> Bool 
reducibleByVar exp0 v0 = case exp0 of 
    ExpLam var typeOfv e1 te0 -> case var of 
        Var v -> if v==v0 
            then True 
            else reducibleByVar e1 v0
        otherwise -> True -- TODO: check if var contains v0
    ExpApp e1 e2 te0 -> (reducibleByVar e1 v0) && (reducibleByVar e2 v0)
    ExpAdd e1 e2 te0 -> (reducibleByVar e1 v0) && (reducibleByVar e2 v0)
    ExpAnd e1 e2 te0 -> (reducibleByVar e1 v0) && (reducibleByVar e2 v0)
    ExpLT e1 e2 te0 -> (reducibleByVar e1 v0) && (reducibleByVar e2 v0)
    ExpCons e1 e2 te0 -> (reducibleByVar e1 v0) && (reducibleByVar e2 v0)
    ExpIf e1 e2 e3 te0 -> (reducibleByVar e1 v0) && (reducibleByVar e2 v0) && (reducibleByVar e3 v0)
    ExpInt _ -> True 
    ExpBool _ -> True 
    ExpVar _ _ -> True 
    ExpNil _ -> True
    ExpDone -> True
    ExpSkip elist -> foldr (\x-> \y-> x&&y) True (map (\x->reducibleByVar x v0) elist)
    ExpYield e1 elist adsl -> b1 && b2 && b3
        where 
            b1 = reducibleByVar e1 v0 
            b2 = foldr (\x-> \y-> x&&y) True (map (\x->reducibleByVar x v0) elist)
            b3 = foldr (\x-> \y-> x&&y) True (map (\x->reducibleByVar x v0) adsl)
    otherwise -> False -- TODO: implement reduction for let, case

{- reduce an application (\x.e1 e2)  by e1[x|->e2] -}
reduceApp :: Exp -> Exp -> Exp 
reduceApp func e0 = case func of 
    ExpLam var tvar e1 tfunc    -> case var of 
        Var v -> replaceVar e1 v e0
        StepVar v1 v2 -> case e0 of 
            ExpVar (StepVar v01 v02) _ | v01==v1 -> replaceVar e1 v2 (ExpVar (Var v02) TypUNK) -- TODO: decide type
            otherwise -> error "cannot reduce"
    otherwise -> error "Cannot reduce expression"

{- helper func of reduceApp -}
replaceVar :: Exp -> String -> Exp -> Exp
replaceVar exp0 v0 rexp = case exp0 of 
    ExpLam var typeOfv e1 te0 -> case var of 
        Var v -> case v==v0 of
            True    -> exp0
            False   -> ExpLam (Var v) typeOfv e1' te0 
                where 
                    e1' = replaceVar e1 v0 rexp
        otherwise -> ExpLam var typeOfv e1' te0  -- TODO: check if var contains v0
            where 
                e1' = replaceVar e1 v0 rexp
    ExpApp e1 e2 te0 -> ExpApp e1' e2' te0
        where 
            e1' = replaceVar e1 v0 rexp
            e2' = replaceVar e2 v0 rexp
    ExpAdd e1 e2 te0 -> ExpAdd e1' e2' te0
        where 
            e1' = replaceVar e1 v0 rexp
            e2' = replaceVar e2 v0 rexp
    ExpAnd e1 e2 te0 -> ExpAnd e1' e2' te0
        where 
            e1' = replaceVar e1 v0 rexp
            e2' = replaceVar e2 v0 rexp
    ExpLT e1 e2 te0 -> ExpLT e1' e2' te0
        where 
            e1' = replaceVar e1 v0 rexp
            e2' = replaceVar e2 v0 rexp
    ExpCons e1 e2 te0 -> ExpCons e1' e2' te0
        where 
            e1' = replaceVar e1 v0 rexp
            e2' = replaceVar e2 v0 rexp
    ExpIf e1 e2 e3 te0 -> ExpIf e1' e2' e3' te0
        where 
            e1' = replaceVar e1 v0 rexp
            e2' = replaceVar e2 v0 rexp
            e3' = replaceVar e3 v0 rexp
    ExpInt n -> ExpInt n 
    ExpBool b -> ExpBool b
    ExpVar var t -> case var of 
        Var v           -> if v==v0 then rexp else exp0
        StreamVar v     -> error "Unexpected case: StreamVar within stream"
        StepVar v1 v2   -> if v1==v0 || v2==v0
            then error "Unhandled case: !"
            else exp0
    ExpNil t -> ExpNil t
    ExpDone -> ExpDone
    ExpSkip elist -> ExpSkip (map (\x->replaceVar x v0 rexp) elist)
    ExpYield e1 elist adsl -> ExpYield e1' elist' adsl'
        where 
            e1' = replaceVar e1 v0 rexp
            elist' = map (\x->replaceVar x v0 rexp) elist
            adsl' = map (\x->replaceVar x v0 rexp) adsl
    otherwise -> error "unimplemented reduction"

vars2exps :: [Var] -> [Exp]
vars2exps vars = case vars of 
    []      -> []
    v:vs    -> (ExpVar v TypUNK):(vars2exps vs) -- TODO: decide type

getAllStates :: (String, Exp) -> [Var]
getAllStates fixp = go (snd fixp) 
    where 
        go e = case e of 
            ExpUNK -> [] 
            ExpLam var1 typeOfv e1 te -> case var1 of
                StreamVar v     -> var1:(go e1)
                Var v           -> var1:(go e1)
                otherwise       -> error "Unexpected: Step value outside stream"
            otherwise -> error "Not Implemented Error: getAllStates" -- TODO 

pickStates :: [Var] -> [Var]
pickStates l = case l of 
    []      -> []
    v:vs    -> case v of 
        Var v1          -> v:(pickStates vs)
        StreamVar v1    -> v:(pickStates vs)
        StepVar v1 v2   -> error "unexpected"

getRenaming :: [Var] -> [(String, Var)]
getRenaming vlstrm = case vlstrm of 
    []      -> []
    x:xs    -> case x of 
        Var v           -> newrename:(getRenaming xs)
            where 
                newrename = (v, Var (concat ["ns_",v]))
        StreamVar v     -> newrename:(getRenaming xs)
            where 
                newrename = (v, StepVar (concat ["next_",v]) (concat ["nstate_",v]))
        otherwise       -> error "Unexpected Stream args"

hasRename :: [(String, Var)] -> String -> Bool 
hasRename renaming v = case renaming of 
    []      -> False
    (name,rv):xs    -> if name == v 
        then True
        else hasRename xs v 
        
getRename :: [(String, Var)] -> String -> Var 
getRename renaming v = case renaming of 
    []      -> error "Unexpected: renaming not found for variable"
    (name,rv):xs    -> if name == v 
        then rv 
        else getRename xs v 

renameStreamArgs :: [(String, Var)] -> [Var] -> [Var]
renameStreamArgs renaming vargs = case vargs of 
    []      -> []
    v:vs    -> nv:(renameStreamArgs renaming vs)
        where 
            name = case v of 
                Var x       -> x 
                StreamVar x -> x 
                otherwise   -> error "Unexpected: renaming StepVar"
            rn = getRename renaming name
            nv = case rn of 
                StepVar v1 v2 -> Var v2 
                otherwise -> rn

listfunc2streamfunc :: [(String, Typ)] -> Exp -> Exp
listfunc2streamfunc env e0 = rewrite e0 env ("f", ExpUNK)


tsi = TypStream TypInt
tsi2si = TypLam tsi tsi
esinil = ExpNil tsi
ti2i = TypLam TypInt TypInt

test0 = ExpLam (Var "l") tsi (vany "l" tsi) tsi2si

ecase1sub2 = ExpCons (ExpAdd (ExpVar (Var "h") TypInt) (ExpInt 1) TypInt) (ExpApp (ExpVar (Var "f") tsi2si) (ExpVar (Var "t") tsi) tsi) tsi
ecase1 = ExpCase (Var "l") esinil (Var "h") (Var "t") ecase1sub2 tsi
test1 = ExpLam (Var "l") tsi ecase1 tsi2si 

vany a t = ExpVar (Var a) t
tc2 = TypLam ti2i tsi2si
econs1 = ExpApp (vany "g" ti2i) (vany "h" TypInt) TypInt
econs2 = ExpApp (ExpApp (vany "f" tc2) (vany "g" ti2i) tsi2si) (vany "t" tsi) tsi
ecase2sub2 = ExpCons econs1 econs2 tsi
ecase2 = ExpCase (Var "l") esinil (Var "h") (Var "t") ecase2sub2 tsi
e2lam = ExpLam (Var "l") tsi ecase2 tsi2si
test2 = ExpLam (Var "g") ti2i e2lam tc2

tc3 = TypLam TypInt tsi2si
e3body = ExpCons (vany "x" TypInt) (vany "l" tsi) tsi
e3lam = ExpLam (Var "l") tsi e3body tsi2si
test3 = ExpLam (Var "x") TypInt e3lam tc3

ti2si = TypLam TypInt tsi
tgo4 = TypLam tsi ti2si
ec1 = ExpAdd (vany "y" TypInt) (vany "h" TypInt) TypInt
ec2 = ExpApp (ExpApp (vany "go" tgo4) (vany "t" tsi) ti2si) (ExpAdd (vany "y" TypInt) (vany "h" TypInt) TypInt) tsi
e4casec = ExpCons ec1 ec2 tsi
e4lsss = ExpCase (Var "x") (ExpNil tsi) (Var "h") (Var "t") e4casec tsi
e4lss = ExpLam (Var "y") TypInt e4lsss ti2si
e4lamsub1 = ExpLam (Var "x") tsi e4lss tgo4
e4lamsub2 = ExpApp (ExpApp (vany "go" tgo4) (vany "l" tsi) ti2si) (ExpInt 0) tsi
e4lam = ExpLet (Var "go") tgo4 e4lamsub1 e4lamsub2 tsi
test4 = ExpLam (Var "l") tsi e4lam tsi2si 

main = do 
    let f0 =  concat ["f=",(show test0)]
    let sf0 = concat ["f=",(show (listfunc2streamfunc [("f",tsi2si)] test0))]
    let f1 =  concat ["f=",(show test1)]
    let sf1 = concat ["f=",(show (listfunc2streamfunc [("f",tsi2si)] test1))]
    let f2 =  concat ["f=",(show test2)]
    let sf2 = concat ["f=",(show (listfunc2streamfunc [("f",tc2)] test2))]
    --let f3 =  concat ["f=",(show test3)]
    --let sf3 = concat ["f=",(show (listfunc2streamfunc [("f",tc3)] test3))]
    let f4 =  concat ["f=",(show test4)]
    let sf4 = concat ["f=",(show (listfunc2streamfunc [("f",tsi2si)] test4))]
    writeFile "outputs/func0.hs" f0
    writeFile "outputs/streamfunc0.hs" sf0
    writeFile "outputs/func1.hs" f1
    writeFile "outputs/streamfunc1.hs" sf1
    writeFile "outputs/func2.hs" f2
    writeFile "outputs/streamfunc2.hs" sf2
    --writeFile "outputs/func3.hs" f3
    --writeFile "outputs/streamfunc3.hs" sf3
    writeFile "outputs/func4.hs" f4
    writeFile "outputs/streamfunc4.hs" sf4
