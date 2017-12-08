module
( Convert
, PushState
)
where
    
------------------------------------------------------------------------
import          TTypes
import          TypedProgram
import          StreamProgram
------------------------------------------------------------------------


PushState sstate (SLambda sv se) = SLambda sv (PushState sstate se)
PushState sstate (SStream snext lsstate SExp) = SStream snext sstate:lsstate SExp

Convert texp env = if isBasicType (getExpType texp)
    then Rewrite texp env
    else case texp of
        (TLambda (TVar tv vtt) te tt)   -> case vtt of
            TSList _    -> SLambda (SStreamVar snext sstate) (PushState sstate se) 
                where
                    snext = concat ["next_",tv]
                    sstate = concat ["state_",tv]
                    se = Convert te (tv,"!SV"):env
            otherwise   -> SLambda (SVar tv) (PushState tv se)
                where
                    se = Convert te env
        -- note that in "\x.e", we only consider type(x) to be basic type or type(x)=TSList _
        (TApp te1 te2 tt)               -> SApp se1 se2
            where
                se1 = Convert te1 env
                se2 = Convert te2 env
        -- need to resolve the case of recursive func def ... let go=\.. in go l 0
        (TLet (TVar tv vtt) te1 te2 tt)  -> case vtt of
            TSList _    -> SLet (SStreamVar snext sstate) se1 (PushState sstate se2)
                where
                    snext = concat ["next_",tv]
                    sstate = concat ["state_",tv]
                    se1 = Convert te1 env
                    se2 = Convert te2 (tv,"!SV"):env
            otherwise   -> SLet (SVar tv) se1 (PushState tv se)
                where
                    se1 = Convert te1 env
                    se2 = Convert te2 env
        -- need to resolve the case of recursive func def ... let go=\.. in go l 0
        (TCase (TVar tv1 vtt1) (TVar tv2 vtt2) (TVar tv3 vtt 3) te1 te2 tt) -> case vtt1 of
            TSList  _   -> let ssv = 
            otherwise   -> SCase (SVar tv1) (SVar tv2) (SVar tv3) se1 se2
                where 
                    se1 = Convert te1 env
                    se2 = Convert te2 env   -- add env?


Rewrite texp env = 