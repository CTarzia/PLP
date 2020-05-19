module TypeInference (TypingJudgment, Result(..), inferType)

where

import Data.List(intersect)
import Exp
import Type
import Unification

------------
-- Errors --
------------
data Result a = OK a | Error String


--------------------
-- Type Inference --
--------------------
type TypingJudgment = (Context, AnnotExp, Type)


inferType :: PlainExp -> Result TypingJudgment
inferType e = case infer' e 0 of
    OK (_, tj) -> OK tj
    Error s -> Error s


infer' :: PlainExp -> Int -> Result (Int, TypingJudgment)

-- COMPLETAR DESDE AQUI

infer' (VarExp x)     n = OK (n + 1,(
                                  extendC emptyContext x (TVar n),
                                  (VarExp x),
                                  (TVar n)
                               )
                          )
infer' ZeroExp        n = OK (n, (
                                  emptyContext,
                                  ZeroExp,
                                  TNat
                               )
                          )
infer' (AppExp u v)   n = 
    case infer' u n of
        OK (n', (c', e', t')) ->
            case infer' v n' of
                OK (n'', (c'', e'', t'')) -> 
                    case mgu (
                        (t', (TFun t'' (TVar n''))) 
                        : [(evalC var1 c', evalC var2 c'') |
                            var1 <- domainC c',
                            var2 <- domainC c'',
                            var1 == var2]) of
                            UOK subst -> OK (
                                n'' + 1,
                                (
                                    joinC [subst <.> c', subst <.> c''],
                                    subst <.> (AppExp e' e''),
                                    subst <.> (TVar n'')
                                )
                            )
                            UError u1 u2 -> uError u1 u2
                res@(Error _) -> res
        res@(Error _) -> res
-- infer' LamExp



--------------------------------
-- YAPA: Error de unificacion --
--------------------------------
uError :: Type -> Type -> Result (Int, a)
uError t1 t2 = Error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
