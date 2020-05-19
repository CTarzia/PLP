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
        OK (nu, (cu, u', tu)) ->
            case infer' v nu of
                OK (nv, (cv, v', tv)) -> 
                    case mgu (
                        (tu, (TFun tv (TVar nv))) 
                        : [(evalC cu var1, evalC cv var2) |
                            var1 <- domainC cu,
                            var2 <- domainC cv,
                            var1 == var2]
                    ) of
                            UOK subst ->
                                OK
                                (
                                    nv + 1,
                                    (
                                        joinC [subst <.> cu, subst <.> cv],
                                        subst <.> (AppExp u' v'),
                                        subst <.> (TVar nv)
                                    )
                                )
                            UError u1 u2 -> uError u1 u2
                res@(Error _) -> res
        res@(Error _) -> res

infer' (LamExp x () e)     n =
    case infer' e n of
        OK ( n', (c', e', t') ) -> 
            OK (
                nRes, (
                    removeC c' x,
                    (LamExp x xType e'),
                    (TFun xType t')
                )
            )
            where nRes = n' + if x `elem` domainC c' then 1 else 0
                  xType = if x `elem` domainC c' then evalC c' x else (TVar n')
        res@(Error _) -> res

infer' (SuccExp e) n =
    case infer' e n of
        OK ( n', (c', e', t') ) ->
            case mgu [ (t', TNat) ] of
                UOK subst ->
                    OK ( n', (subst <.> c',subst <.> SuccExp e',TNat) )
                UError u1 u2 ->uError u1 u2
        res@(Error _) -> res


--------------------------------
-- YAPA: Error de unificacion --
--------------------------------
uError :: Type -> Type -> Result (Int, a)
uError t1 t2 = Error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
