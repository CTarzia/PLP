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

infer' (VarExp x)     n = undefined




--------------------------------
-- YAPA: Error de unificacion --
--------------------------------
uError :: Type -> Type -> Result (Int, a)
uError t1 t2 = Error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
