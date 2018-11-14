-- | Translated from the original Scheme paper.
--
-- TODO : Reification seems broken:
  -- > mK_reify $ runTest $ conj five six
  -- [Atom "5"]
  -- > runTest $ conj five six
  -- [([(1,Atom "6"),(0,Atom "5")],2)]

module MuKanren.Reify where

import MuKanren.Main


walk' :: Term -> Subst -> Term
walk' t s = let v = walk t s
  in case v of Pair p q -> Pair (walk' p s) (walk' q s)
               _        -> v

mK_reify :: Functor f => f State -> f Term
mK_reify = fmap reify_head

reify_head :: State ->  Term -- TODO (#safe) accept Subst, not State
reify_head (s,_) = let v = walk' (Var 0) s
  in walk' v $ reify_s v []

reify_s :: Term -> Subst -> Subst
reify_s t s = let t' = walk t s in case t' of
  Var v    -> let n = reify_name $ length s
              in (v, Atom n) : s
  Pair p q -> reify_s q $ reify_s p s
  _        -> s

reify_name :: Int -> String
reify_name i = "_." ++ show i
