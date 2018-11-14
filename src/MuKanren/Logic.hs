module MuKanren.Logic where

import Control.Applicative
import Control.Monad
import qualified Data.Map as M

import MuKanren.Types


-- | = Terms and substitutions

-- | Apply a substitution to the top level of a term.
-- See `unify` for why we do not need to address the case of a Pair input.
-- See `walk'` for something that does.
walk :: Term -> Subst -> Term
walk (Var v) s = case lookup v s of Nothing -> Var v
                                    Just us -> walk us s
walk t s = t

extS :: Var -> Term -> Subst -> Subst
extS v t = (:) (v, t)

-- | Try to unify two terms under a Subst.
-- Return an extended Subst if it succeeds.
unify :: Term -> Term -> Subst -> Maybe Subst
unify u v s = go (walk u s) (walk v s) where
  go (Var v1)     (Var v2) | v1 == v2   = return s
  go (Var v)      t                     = return $ extS v t s
  go t            (Var v)               = return $ extS v t s
  go (Pair u1 u2) (Pair v1 v2)          = unify u1 v1 s >>= unify u2 v2
  go (Atom a1)    (Atom a2) | a1 == a2  = return s
  go _            _                     = mzero


-- | = MicroKanren program formers

zzz :: Program -> Program
zzz g = \sc -> Delay $ g sc

-- | TODO (#extend) use an "occurs check" to prohibit circularities.
-- (Example in paper.)
equiv :: Term -> Term -> Program
equiv u v = \(s, c) -> case unify u v s of
  Nothing -> mzero
  Just s' -> return (s', c)

callFresh :: (Term -> Program) -> Program
callFresh f = \(s, c) -> f (Var c) (s, c+1)

disj :: Program -> Program -> Program
disj g1 g2 = \sc -> mplus (g1 sc) (g2 sc)

conj :: Program -> Program -> Program
conj g1 g2 = \sc -> g1 sc >>= g2

klistToList :: KList a -> [a]
klistToList Nil            = []
klistToList (Delay xs)     = klistToList xs
klistToList (x `Cons` xs)  = x : klistToList xs
