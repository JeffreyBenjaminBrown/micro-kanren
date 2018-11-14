module MuKanren.Main where

import Control.Applicative
import Control.Monad
import qualified Data.Map as M


type Var     = Int
type Subst   = [(Var, Term)]
type State   = (Subst, Int) -- ^ TODO (#grok) Int because not a Var yet?
type Program = State -> KList State -- ^ the paper calls `Program`s "goals"

-- | From the paper: "terms of the language consist of variables, objects
-- deemed identical under eqv?, and pairs of the foregoing".
-- TODO (#grok) Where do Pairs come from? What are they for?
  -- No function below generates a Pair except in response to one.
  -- If I comment out every mention of them, the tests still run fine.
  -- This stackoverflow question suggests it means lists, not pairs.
  -- https://stackoverflow.com/questions/28658148/microkanren-what-are-the-terms
data Term = Atom String | Pair Term Term | Var Var deriving Show

-- | Unlike the original paper, in Haskell we don't need to eta-expand.
-- We just need to add a marker.
-- TODO (#grok) That marker is the Delay constructor, right?
data KList a = Nil | Cons a (KList a) | Delay (KList a)
  deriving (Show, Eq, Ord)

instance Functor KList where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) $ fmap f as
  fmap f (Delay as)  = Delay $ fmap f as

instance Applicative KList where
  pure a = Cons a Nil
  (<*>) _ _ = error "<*> undefined for KList."
    -- TODO (#grok) Should `<*>` mean something for `KList`s?

instance Alternative KList where
  empty = Nil
  (<|>) = mplus -- TODO (#grok) ? This is just a guess.

instance Monad KList where
  return = pure
  Nil         >>= f = Nil
  x `Cons` xs >>= f = f x `mplus` (xs >>= f)
  Delay xs    >>= f = Delay       (xs >>= f)

-- | Unlike (++), `mplus` swaps order, to interleave processes.
instance MonadPlus KList where
  mzero = Nil
  Nil           `mplus` xs = xs
  (x `Cons` xs) `mplus` ys = x `Cons` (ys `mplus` xs)
  Delay      xs `mplus` ys = Delay    (ys `mplus` xs)

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