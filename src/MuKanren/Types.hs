module MuKanren.Types where

import Control.Applicative
import Control.Monad


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
