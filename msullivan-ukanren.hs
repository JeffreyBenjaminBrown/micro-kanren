-- Based on Hennan and Friedman's original microKanren paper.
-- Had a little accompanying text in a blog:
  -- https://www.msully.net/blog/2015/02/26/microkanren-%CE%BCkanren-in-haskell/

{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Control.Monad


type Var = Integer
type Subst = [(Var, Term)]
type State = (Subst, Var)
type Program = State -> KList State

data Term = Atom String | Pair Term Term | Var Var deriving Show

-- Unlike the original paper, in Haskell we don't need to eta-expand.
-- We just need to add some sort of marker.
data KList a = Nil | Cons a (KList a) | Delay (KList a)
  deriving (Show, Eq, Ord, Functor)

instance Applicative KList where
  pure a = Cons a Nil
  (<*>) _ _ = error "<*> undefined for KList."
    -- TODO (#grok) Should `<*>` mean something for `KList`s?
-- <<< types >>>
instance Alternative KList where
  empty = Nil
  (<|>) = mplus -- TODO (#grok) ? This is just a guess.

instance Monad KList where
  return = pure
  Nil >>= f = Nil
  x `Cons` xs >>= f = f x `mplus` (xs >>= f)
  Delay xs >>= f = Delay (xs >>= f)

instance MonadPlus KList where
  mzero = Nil
  Nil `mplus` xs = xs
  (x `Cons` xs) `mplus` ys = x `Cons` (ys `mplus` xs) -- swapped per sect. 6
  Delay xs `mplus` ys = Delay $ ys `mplus` xs

-- Apply a substitution to the top level of a term
-- See `unify` for why we do not need to address the case of a Pair input.
walk :: Term -> Subst -> Term
walk (Var v) s = case lookup v s of Nothing -> Var v
                                    Just us -> walk us s
walk t s = t

extS :: Var -> Term -> Subst -> Subst
extS v t = (:) (v, t)

-- Try to unify two terms under a Subst;
-- return an extended Subst if it succeeds.
unify :: Term -> Term -> Subst -> Maybe Subst
unify u v s = go (walk u s) (walk v s)
  where go (Var v1) (Var v2) | v1 == v2 = return s
        go (Var v) t = return $ extS v t s
        go t (Var v) = return $ extS v t s
        go (Pair u1 u2) (Pair v1 v2) =
          do s' <- unify u1 v1 s
             unify u2 v2 s'
        go (Atom a1) (Atom a2) | a1 == a2 = return s
        go _ _  = mzero


-- | = MicroKanren program formers
zzz :: Program -> Program
zzz g = \sc -> Delay $ g sc

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
klistToList Nil = []
klistToList (Delay xs) = klistToList xs
klistToList (x `Cons` xs) = x : klistToList xs


-- | = Test cases
empty_state :: State
empty_state = ([], 0)

five :: Program
five = callFresh $ \x -> equiv x $ Atom "5"

fives_ :: Term -> Program
fives_ x = disj (equiv x $ Atom "5") (zzz $ fives_ x)

fives :: Program
fives = callFresh fives_

fivesRev_ :: Term -> Program
fivesRev_ x = disj (zzz $ fivesRev_ x) (equiv x $ Atom "5")

fivesRev :: Program
fivesRev = callFresh fivesRev_

a_and_b :: Program
a_and_b = conj
          (callFresh $ \a -> equiv a (Atom "7"))
          (callFresh $ \b -> disj (equiv b $ Atom "5") (equiv b $ Atom "6"))

runTest :: (State -> KList a) -> [a]
runTest p = klistToList $ p empty_state
