module MuKanren.Test where

import MuKanren.Main


empty_state :: State -- ^ The first variable used is always 0.
empty_state = ([], 0)

six :: Program
six = callFresh $ \x -> equiv x $ Atom "6"

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
          (callFresh $ \a -> equiv a $ Atom "7")
          (callFresh $ \b -> disj (equiv b $ Atom "5") (equiv b $ Atom "6"))

runTest :: Program -> [State]
runTest p = klistToList $ p empty_state
