{-# LANGUAGE RankNTypes, DataKinds, NoMonomorphismRestriction, BangPatterns #-}

module Examples where

import Types
import Data.Dynamic
import Control.Monad

import InterpreterMH hiding (main)
import Visual

type Outcome = Measure (Bool, Bool)
type Trust = Double
type Strategy = Trust -> Bool -> Measure Bool

allDefect :: Trust -> Bool -> Measure Bool
allDefect _ _ = conditioned $ bern 0.9

tit :: Trust -> Bool -> Measure Bool
tit me True  = conditioned $ bern 0.9
tit me False = conditioned $ bern me  

strat :: Measure Bool
strat = unconditioned $ categorical [(False, 0.5),
                                     (True, 0.5)]

play :: Strategy -> Strategy -> 
        (Bool, Bool) -> (Trust, Trust) -> Outcome
play strat_a strat_b (last_a,last_b) (a,b) = do
    a_action <- strat_a a last_b
    b_action <- strat_b b last_a
    return (a_action, b_action)

iterated_game :: Measure (Double, Double)
iterated_game = do
  let a_initial = False
  let b_initial = False
  a <- unconditioned $ uniform 0 1
  b <- unconditioned $ uniform 0 1
  rounds <- replicateM 10 $ return (a, b)
  foldM_ (play tit tit) (a_initial,b_initial) rounds
  return (a, b)


iterated_game2 :: Measure (Double, Double)
iterated_game2 = do
  let a_initial = False
  let b_initial = False
  a <- unconditioned $ uniform 0 1
  b <- unconditioned $ uniform 0 1
  na <- strat
  let (a_strat, as) = case na of
                        True  -> (tit, 0)
                        False -> (allDefect, 1)
  nb <- strat
  let (b_strat, bs) = case nb of
                        True  -> (tit, 0)
                        False -> (allDefect, 1)
  rounds <- replicateM 10 $ return (a, b)
  foldM_ (play a_strat b_strat) (a_initial,b_initial) rounds
  return (as, bs)
 
games = [Just (toDyn False), Just (toDyn False),
         Just (toDyn False), Just (toDyn True),
         Just (toDyn False), Just (toDyn False),
         Just (toDyn False), Just (toDyn True),
         Just (toDyn False), Just (toDyn True),
         Just (toDyn False), Just (toDyn False),
         Just (toDyn False), Just (toDyn True),
         Just (toDyn False), Just (toDyn True),
         Just (toDyn False), Just (toDyn True),
         Just (toDyn False), Just (toDyn False)]

run_game :: IO [(Double, Double)]
run_game = mcmc iterated_game games

viz_game :: IO ()
viz_game = do
  samples <- run_game
  viz 10000 ["a", "b"] $ map (\ (x,y) -> [x,y]) samples

main = viz_game