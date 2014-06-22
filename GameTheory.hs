{-# LANGUAGE RankNTypes, DataKinds, NoMonomorphismRestriction,
  DeriveDataTypeable, BangPatterns #-}

module Examples where

import Types
import Data.Dynamic
import Control.Monad

import InterpreterMH hiding (main)
import Visual

type Outcome = Measure (Bool, Bool)
type Trust = Double
type Strategy = Trust -> Bool -> Bool -> Measure Bool

allCooperate :: Trust -> Bool -> Bool -> Measure Bool
allCooperate _ _ _ = conditioned $ bern 0.1

allDefect :: Trust -> Bool -> Bool -> Measure Bool
allDefect _ _ _ = conditioned $ bern 0.9

grimTrigger :: Trust -> Bool -> Bool -> Measure Bool
grimTrigger me True False   = conditioned $ bern 0.9
grimTrigger me False False  = conditioned $ bern 0.1
grimTrigger me _ True       = conditioned $ bern 0.9

tit :: Trust -> Bool -> Bool -> Measure Bool
tit me True _  = conditioned $ bern 0.9
tit me False _ = conditioned $ bern me  

data SChoice = Tit | GrimTrigger | AllDefect | AllCooperate deriving (Eq, Enum, Typeable)

chooseStrategy :: Int -> Strategy
chooseStrategy 0 = tit
chooseStrategy 1 = allDefect
chooseStrategy 2 = allCooperate
chooseStrategy 3 = grimTrigger

strat :: Measure Int
strat = unconditioned $ categorical [(fromEnum AllCooperate, 0.25),
                                     (fromEnum AllDefect, 0.25),
                                     (fromEnum GrimTrigger, 0.25),
                                     (fromEnum Tit, 0.25)]

play :: Strategy -> Strategy -> 
        (Bool, Bool) -> (Trust, Trust) -> Outcome
play strat_a strat_b (last_a,last_b) (a,b) = do
    a_action <- strat_a a last_b last_a
    b_action <- strat_b b last_a last_b
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


iterated_game2 :: Measure (Int, Int)
iterated_game2 = do
  let a_initial = False
  let b_initial = False
  a <- unconditioned $ uniform 0 1
  b <- unconditioned $ uniform 0 1
  na <- strat
  let as = fromEnum na 
  let a_strat = chooseStrategy na
  nb <- strat
  let bs = fromEnum nb 
  let b_strat = chooseStrategy nb
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

run_game :: IO [(Int, Int)]
run_game = mcmc iterated_game2 games

viz_game :: IO ()
viz_game = do
  samples <- run_game
  viz 10000 ["a", "b"] $ map (\ (x,y) -> [x,y]) samples

main = viz_game
