\section{Genetic Algorithm to Evolve an ANN}
\label{module:EvolveANN}

This module defines a very rough and ready Genetic Algorithm (GA) to
evolve a set of weights for the ANN defined in the ANNEvalFun module
\myref{module:ANNEvalFun}. It uses a tournament style GA, each round a
random set of 4 individuals is chosen in two pairs which are played
against each other, the two losers are eliminated winners are used as
parents for the new individuals that replace them.

Crossover (xover) is implemented by making each sub-tree of the weights
come from either a random one of the parents or a combination of the
two (recursively). This means that in some cases whole layers will
have weights from a single parent but in other cases the random
combination will go down the individual weight level. The chance of
crossover is currently set at $15\%$.

Newly created individuals are also mutated, there is a 1 in 300
chance of a given weight being replaced by a new random weight.

The \verb!evolve! function itself takes 2 numbers to indicate the
number of hidden and output neurons (a 3 layer ANN is assumed by this
module) and the population size to use. It never terminates but
instead prints out a winning set of weights after each round, the
output can be direct to a file for as long as required then the
function terminated manually. GHCi\footnote{A interactive Haskell
shell} is very useful for experimenting with this module.

\begin{code}

module EvolveANNN where
import ANNEvalFun
import Strategies
import GameState
import Hops
import Board
import System.Random(mkStdGen, randoms, randomRIO, randomIO)
import Control.Monad(liftM)
import Maybe(fromJust)


\end{code}
\funidx{evolve}
\begin{code}
evolve :: Int -> Int -> Int -> IO ()
evolve hiddenCount outputCount  popSize = 
    evolve' [(randomWeights [128, hiddenCount, outputCount] i)|i <- [1..popSize]]

\end{code}
\funidx{evolve'}
\begin{code}
evolve' :: [ANNWeights] -> IO ()
evolve' pop = do 
  let popSize = length pop
  i1 <- randomRIO (0,popSize-1)
  i2 <- randomRIO (0,popSize-1)
  let (winnerIdx, loserIdx) = pickWinner i1 i2
  let winner1 = pop !! winnerIdx
  let pop' = remove loserIdx pop
  i1 <- randomRIO (0,popSize-2)
  i2 <- randomRIO (0,popSize-2)
  let (winnerIdx, loserIdx) = pickWinner i1 i2
  let winner2 = pop' !! winnerIdx
  let pop'' = remove loserIdx pop'
  new1 <- breed winner1 winner2
  new2 <- breed winner2 winner1
  putStrLn $ "==========\n\n" ++ (show new1) ++ "\n\n"
  evolve' $  new1 : new2 : pop''
   where remove idx lst = (take idx lst) ++ (drop (idx+1) lst)
         -- if neither side wins then White is declared the winner,
         -- since both are picked randomly this pretty much has the
         -- effect of randomly picking a winner.
         pickWinner first second = 
             if Just Black == (game (makeStrategy (annEvalFun (pop !! first)) 0)
                                    (makeStrategy (annEvalFun (pop !! second)) 0))
                then (first,  second)
                else (second, first)

\end{code}
\funidx{game}
\begin{code}
game :: ([GameState] -> GameState) -> ([GameState] -> GameState) -> Maybe Side
game black white = game' black white startState
    where game' currentStrategy otherStrategy gameState =
            case successors gameState of
              [] -> Just $ opposition $ turnNext gameState -- we have a winner!
              succs -> let gameState' = currentStrategy succs in
                       if infiniteLoop gameState' (fromJust $ prevState gameState')
                         then Nothing
                         else game' otherStrategy currentStrategy (currentStrategy succs)
          infiniteLoop a b | (board a) == (board b) = True
          infiniteLoop a (GameState (Just prev) ((SingleHop _ _ _ _) : _) _  _) = 
              infiniteLoop a prev
          infiniteLoop _ _ = False


\end{code}
\funidx{breed}
\begin{code}
breed :: ANNWeights -> ANNWeights -> IO ANNWeights
breed parent1 parent2 = do do_xover <- randomRIO (0,100)
                           combined <- if do_xover < 15
                                         then xover parent1 parent2
                                         else return parent1
                           mutated <- mutate combined
                           return mutated

\end{code}
\funidx{mutate}
\begin{code}
mutate :: ANNWeights -> IO ANNWeights
mutate xs = mutate' (mutate' (mutate' mutateWeight)) xs
    where mutate' fn (x:xs) = do x' <- fn x
                                 xs' <- mutate' fn xs
                                 return $ x' : xs'
          mutate' fn [] = return []
          mutateWeight x = do r <- randomRIO (0,300)
                              if r == 1
                                 then liftM (\x -> x*2-1) randomIO
                                 else return x
\end{code}
\funidx{xover}
\begin{code}
xover :: ANNWeights -> ANNWeights -> IO ANNWeights
xover [] [] = return []
xover (x:xs) (y:ys) = do r <- randomRIO (0,100)
                         xy <- if r < 30 then
                                   (xover' x y)
                                 else return $ if r < 60 then x else y
                         xys <- xover xs ys
                         return $ xy : xys
    where xover' (x:xs) (y:ys) = do r <- randomRIO (0,100)
                                    let xy = if r < 50 then x else y
                                    xys <- xover' xs ys
                                    return $ xy : xys
          xover' [] [] = return []
\end{code}
