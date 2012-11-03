\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module Game where
import GameState
import Hops
import Board
import Control.Monad(mapM_)
import Maybe
\end{code}

\funidx{game}
\begin{code}
class Strategy s where
    getStrategy :: s -> GameState -> [GameState] -> (GameState -> IO ()) -> IO ()

type PureStrategy = GameState -> [GameState] -> GameState
instance Strategy PureStrategy where
    getStrategy strategy state states cont = cont $ strategy state states

type IOContinuationStrategy = GameState -> [GameState] -> (GameState -> IO ()) -> IO ()
instance Strategy IOContinuationStrategy where
    getStrategy strategy = strategy

class Watcher w where
    getWatcher :: w -> GameState -> [GameState] -> IO () -> IO ()

type SimpleIOWatcher = GameState -> [GameState] -> IO ()
instance Watcher SimpleIOWatcher where
    getWatcher watcher state states cont = watcher state states >> cont

type IOContinuationWatcher = GameState -> [GameState] -> IO () -> IO ()
instance Watcher IOContinuationWatcher where
    getWatcher watcher = watcher

game :: (Strategy a, Strategy b, Watcher c) => a -> b -> [c] -> (Side -> IO ()) -> IO ()
game black white watchers winCont = game' (getStrategy black) (getStrategy white) startState
    where game' currentStrategy otherStrategy gameState = do
            let succs = successors gameState
            -- Notify all watchers of the current gamestate, passing a
            -- continuation
            notify watchers gameState succs $ do
              case succs of
                [] -> winCont $ opposition $ turnNext gameState
                -- We pass a continuation into getStrategy, that's how
                -- the game will continue.
                otherwise -> currentStrategy gameState succs
                             (\ gameState' ->
                              -- Check for cheaters (is the new game state a
                              -- succesor to the old?)
                              if not (gameState' `elem` succs)
                                then error $ "Cheater! Cheater!"
                                else game' otherStrategy currentStrategy gameState')
          notify [] state states cont = cont
          notify (w:ws) state states cont = getWatcher w state states $ 
                                                       notify ws state states cont
\end{code}

A watcher to detect infinite loops will be useful when playing AI
strategies against each other. 
\funidx{infiniteLoopWatcher}
\begin{code}
infiniteLoopWatcher :: (GameState -> IO ()) -> IOContinuationWatcher
infiniteLoopWatcher callback state states cont 
    | isNothing $ prevState state                       = cont
    | isInfiniteLoop state (fromJust $ prevState state) = callback state
    | otherwise                                         = cont
    where isInfiniteLoop a b | (board a) == (board b) = True
          isInfiniteLoop a (GameState (Just prev) ((SingleHop _ _ _ _) : _) _  _) = 
              isInfiniteLoop a prev
          isInfiniteLoop _ _ = False
\end{code}
