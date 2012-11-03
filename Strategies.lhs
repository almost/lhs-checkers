\section{AI Strategy}
\label{module:Strategies}

\begin{code}
module Strategies where
import GameState 
import EvalFuns(defaultEval, EvalFun)
import Negamax
import Data.List (sortBy)
import Data.Ord (comparing)
\end{code}


Once an evaluation function has been built it needs to be combined
with Negamax \myref{module:Negamax} to create a strategy. The
\verb!makeStrategy! function will do this, it takes a static
evaluation function and a depth to search and returns a function that
will choose the best choice from a list of \verb!GameState!s.

\funidx{makeStrategy}
\begin{code}
makeStrategy :: EvalFun -> Int -> [GameState] -> GameState
makeStrategy eval ply = (\choices -> last $ sortBy compareFn choices)
    where compareFn = comparing $ negamax eval successors ply
\end{code}

Finally we can create a strategy!

\funidx{defaultStrategy}
\label{function:defaultStrategy}
\begin{code}
defaultStrategy :: [GameState] -> GameState
defaultStrategy = makeStrategy defaultEval 6
\end{code}