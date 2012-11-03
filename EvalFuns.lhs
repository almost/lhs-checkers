\section{Static Evaluation Functions} 
\label{module:EvalFuns}
A static evaluation function---a heuristic function that is---will be
needed. It can be used to make game play decisions on its own but will
be more useful later on when combined with the Minimax (or, in this
case, Negamax) algorithm \myref{module:Negamax}.

\begin{code}
module EvalFuns where
import Board
import GameState
import Negamax
\end{code}

A static evaluation function will be defined as a function from a
\verb!GameState! \myref{data:GameState} to an \verb!Int!. This will
allow different evaluation functions to be weighted and combined
together. The static evaluation function should always return the
``goodness'' of the move for the current player.

\funidx{EvalFun}
\begin{code}
type EvalFun = GameState -> Int
\end{code}

A function is defined to aid the combination of evaluation functions,
it takes a list of \verb!(EvalFun, Int)! tuples and returns an
evaluation function which is a linear combination of them.

\funidx{combineEvalFuns}
\begin{code}
combineEvalFuns :: [(EvalFun, Int)] -> EvalFun
combineEvalFuns [] _ = 0
combineEvalFuns ((fn,w):rest) state = ((fn state) * w) + combineEvalFuns rest state
\end{code}

A default evaluation function is defined as a combination of the
component evaluation functions which are given below..

\funidx{defaultEval}
\label{function:defaultEval}
\begin{code}
defaultEval = combineEvalFuns [(countCheckers    , 10000), 
                               (countKings       , 10000),
                               (closestToKinging , 10),
                               (avgToKinging     , 1),
                               (kingsMoveAway    , 100)]
\end{code}
\funidx{countCheckers}
\funidx{countKings}
\funidx{closestToKinging}
\funidx{rowsForBlack}
\funidx{kingsMoveAway}
\begin{code}
countCheckers :: EvalFun
countCheckers (GameState _ _ board turnNext) = checkers (opposition turnNext) - checkers turnNext
    where checkers side = length $ allSquaresForSide board side

countKings :: EvalFun
countKings (GameState _ _ board turnNext) = kings (opposition turnNext) - kings turnNext
    where kings side = length $ [()|(_, Just (Checker _ King)) <- allSquaresForSide board side]

closestToKinging  :: EvalFun
closestToKinging gamestate = 
    case gamestate of
      (GameState _ _ board Black) -> aux (swapSides board)
      (GameState _ _ board White) -> aux board
    where aux board = foldl max 0 (rowsForBlack board)

avgToKinging  :: EvalFun
avgToKinging gamestate = 
    case gamestate of
      (GameState _ _ board Black) -> aux (swapSides board)
      (GameState _ _ board White) -> aux board
    where aux board = if (length (rowsForBlack board)) == 0 
                        then 0 
                        else (sum (rowsForBlack board)) `div` (length (rowsForBlack board))

-- Helper function
rowsForBlack :: Board -> [Int]
rowsForBlack board = [(row loc)|(loc, Just (Checker _ Normal)) <- allSquaresForSide board Black]

kingsMoveAway :: EvalFun
kingsMoveAway gamestate = 
    case gamestate of
      (GameState _ _ board Black) -> aux board
      (GameState _ _ board White) -> aux (swapSides board)
    where aux board = if (length (kingRows board)) == 0 
                        then 0 
                        else (sum (kingRows board)) `div` (length (kingRows board))
          kingRows board = [(row loc)|
                            (loc, Just (Checker _ King)) <- allSquaresForSide board White]

\end{code}

