\section{The GameState Data Structure}
\label{module:GameState}

Building on the data types and associated helper functions in the
\verb!Board! and \verb!Hops! this module defines a data structure to
encode the current game state and a function to generate successor
states. It also defines the starting state as a
constant---\verb!startState! \myref{function:startState}.

\subsection{Interface}
\begin{code}
module GameState(GameState(GameState,
                           prevState,
                           hops,
                           board,
                           turnNext),
\end{code}
A data structure to hold current game state \myref{data:GameState}
\begin{code}
                 startState, -- :: GameState
\end{code}
A constant holding the starting game state \myref{function:startState}
\begin{code}
                 successors, -- :: GameState -> [GameState]
\end{code}
The successor function for \verb!GameState!s, returns a list of
possible successor states \myref{function:successors}
\begin{code}
                 winner      -- :: GameState -> Maybe Side
\end{code}
A predicate which indicates whether a side has won in the given
\verb!GameState!, returns \verb!Nothing! if neither side has won
\myref{function:winner}
\begin{code}
                ) where
import Board
import Hops
\end{code}

\subsection{Code}
\subsubsection{Data Type}

The \verb!GameState!  consists of the current board state and the
player whose turn it is next, and, the sequence of hops taken on the
previous turn (which will equal the empty list for the starting
state). The previous state is also stored, a \verb!Maybe! type is used
for this to handle the case of the beginning of game (when there is no
previous state).

\funidx{GameState}
\funidx{prevState}
\funidx{board}
\funidx{turnNext}
\label{data:GameState}
\begin{code}
data GameState = GameState {prevState  :: Maybe GameState,
                            hops       :: [Hop],
                            board      :: Board,
                            turnNext   :: Side} deriving (Show, Eq)
\end{code}

\subsubsection{Functions}

A constant is defined to give the state at the beginning of a new game.

\label{function:startState}
\funidx{startState}
\begin{code}
startState :: GameState
startState = GameState Nothing [] startBoard Black
\end{code}

The \verb!successors! function takes a \verb!GameState! and returns a
list of all successor \verb!GameState!s. Once again the \verb!List Monad! 
is used as explained previously \myref{listmonad}.

\label{function:successors}
\funidx{successors}
\begin{code}
successors :: GameState -> [GameState]
successors gameState@(GameState _ _ board turnNext) = 
    do firstHop <- allowedHops board turnNext
       hops     <- continuations firstHop
       let turnNext' = opposition turnNext
       return $ GameState (Just gameState) hops (newBoard . last $ hops) turnNext'
    where continuations lastHop = 
              case continuingHops lastHop of
                []       -> [[lastHop]]
                nextHops -> do nextHop <- nextHops
                               rest    <- continuations nextHop
                               return  $ lastHop:nextHop:rest
\end{code}

The \verb!winner! function will return \verb!Just side! value
indicating which which side has won the game, or \verb!Nothing! if
neither side has.

\label{function:winner}
\funidx{winner}
\begin{code}
winner :: GameState -> Maybe Side
winner gameState@(GameState _ _ _ turnNext) = 
    case successors gameState of
      []        -> Just $ opposition turnNext
      otherwise -> Nothing
\end{code}
