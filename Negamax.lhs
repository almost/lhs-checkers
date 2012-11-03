\section{Generic Negamax (Minimax) Algorithm with Alpha-Beta Pruning}
\label{module:Negamax}

In the interests of code reuse Negamax algorithm is separated from
the specifics of the game, all that is required is a heuristic
function, a successor function, the depth (\verb!ply!), and, the state
to be evaluated.

\begin{code}
module Negamax(negamax) where
\end{code}

Negamax can be seen as just a right handed fold over the list of
maximum-so-far values of successive negated scores of the
successors. The case statement first checks if the \verb!successors!
list is empty---indicating the end of the game---and, if it is,
returns the heuristic value of the state (which in the circumstances
would most probably be not so heuristic). If there are in fact
successors a (lazy) list of is generated which each element each
element is the maximum of the negated scores of the successors up to
that point. This list of partial maximums is then combined by
\verb!foldr! which uses the \verb!combine! to force the list up to a
value that is greater than $\beta$. Because of the lazy semantics of
Haskell the successors are only evaluated as far as they need to
be---no further.

\funidx{negamax'}
\label{function:negamax'}
\begin{code}
negamax' :: (a -> Int) -> (a -> [a]) -> Int -> Int -> Int -> a -> Int
negamax' heuristic _ 0 _ _ state =
    heuristic state
negamax' heuristic successorFn ply __alpha__ __beta__ state = 
    case successors of 
      [] -> heuristic state
      otherwise -> foldr combine __alpha__ scores
    where successors = successorFn state
          combine __alpha__ maxScoreSoFar = 
              if __alpha__ >= __beta__ 
                then __beta__
                else max __alpha__ maxScoreSoFar
          -- define a (lazy) list of the negations of the algorithm
          -- as applied to all successor states.
          scores = map (uncurry getScore) $ zip successors (scanl max __alpha__ scores)
          getScore succ __alpha__ = negate $ negamax' heuristic successorFn (ply-1) (-__beta__) (-__alpha__) succ

\end{code}

All that's left is to define the \verb!negamax! function itself, this
just passes its arguments in to \verb!negamax!$^\prime$ along with starting
$\alpha$ and $\beta$ cutoffs---maximum and minimum values that an Int
(Haskell's fixed size integer type) can represent.

\funidx{negamax}
\label{function:negamax}
\begin{code}

negamax :: (a -> Int) -> (a -> [a]) -> Int -> a -> Int
negamax heuristic successorFn ply state = negamax' heuristic successorFn ply (minBound+1) (maxBound) state
\end{code}