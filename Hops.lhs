\section{The Hop Data Structure} 
\label{module:Hops}

Building on the \verb!Board! module \myref{module:board} we next want
to represent what I call ``hops''. A hop is a a single move of a
checker piece from one board location to another---possibly capturing
another piece along the way. A hop is not always a complete move,
several capture hops may be strung together to form one move.

\subsection{Interface}

\begin{code}
module Hops(
            Hop(SingleHop, 
                CaptureHop, 
                source, 
                capture, 
                destination, 
                newBoard),
\end{code}
Data type representing a hop, the constructors---\verb!SingleHop! and
\verb!CaptureHop!--are a public part of the module interface to allow
easy pattern matching \myref{data:Hop}
\begin{code}
            allowedHops,   -- :: Board -> Side -> [Hop]
\end{code}
Returns all legal \verb!Hop!s for a given \verb!Side! on a supplied
\verb!Board! \myref{function:allowedHops}
\begin{code}
            continuingHops -- :: Hop -> [Hop]
\end{code}
Returns all legal hops continuing hops from a given \verb!Hop!. Always
returns the empty list for a non-capture hop \myref{function:continuingHops}
\begin{code}
           ) where
import Board
import Data.Maybe (maybeToList, isNothing)
import Control.Monad (guard)
\end{code}

\subsection{Code}
\subsubsection{Data Types}

A non-capturing hop is represented by a source location, a destination
location, and, a new board state. A capturing hop is the same as a
non-capturing hop except that it has a field for the captured piece as
well.

\label{data:Hop}
\funidx{Hop}
\funidx{SingleHop}
\funidx{kingHop}
\funidx{source}
\funidx{destination}
\funidx{newBoard}
\funidx{CaptureHop}
\funidx{destination}
\begin{code}
data Hop = SingleHop {kingHop      :: Bool,
                      source       :: Location, 
                      destination  :: Location,
                      newBoard     :: Board}
         | 
           CaptureHop {kingHop     :: Bool,
                       source      :: Location, 
                       capture     :: Location,
                       destination :: Location,
                       newBoard    :: Board} deriving (Eq, Show)
\end{code}

\subsubsection{Interface functions}

I'm going to start by defining the two functions which form the interface
to this module, followed by the helper functions they in turn rely on.

The first of these two is the \verb!allowedHops! function, its job is
to return a list of valid hops given a current board state and the
side whose turn it is. It is very simply defined in terms of two
functions---\verb!captureHops! \myref{function:captureHops} and
\verb!singleHops! \myref{function:singleHops}---which will be defined
later on. It neatly expresses the rule that the legal hops are any
capture hops unless there are no legal capture hops, in which case the
legal hops are all legal single hops---that is to say that capture
hops are forced.

\label{function:allowedHops}
\funidx{allowedHops}
\begin{code}
allowedHops :: Board -> Side -> [Hop]
allowedHops board side = case captureHops board side of
                           [] -> singleHops board side
                           x  -> x
\end{code}

As a hop is only a partial turn a function is needed to find the
allowed continuations of a hop. It makes use of
\verb!captureHopsForLocation! \myref{function:captureHopsForLocation}
which is similar to \verb!captureHops!---in fact the latter is
implemented in terms of the former as we will see below---except that it
only returns capture hops starting from a given square.

The rule that if a checker becomes a king it must stop its turn
immediately is also representing here (by pattern matching \verb!False!
on the \verb!kingHop! field of the previous \verb!Hop!).

\label{function:continuingHops}
\funidx{continuingHops}
\begin{code}
continuingHops :: Hop -> [Hop]
continuingHops (CaptureHop False _ _ dest board) = captureHopsForLocation board dest
continuingHops _ = []
\end{code}

\subsubsection{A Quick Detour: The List Monad and ``do'' Notation}
\label{listmonad}

The functions below make use of the \verb!List Monad!\footnote{See
\url{http://www.haskell.org/all_about_monads/html/listmonad.html} for
more information. See also
\url{http://lukeplant.me.uk/blog.php?id=1107301643} for an easier
introduction if you're already familiar with Python's List
Comprehensions.} to find all possible hops. ``do'' notation is used to
keep the code pretty. Monads are really not as scary as they sound and
the \verb!List Monad! is certainly the least scary of the bunch. A
quick example of the \verb!List Monad! and ``do'' notation might help:

\begin{code}
example :: [Int] -> [Int] -> [Int]
example as bs = do a <- as
                   b <- bs
                   guard $ a /= b
                   return $ a+b
\end{code}

Is equivalent to the following (untested) Java code (ignoring the
issues of lazy vs. strict evaluation):

\begin{verbatim}
int[] example(int[] as, int[] bs) {
    ArrayList<int> = new ArrayList<int>();
    for(int i = 0; i < as.length; i++) {
        for(int j = 0; j < bs.length; j++) {
          if (!(as[i] != bs[j])) {          
              continue;
          }
          retvals.add(as[i] + bs[j]);
        }
    }  
    return (int[]) retvals.toArray(new int[retvals.size()]);
}

\end{verbatim}

As you can see, this makes for a powerful abstraction in certain situations.

\subsubsection{Helper Functions}

The first helper function is \verb!singleHops!, this function returns
all legal single hops for a given \verb!Board! and \verb!Side!. It does
this by first taking all squares containing pieces of the correct
colour.  Next it applies all valid deltas for the given piece to its
location---deltas that would move the piece of the board are ignored
since \verb!applyDelta! would return \verb!Nothing! which
\verb!maybeToList!  converts to the empty list. All destinations that
would move a piece onto another piece are discarded by the guard and
the remaining ones used to construct \verb!Hop!s using the
\verb!SingleHop! constructor.

\label{function:singleHops}
\funidx{singleHops}
\begin{code}
singleHops :: Board -> Side -> [Hop]
singleHops board side = do 
  (source, Just checker@(Checker _ checkertype)) <- allSquaresForSide board side
  delta <- validDeltas checker
  dest  <- maybeToList $ applyDelta delta source
  -- Make sure the hop is valid for the board
  guard $ isNothing $ getSquare dest board
  let newBoard = setSquare source Nothing . 
                 setSquare dest   (Just $ maybeBecomeKing checker dest) $ board
  return $ SingleHop (kingLocation dest && checkertype /= King) source dest newBoard
\end{code}

The \verb!captureHops! function returns all valid capturing hops, it
does this by calling the \verb!captureHopsForLocation! function for
each piece of the correct colour on the board.

\label{function:captureHops}
\funidx{captureHops}
\begin{code}

captureHops :: Board -> Side -> [Hop]
captureHops board side = do 
  (source, _) <- allSquaresForSide board side
  captureHopsForLocation board source

\end{code}

\verb!captureHopsForLocation! works in a similar way to
\verb!singleHops!, using the \verb!List Monad! to generate then filter
all possible moves---except this time it's only considering a single
starting \verb!Location!.

\label{function:captureHopsForLocation}
\funidx{captureHopsForLocation}
\begin{code}
captureHopsForLocation :: Board -> Location -> [Hop]
captureHopsForLocation board source = do
  let Just checker@(Checker side checkertype) = getSquare source board
  delta <- validDeltas checker
  cap   <- maybeToList $ applyDelta delta source
  dest  <- maybeToList $ applyDelta delta cap
  -- Make sure the hop is valid for the board
  guard $ isNothing $ getSquare dest board
  Checker capside _  <- maybeToList $ getSquare cap board
  guard $ capside /= side
  let newBoard = setSquare source Nothing . 
                 setSquare cap    Nothing . 
                 setSquare dest   (Just $ maybeBecomeKing checker dest) $ board
  return $ CaptureHop (kingLocation dest && checkertype /= King) source cap dest newBoard

\end{code}

The \verb!maybeBecomeKing! function was used above to get the checker piece
to place in the new location. This will usually be the same as the
checker from the source location except in the case of a normal
checker moving into a king square, in this case the checker will
become a king.

\label{function:maybeBecomeKing}
\funidx{maybeBecomeKing}
\begin{code}
maybeBecomeKing :: Checker -> Location -> Checker
maybeBecomeKing (Checker side _) dst | kingLocation dst = Checker side King
maybeBecomeKing checker _ = checker
\end{code}

