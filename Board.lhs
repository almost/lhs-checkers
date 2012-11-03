\section{The Board Data Structure}
\label{module:Board}
\label{module:board}

The first thing that's needed is a data structure to represent a
Checkers board. This module will confine itself to representing the
board and locations on the board, it will not represent movement.

Using the exported functions it is possible to:

\begin{itemize}
\item Create a \verb!Board! containing the initial state of a checkers board
\item Enumerate all valid locations (represented by a \verb!Location! data type)
\item Get and set a given location's contents
\item Transform a location into another by way of a \verb!LocationDelta! representing one of the 4 possible (diagonal) directions.
\item Get the row and column coordinates of a \verb!Location!
\item Find out if a given location is on a king row
\end{itemize}

Conspicuously absent from this list is a way to construct a
\verb!Location! from row and column coordinates, this is intentional
and helps insure that an inconsistent board state can never be
constructed. Any function that could return an invalid \verb!Board! or
\verb!Location! will have a \verb!Maybe! return type and will return a
\verb!Nothing! in place of invalid data.

\subsection{Interface}

The module exports the following data types and functions.

\begin{code}
module Board (
              Board,
\end{code}
Data type representing a checkers board \myref{data:Board}
\begin{code}
              Location, 
\end{code}
Data type representing a location on a checkers board \myref{data:Location}
\begin{code}
              Side(Black, White),
\end{code}
Enumeration type indicating the side of checkers player \myref{data:Side}
\begin{code}
              CheckerType(King, Normal),
\end{code}
Enumeration type indicating the type of a checkers piece \myref{data:CheckerType}
\begin{code}
              Checker(Checker),  -- :: Side -> CheckerType -> Checker
\end{code}
A data type consisting of a \verb!Side! and \verb!CheckerType! used to
represent a checker piece \myref{data:Checker}
\begin{code}
              SquareContent,
\end{code}
An alias for \verb!Maybe Checker! used to represent the contents of a
given square on a \verb!Board! \myref{data:SquareContent}
\begin{code}
              row,               -- :: Location -> Int 
\end{code}
Retrieve the row coordinate from a \verb!Location! \myref{function:row}
\begin{code}
              col,               -- :: Location -> Int
\end{code}
Retrieve the col coordinate from a \verb!Location! \myref{function:col}
\begin{code}
              startBoard,        -- :: Board
\end{code}
A constant set to the starting board state for checkers
\myref{function:startBoard}
\begin{code}
              getSquare,         -- :: Location -> Board -> SquareContent
\end{code}
Gets the contents of the square on a \verb!Board! at a given
\verb!Location! \myref{function:getSquare}
\begin{code}
              setSquare,         -- :: Location -> SquareContent -> Board -> Board
\end{code}
Returns a copy of a \verb!Board! with contents of the square at a
given \verb!Location! set to the new value supplied
\myref{function:setSquare}
\begin{code}
              allLocations,      -- :: [Location]
\end{code}
A list of all valid \verb!Location!s. The list covers all possible
locations on a checkers board (all the black
squares) \myref{function:allLocations}
\begin{code}
              allSquares,        -- :: Board -> [(Location, SquareContent)]
\end{code}
A convenience function that returns all possible \verb!Location!s plus
the contents of those \verb!Location!s for a given board
\myref{function:allSquares}
\begin{code}
              allSquaresForSide, -- :: Board -> Side -> [(Location, SquareContent)]
\end{code}
As for \verb!allSquares! except only returns squares occupied by a
given side \myref{function:allSquares}
\begin{code}
              validDeltas,       -- :: Checker -> [LocationDelta]
\end{code}
Returns all the valid directions a given checker piece can move in---2
for a normal checker piece and 4 for a king \myref{function:validDeltas}
\begin{code}
              applyDelta,        -- :: LocationDelta -> Location -> Maybe Location
\end{code}
Apply a \verb!LocationDelta! to a \verb!Location! producing a
\verb!Just Location! if the result of adding would be a legal position
and \verb!Nothing! if it would not. \myref{function:applyDelta}
\begin{code}
              kingLocation,      -- :: Location -> Bool
\end{code}
A predicate indicating whether a given \verb!Location! is on a king
row or not \myref{function:kingLocation}
\begin{code}
              opposition,        -- :: Side -> Side
\end{code}
Converts \verb!Black! to \verb!White! and \verb!White! to \verb!Black!
\myref{function:opposition}
\begin{code}
              swapSides          -- :: Board -> Board
\end{code}
Swaps the sides on a \verb!Board! and rotate it $180^\circ$ \myref{function:swapSides}
\begin{code}
             ) where
import Data.Maybe (catMaybes, maybeToList)
\end{code}

\subsection{Code}
\subsubsection{Data Types}

A checker can be either black or white and is either a king or a
non-king (which I will call ``normal'') so I'll start by defining
enumerations to represent those two properties.

\label{data:Side}
\funidx{Side}
\funidx{White}
\funidx{Black}
\begin{code}
data Side = White | Black deriving (Eq, Show)
\end{code}
\label{data:CheckerType}
\funidx{CheckerType}
\funidx{King}
\funidx{Normal}
\begin{code}
data CheckerType = King | Normal deriving (Eq, Show)
\end{code}

I can then define the checker type in terms of those two enumerations.

\label{data:Checker}
\funidx{Checker}
\funidx{side}
\funidx{checkertype}
\begin{code}
data Checker = Checker {side        :: Side, 
                        checkertype :: CheckerType} deriving (Eq,Show)
\end{code}

A given square can be either empty, or, it can contain a checker. The
obvious way to do this is to use a Maybe type so that's what I do. A
type synonym is defined to make the type signatures clearer.

\label{data:SquareContent}
\funidx{SquareContent}
\begin{code}
type SquareContent = Maybe Checker
\end{code}

A type to represent the board itself can now be defined. If the white
squares are ignored this can simply be a 32 element list. The
implementation is hidden from other modules so it could be swapped out
for a more efficient one later on. 

\label{data:Board}
\funidx{Board}
\funidx{MkBoard}
\begin{code}
data Board = MkBoard [SquareContent]  deriving (Eq, Show)
\end{code}

A special data type is created to represent locations on the board,
this means that we can insure that an invalid location can never be
constructed. It also means that an efficient internal representation
could be used later on if it needed.

Locations are specified using full board coordinates (with the white
squares.)

\label{data:Location}
\funidx{Location}
\funidx{MkLoc}
\begin{code}
data Location = MkLoc Int Int deriving (Eq, Show)

\end{code}

A representation of movement will also be useful.

\label{data:LocationDelta}
\funidx{LocationDelta}
\funidx{MkLocDelta}
\begin{code}
data LocationDelta = MkLocDelta Int Int deriving (Eq, Show)
\end{code}

\subsubsection{Constructors}

A function is also required to create a starting board. I have chosen
to provide a function, \verb!startBoard!, that creates a board set up
for the beginning of a game of checkers, I choose this over a blank
board as I think it will be more convenient---it would be very rare to
want a blank starting board.

\label{function:startBoard}
\funidx{startBoard}
\begin{code}
startBoard :: Board
startBoard = MkBoard $ (replicate 12 $ Just $ Checker Black Normal) ++ 
                       (replicate 8 $ Nothing) ++ 
                       (replicate 12 $ Just $ Checker White Normal)
\end{code}


A \verb!makeLocation! function is provided to construct a location
given row and column coordinates. It returns a \verb!Just location! if
the coordinates are valid and \verb!Nothing! if not.

This function was originally part of the public interface to the
module but no longer is. I found that use of the
\verb!LocationDeltas!, allowed manipulation of locations in a safer
way.

\label{function:makeLocation}
\funidx{makeLocation}
\begin{code}
makeLocation :: Int -> Int -> Maybe Location
makeLocation r c 
    | r >= 0 && r < 8 && c >= 0 && c < 8 && (r `mod` 2) /= (c `mod` 2) = Just $ MkLoc r c
makeLocation _ _ = Nothing
\end{code}


\subsubsection{Accessors}

Using these location data types we can get and set locations on the
board. A internal helper function, \verb!getIdx!, is used to translate
a \verb!Location! into an index into the list holding the board
state. Since only valid locations can ever be constructed we don't
need to worry about the row or column being out of bounds.

\label{function:getIdx}
\funidx{getIdx}
\begin{code}
-- internal module function to get index for position
getIdx :: Location -> Int
getIdx (MkLoc row col) = row*4+(col `div` 2)  

\end{code}
\label{function:getSquare}
\funidx{getSquare}
\begin{code}
getSquare :: Location -> Board -> SquareContent
getSquare loc (MkBoard boardLst) =
    let i = getIdx loc in
    boardLst !! i

\end{code}
\label{function:setSquare}
\funidx{setSquare}
\begin{code}
setSquare :: Location -> SquareContent -> Board -> Board
setSquare loc content (MkBoard boardLst) = 
    let i = getIdx loc in
    MkBoard $ (take i boardLst) ++
              [content] ++ 
              (drop (i+1) boardLst)
\end{code}

\label{function:row}
\label{function:col}
\begin{code}
row, col :: Location -> Int
row (MkLoc r _) = r
col (MkLoc _ c) = c
\end{code}

\subsubsection{Location transformations}

Once we have a location we will want to transform it into other
locations. This will be done using \verb!LocationDelta! data. This
means that most parts of the program need not concern themselves
with actual board coordinates.

First we need a way to construct \verb!LocationDelta!s, this is fairly
easy as only a few will ever be needed---just those representing valid
moves on the checker board. This will be done using a function that
takes a \verb!Checker! and returns \verb!LocationDelta!s representing
all the moves it could make.

\label{function:validDeltas}
\funidx{validDeltas}
\begin{code}
validDeltas :: Checker -> [LocationDelta]
validDeltas (Checker _ King) = validDeltas (Checker White Normal) ++ 
                               validDeltas (Checker Black Normal)
validDeltas (Checker side Normal) = [MkLocDelta (dir side) (-1), 
                                     MkLocDelta (dir side) 1]
    where dir Black = 1
          dir White = -1
\end{code}

A function is then required to apply a delta to an existing location,
it's return type is \verb!Maybe Location! so that \verb!Nothing! can
be returned if applying the delta would result in invalid coordinates.

\label{function:applyDelta}
\funidx{applyDelta}
\begin{code}
applyDelta :: LocationDelta -> Location -> Maybe Location
applyDelta (MkLocDelta rd cd) (MkLoc r c) = makeLocation (r + rd) (c + cd)
\end{code}

\subsubsection{Board transformations}

A function to swap the sides on the board will be very useful for some
board evaluation functions. This means to turn it $180^\circ$ and
switch the colours. This means that a board evaluation function which
assumes it is evaluating for black can be used for white as well.

\label{function:swapSides}
\funidx{swapSides}
\begin{code}
swapSides :: Board -> Board
swapSides board = foldr (uncurry setSquare) board squares
    where squares = [(MkLoc (7-r) (7-c),Just (Checker (opposition s) t))|
                      (MkLoc r c,Just (Checker s t)) <- allSquares board]
\end{code}

\subsubsection{Board information}

A constant containing all valid locations on a checkers board will be
useful, this can be used in conjunction with \verb!getSquare! to get
all pieces on a board.

\label{function:allLocations}
\funidx{allLocations}
\begin{code}
allLocations :: [Location]
allLocations = catMaybes [makeLocation r c| r  <- [0..7], c <- [0..7]]
\end{code}

A couple of helper functions to return all squares on a board along
with their location and all squares containing pieces of a given side
will simplify other code later on.

\label{function:allSquares}
\funidx{allSquares}
\begin{code}
allSquares :: Board -> [(Location, SquareContent)]
allSquares board = [(loc, getSquare loc board) | loc <- allLocations]

\end{code}
\label{function:allSquaresForSide}
\funidx{allSquaresForSide}
\begin{code}
allSquaresForSide :: Board -> Side -> [(Location, SquareContent)]
allSquaresForSide board sid = 
    filter ((==[sid]) . map side .maybeToList . snd) (allSquares board)
\end{code}

A predicate function to check if a location is a ``king'' location
will also be useful. A ``king'' location is a square which moving into
will cause a normal checker to become a king, that is to say it is all
the locations on the top and bottom lines of the board.

\label{function:kingLocation}
\funidx{kingLocation}
\begin{code}
kingLocation :: Location -> Bool
kingLocation (MkLoc 0 _) = True
kingLocation (MkLoc 7 _) = True
kingLocation _ = False
\end{code}

We will also often want a function to flip a side, given \verb!White!
it would return \verb!Black! and vise versa.

\label{function:opposition}
\funidx{opposition}
\begin{code}
opposition :: Side -> Side
opposition White = Black
opposition Black = White
\end{code}

% \subsubsection{``Show'' instances}

% {\bf This section can safely be ignored.}

% It is very useful when debugging to be able to print board states and
% locations, the easiest way to enable this is to supply instances of
% \verb!Show!. 

% \begin{code}
% -- instance Show Side where
% --     show White = "W"
% --     show Black = "B"                 

% -- instance Show CheckerType where
% --     show Normal = "n"
% --     show King = "k"

% -- instance Show Location where
% --     show MkLoc row col = "(" ++ (show row) ++ "," ++ (show col) ++ ")"


% -- instance Show Checker where
% --     show (Checker side typ) = (show typ) ++ (show side)


% -- instance Show Board where
% --     show board = foldl ((++) . (++"\n")) "" $ map line [0..7]
% --         where line x = foldl (++) "" $ map (showSquare x) [0..7]
% --               showSquare x y
% --                   | validCoords x y = case getSquare (MkLoc x y) board bboaof
% --                                         Just x -> show x
% --                                         Nothing -> "  "
% --                   | otherwise = ".."

% \end{code}
