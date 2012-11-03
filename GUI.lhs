\section{GUI}
\label{module:GUI}

The GUI is implemented in GTK\footnote{\url{http://www.gtk.org/}}
using the the gtk2hs\footnote{\url{http://www.haskell.org/gtk2hs/}}
binding for Haskell. This is a very low level library requiring a lot
of imperative style code (in the IO~Monad) and in hindsight I think it
might have been better to have used a higher level more functional
library.

TODO explain glade
TODO explain game state thingies

\begin{code}
module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Monad
import SimpleGUI(messageBox)

import Board
import GameState
import Hops
import Strategies

\end{code}

In order to update the display handles for the various display
elements are needed. A data type is used to pass around all the
handles as a block rather than try and pass the correct handles to
each place that needs them.

\funidx{GUI}
\funidx{guiWindow}
\funidx{guiBoard}
\funidx{guiPlayerBlack}
\funidx{guiPlayerWhite}
\label{data:GUI}
\begin{code}
data GUI = GUI {guiWindow      :: Window, 
                guiBoard       :: Table, 
                guiPlayerBlack :: CheckButton,
                guiPlayerWhite :: CheckButton}

\end{code}

The main loop of the program first calls the gtk2hs \verb!initGUI!
function, sets up the widgets, requests the first player move (using
the \verb!doTurn! \myref{function:doTurn} function), and, enters the
GTK main event loop.

\funidx{main}
\label{function:main}
\begin{code}
main :: IO ()
main = do initGUI
          gui <- setupWidgets
          doTurn gui startState
          mainGUI
\end{code}

The \verb!setupWidgets! function gets references to the widgets which
are defined in the \verb!.glade! XML file and sets up stuff which
isn't (such as the board grid). It returns a \verb!GUI! data structure
\myref{data:GUI} containing all the references which will be needed by
the rest of the program.

\funidx{setupWidgets}
\label{function:setupWidgets}
\begin{code}
setupWidgets :: IO GUI
setupWidgets = do Just xml   <- xmlNew "checkers.glade"
                  window     <- xmlGetWidget xml castToWindow "gameWindow"
                  container  <- xmlGetWidget xml castToAlignment "container"
                  [playerBlack, playerWhite] <- mapM (xmlGetWidget xml castToCheckButton) 
                                                ["playerBlack", "playerWhite"]
                  -- Now recreate the board, twice as many cells as
                  -- there are squares on the board because we want to
                  -- divide squares into 4 for the arrows
                  table <- tableNew 16 16 True
                  containerAdd container table
                  onDestroy window mainQuit
                  widgetShowAll window
                  return $ GUI window table playerBlack playerWhite
\end{code}

The \verb!doTurn! function checks the current status of the player
type check boxes to determine if AI player should be used or if the
human should be asked. This means the type of player can be changed
mid-game. It also is responsible for checking if a winner has been
found and will display a message box if one has.

\funidx{doTurn}
\label{function:doTurn}
\begin{code}
doTurn :: GUI -> GameState -> IO ()
doTurn gui state = 
    case winner state of
      Just w -> do let w' = show w
                   playerTurn gui state -- Causes the board display to
                                        -- be updated
                   messageBox (w' ++ " wins!") $ 
                              "We have a winner!\n" ++
                              "Congratulations to " ++ w' ++ "!"
                   -- reset the board
                   doTurn gui startState
      Nothing -> do human <- toggleButtonGetActive toggle
                    case human of
                      True  -> playerTurn gui state
                      False -> aiTurn gui state
    where toggle = case turnNext state of 
                     White -> guiPlayerWhite gui
                     Black -> guiPlayerBlack gui

\end{code}

The \verb!aiTurn! function first hands calculates the AI move using
\verb!defaultStrategy! \myref{function:defaultStrategy}. Once it has
the move it intends to make it calls \verb!showTurn! which will show
each hop of the move with a small gap in between---this will allow the
user to see the move being made. Because \verb!timeoutAdd! is used the
function will return after the first hop is shown but will be
scheduled to run again by the main event loop.

\funidx{aiTurn}
\label{function:aiTurn}
\begin{code}
aiTurn :: GUI -> GameState -> IO ()
aiTurn gui state = let state'@(GameState _ hops _ _) = (defaultStrategy $ successors state) in
                   showTurn state' [] hops
        where showTurn state hops (h:hs) = 
                  do clearBoard gui
                     drawHops gui (h:hops)
                     drawBoard gui (newBoard h)
                     widgetShowAll $ guiWindow gui
                     timeoutAdd (showTurn state (h:hops) hs >> return False) 300
                     return ()
              showTurn state _ [] = doTurn gui state

\end{code}

The \verb!clearButton! function is used by both \verb!playerTurn! and
\verb!aiTurn! to clear the board of all widgets before recreating it
using the new game state.

\funidx{clearBoard}
\label{function:clearBoard}
\begin{code}
clearBoard :: GUI -> IO ()
clearBoard gui = do pieces <- containerGetChildren (guiBoard gui)
                    mapM_ (containerRemove (guiBoard gui)) pieces

\end{code}

\verb!drawHops! will draw an indication of the last move made. This
enables the player to see where the AI moved last turn.

\funidx{drawHops}
\label{function:drawHops}
\begin{code}
drawHops :: GUI -> [Hop] -> IO ()
drawHops gui hops = mapM_ drawHop hops
    where drawHop (SingleHop _ _ dest _)  = overlay dest "images/marker.png"
          drawHop (CaptureHop _ _ cap dest _)  = do overlay dest "images/marker.png"
                                                    overlay cap "images/capture.png"
          overlay loc file = do image <- imageNewFromFile file
                                tableAttach (guiBoard gui)
                                            image
                                            (col loc * 2)
                                            (col loc * 2 + 2)
                                            (row loc * 2)
                                            (row loc * 2 + 2)
                                            [Shrink] [Shrink] 0 0
                                return ()
\end{code}

\verb!drawBoard! draws the actual squares onto the checker board. It
simply calls \verb!drawSquare! for each square on the board, this
loads the correct image (given by \verb!squareImage!) and attaches it
into the board table at its location.

\funidx{drawBoard}
\label{function:drawBoard}
\begin{code}
drawBoard :: GUI -> Board -> IO ()
drawBoard gui board = mapM_ (uncurry drawSquare) (allSquares board)
  where drawSquare loc square = do
          image <- imageNewFromFile (squareImage square)
          tableAttach (guiBoard gui)
                      image 
                      (col loc * 2)
                      (col loc * 2 + 2)
                      (row loc * 2)
                      (row loc * 2 + 2)
                      [Shrink] [Shrink] 0 0
        squareImage Nothing                       = "images/empty.png"
        squareImage (Just (Checker White Normal)) = "images/white.png"
        squareImage (Just (Checker White King))   = "images/white_king.png"
        squareImage (Just (Checker Black Normal)) = "images/black.png"
        squareImage (Just (Checker Black King))   = "images/black_king.png"
\end{code}


\verb!playTurn! is responsible for drawing the current board state to
the window and allowing the user to select their next move. The
\verb!drawChoices! function \myref{function:drawChoices} will draw the
arrow buttons for each checker and direction that can be moved. Once
the user has selected a hop the callback \verb!doHop'! will be called
which will check if the move is complete and either move to the next
turn or loop round and ask for the next hop.

\funidx{playerTurn}
\label{function:playerTurn}
\begin{code}
playerTurn :: GUI -> GameState -> IO ()
playerTurn gui state@(GameState prevState hops board0 side) = 
    doHop board0 $ allowedHops board0 side
    where succs = successors state
          doHop board choices = do 
            clearBoard gui
            drawChoices gui choices (doHop' choices)
            drawHops gui hops
            drawBoard gui board
            widgetShowAll $ guiWindow gui
          doHop' choices hop = let b = (newBoard hop) in
                               case filter ((==b) . board) succs of
                                 (s : _) -> doTurn gui s
                                 otherwise  ->  doHop b $ continuingHops hop

\end{code}

\verb!drawChoices! is the method by which \verb!playerTurn! prompts
the user for his/her next move. It displays up to 4 buttons on the
corners of each check that can be moved, the click handler for each is
set to a closure which will call the callback function passing on the
choice made.

\funidx{drawChoices}
\label{function:drawChoices}
\begin{code}
drawChoices :: GUI -> [Hop] -> (Hop -> IO ()) -> IO ()
drawChoices gui choices callback = mapM_ showChoice choices
    where showChoice hop = do
            let (src,dst) = (source hop, destination hop)
            let tablerow = (row src * 2) + if row dst > row src then 1 else 0
            let tablecol = (col src * 2) + if col dst > col src then 1 else 0
            image <- imageNewFromFile (if row dst > row src 
                                         then if col dst > col src
                                                then "images/arrowdr.png" 
                                                else "images/arrowdl.png"
                                          else if col dst > col src
                                                then "images/arrowur.png" 
                                                else "images/arrowul.png")
            button <- buttonNew
            onButtonPress button $ (\_ -> callback hop >> return True)
            buttonSetImage button image
            buttonSetRelief button ReliefNone
            tableAttach (guiBoard gui)
                        button
                        tablecol
                        (tablecol + 1)
                        tablerow
                        (tablerow + 1)
                        [Fill] [Fill] 0 0

\end{code}

