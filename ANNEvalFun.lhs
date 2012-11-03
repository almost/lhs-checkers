\section{Artificial Neural Network as an Evaluation Function}
\label{module:ANNEvalFun}

This module allows the creation of an Artificial Neural Network (ANN)
which can be used as an evaluation function for the checkers
game. Unfortunately there has not been enough time to do much
experimentation with it---or integrate it into the GUI, it can replace
the existing evaluation function in the code but there is no GUI
widget to do this from the interface---but a reasonably effective set
of weights has been found using the Genetic Algorithm (GA) in the
\verb!EvolveANN! module \myref{module:EvolveANN}.

\begin{code}
module ANNEvalFun where
import EvalFuns(EvalFun)
import GameState
import Board
import Data.List(foldl')
import System.Random(mkStdGen, randoms, randomRIO, randomIO)
\end{code}

\subsection{A simple ANN}

This module uses multi-layer feed-forward network, the number of
layers and the weights of each layer are configurable---and are thus
candidates for optimization with the GA.

A type synonym---\verb!ANNWeights!---is defined to represent the
weights. It is defined as a list of lists of lists of floating point
numbers. The organization is a list for each layer (except the first)
containing a list for each neuron in that layer containing a weight
for each neuron on the previous layer plus a weight for the bias.

\funidx{ANNWeights}
\label{data:ANNWeights}
\begin{code}
type ANNWeights = [[[Float]]]
\end{code}


This organization reflects the manner in which each value is needed
and makes the code very simple. The code for the ANN itself consists
of a single function--\verb!makeANN!--which takes the weights as its
first parameter and returns a function from a list of input
values---one for each input neuron---to a list of output values. The
activation function used is the sigmoid function: $\frac{1}{1+e^{-x}}$.

\funidx{makeANN}
\label{function:makeANN}
\begin{code}

makeANN :: ANNWeights -> [Float] -> [Float]
makeANN [] inputs = inputs
makeANN (w:ws) inputs = makeANN ws $ map sigmoid weightedSum
    where weightedSum = [foldl' (+) 0 $ zipWith (*) w' (1.0:inputs)| w' <- w]
          sigmoid x = 1.0 / (1.0 + exp (-x))

\end{code}

A function to generate a random set of weights is also provided. It
takes a list containing an int for each layer giving the number of
neurons in that layer and a seed for the random number generator. Note
that this function is completely deterministic, given the same seed it
will always return the same weights.

\funidx{randomWeights}
\begin{code}
randomWeights :: [Int] -> Int -> ANNWeights
randomWeights (x:x':xs) seed = weights : randomWeights (x':xs) seed'
    where gen = mkStdGen seed
          (seed' : rands) = randoms gen
          weights = [[(j*2)-1|j <- take (x+1) $ randoms $ mkStdGen k]|k <- take x' rands]
randomWeights _ _ = []
\end{code}

\subsection{An evaluation function using the ANN}

The \verb!annEvalFun! function takes a set of weights and returns an
evaluation function---which, as you will remember from the
\verb!EvalFuns! \myref{module:EvalFuns} module is itself is a function
of \verb![GameState]! to Int---based on the resulting ANN.

The inputs are created by first swapping over the board if necessary
so that the current player is \verb!Black! then making a list with 4
values for each square. There's a value to indicate if the square is
occupied by a \verb!White! piece, one to indicate if it's occupied by
a \verb!Black! piece and two more to indicate if those pieces are
kings.

To get an output the mean of the values of the output neurons is taken
and scaled to a number between 0 and the maximum value of \verb!Int!.

\funidx{annEvalFun}
\begin{code}
annEvalFun :: ANNWeights -> EvalFun
annEvalFun weights = eval
    where eval gs@(GameState _ _ _ White) = eval gs{board=swapSides $ board gs, turnNext=Black}
          eval (GameState _ _ board _) = let outputs = ann $ inputs board in
                                         round $ (fromIntegral (maxBound :: Int)) * 
                                                 ((sum $ outputs) / (fromIntegral $ length outputs))
          inputs board = do (_,sc) <- allSquares board
                            -- id is identify so these to lines decide
                            -- whether we're using the inverse of our
                            -- two functions
                            f1 <- [id, not]
                            f2 <- [id, not]
                            case sc of
                              Nothing -> return 0
                              Just (Checker s t) -> do if (f1 $ s == White) && 
                                                          (f2 $ t == Normal) 
                                                         then return 1
                                                         else return 0
          ann = makeANN weights

\end{code}