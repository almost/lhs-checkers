Literate Haskell Checkers
=========================

Thomas Parslow (tom@almostobsolete.net)

A literate Haskell program that plays Checkers. So it's both a LaTeX document AND a Haskell program at the same time, I think that's kind of cool.

Written a a few years ago while at the University of Sussex, just sticking it on GitHub now to keep a hold of it. It works ok although it's not the best Checkers player. The GUI is a bit crappy also :)

There's a Negamax (like Minimax) eval function which is used for the default play strategy. There's also a ANN (Artificial Neural Network) based eval function with a GA (Genetic Algorithm) based trainer because at the time I was rather fond of implementing ANNs and GAs, they don't work especially well though!

I've included the compiled PDF in the repository, but it can also be generated from the sources. 