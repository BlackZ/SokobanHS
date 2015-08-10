# SokobanHS

A simple Sokoban clone written in Haskell

## Requirements

* gtk2hs-buildtools need to be installed

Afterwards it can be build using cabal

## Game

* Use the player (yellow circle) to push the crates (brown rectangles) to the target positions (green tiles).

## Controls

* WASD to control the player
* R step back
* N skip to next level
* P go to previous level
* Q quit the game

When a level is cleared, the next level is loaded when WASD is pressed again.


## TODOS:

* Handling errornous files
* Show on gui when all levels are done or loop back to first
* Nicer tile images
* Ki to solve level
* Scaling of window
