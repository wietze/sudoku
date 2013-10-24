module Sudoku.Main where

import Prelude
import FPPrac.Graphics
import FPPrac.Events
import Sudoku.GUI.Events
                  
main :: IO ()
main = installEventHandler "Sudoku solver" handleEvents initialState Blank 25