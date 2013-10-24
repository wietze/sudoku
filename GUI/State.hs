module Sudoku.GUI.State where
import Prelude

data State = State
  { invalidInput :: Bool
  , invalidOutcome :: Bool
  , selectedCell :: Maybe (Int, Int)
  , sudokuSolution :: String
  , sudoku :: String
  , partiallySolved :: Bool
  , example :: Int
  }