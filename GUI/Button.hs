module Sudoku.GUI.Button where
import Prelude
import FPPrac.Graphics
import FPPrac.Events
import Sudoku.GUI.State

data Button = Button { xPos :: Float, yPos :: Float, value :: String}

resetButton, solveButton :: Button
resetButton = Sudoku.GUI.Button.Button {xPos=240, yPos=148, value="Clear"}
solveButton = Sudoku.GUI.Button.Button {xPos=240, yPos=260, value="Solve!"}
loadButton :: State ->Button
loadButton s = Sudoku.GUI.Button.Button {xPos=240, yPos=204, value=("Load Sample " ++ (show (example s +1)))}

drawButton :: Button -> [Picture]
drawButton b = [ Translate x y $ Color (makeColor 1 1 1 0.05) $ rectangleSolid 200 50
               , Translate x y $ Color (makeColor 1 1 1 0.5) $ rectangleWire 200 50
               , Translate (x-(t*5)) (y-8) $ Color (white) $ Scale 0.15 0.15 $ Text $ v]
               where x = xPos b
                     y = yPos b
                     v = value b
                     t = toEnum $ length v

isButton :: (Float, Float) -> Button -> Bool
isButton (x, y) b = x > (bx-fx) && x < (bx+fx) && y > (by-fy) && y < (by+fy)
                  where bx = xPos b 
                        by = yPos b
                        fx = 200 / 2
                        fy = 50 / 2      