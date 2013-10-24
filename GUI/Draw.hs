module Sudoku.GUI.Draw where
import Prelude
import FPPrac.Graphics
import FPPrac.Events
import Sudoku.GUI.State
import Sudoku.GUI.Button
import Sudoku.Intelligence

--Measurements       
width     = 500
factor    = width / 3
rowfactor = width / 9

--Custom colors
blueb, bluel :: Color
blueb = makeColor 0.25 0.58 1.75 1
bluel = makeColor 0.25 0.58 1.75 0.25

draw :: State -> t -> Picture
draw s e | invalidInput s     = Pictures $ d ++ drawWarning "INVALID INPUT: please provide a number between 1 and 9"
         | invalidOutcome s   = Pictures $ d ++ drawWarning "INVALID INPUT: this sudoku will be insolvable when inserting this number"
         | partiallySolved s  = Pictures $ d ++ drawWarning "Warning: this sudoku can only partially be solved"
         | otherwise          = Pictures $ d
         where d = [drawBackground s, drawNumbers (sudokuSolution s) green, drawNumbers (sudoku s) white ]

redraw :: State -> Input -> [Output]
redraw s e  = [DrawOnBuffer True, ScreenClear, DrawPicture $  draw s e]

drawRow :: [Char] -> Float -> Float -> Color -> [Picture]
drawRow [] x y c    = []
drawRow (n:r) x y c | n == '.'  = [Translate x y $ Color c $ Scale 0.25 0.25 $ Text " "] ++ drawRow r (x+rowfactor) y  c
                    | otherwise = [Translate x y $ Color c $ Scale 0.25 0.25 $ Text [n]] ++ drawRow r (x+rowfactor) y  c            

drawRows :: Int -> [Char] -> Color -> [Picture]
drawRows n su c | n < 9 = (drawRow (getRow n su) (-364) ((width /2)-(rowfactor*toEnum n)-5) c) ++ drawRows (n+1) su c
                | otherwise = []

drawNumbers :: [Char] -> Color -> Picture
drawNumbers su c = Pictures $ drawRows 0 su c

drawBackground :: State -> Picture
drawBackground s = Pictures $
  [ Translate 0 0 $ Color (makeColor 0.0 0.1 0.1 1) $ rectangleSolid 800 600
  , Translate (-380) (-290) $ Color (makeColor 1 1.5 0.5 1) $ Scale 0.2 0.2 $ Text "Sudoku solver" 
  , Translate (-200) (-290) $ Color (makeColor 0.5 0.5 0.5 1) $ Scale 0.10 0.10 $ Text "(c) Wietze Beukema, 2013"] 
    ++ drawButton solveButton
    ++ drawButton (loadButton s)
    ++ drawButton resetButton
    ++ (concat $ map (drawBlock) [0..8])
    ++ (concat $ map (drawGrid)  [0..2])

drawGrid :: Int -> [Picture]
drawGrid i = [ Translate (-132 - l + a) (35) $ Color bluel $ rectangleWire 1 width,
               Translate (-132 + l + a) (35) $ Color bluel $ rectangleWire 1 width,
               Translate (-132) (35 - l + a) $ Color bluel $ rectangleWire width 1,
               Translate (-132) (35 + l + a) $ Color bluel $ rectangleWire width 1]
           where a = factor * (toEnum ((i `mod` 3) - 1))
                 l = factor / 6

drawBlock :: Int -> [Picture]
drawBlock i = [ Translate (-132 + a) (35 - b) $ Color (makeColor 1 1 1 0.1) $ rectangleSolid f f 
              , Translate (-132 + a) (35 - b) $ Color blueb $ rectangleWire f f]
            where a = factor * (toEnum ((i `mod` 3) - 1))
                  b = factor * (toEnum ((i `div` 3) - 1))
                  f = factor - 2            

drawWarning :: String -> [Picture]
drawWarning text = [Translate (-380) (-230) $ Color red $ Scale 0.12 0.12 $ Text text]
