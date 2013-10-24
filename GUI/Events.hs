module Sudoku.GUI.Events where
import Prelude
import FPPrac.Graphics
import FPPrac.Events
import Sudoku.GUI.State
import Sudoku.GUI.Button
import Sudoku.GUI.Draw
import Sudoku.Intelligence
import Data.Maybe

initial  =  ".........\n.........\n.........\n.........\n.........\n.........\n.........\n.........\n........."
examples = ["......48.\n.....13..\n..75.....\n....8.9.6\n64.......\n.1...5..7\n..1.6....\n89.3.7..2\n..6.4.73."
           ,"67.51..84\n91.......\n.........\n8...2.1..\n..4.3.5..\n..6.7...9\n.........\n.......27\n35..46.18"
           ,"1......28\n..3.1...6\n..54.....\n...8...75\n7..2...9.\n.84.5.2..\n...52....\n..1....6.\n6...4...3"]

initialState :: State
initialState = State { invalidInput = False, invalidOutcome = False, selectedCell = Nothing, sudoku = initial, sudokuSolution = initial, partiallySolved = False, example = 0 }

handleEvents :: State -> Input -> (State, [Output])
handleEvents s (MouseMotion (mx, my)) = (s, redraw s $ MouseMotion (mx, my))

handleEvents s (MouseDown (mx, my)) = (s', redraw s' $ MouseDown (mx, my))
                                    where s' = s { invalidInput = False, invalidOutcome = False, partiallySolved = False }

handleEvents s (MouseUp (mx, my)) | isCell mx my = q'
                                  | isButton (mx, my) solveButton= q s'
                                  | isButton (mx, my) (loadButton s) = q s''
                                  | isButton (mx, my) resetButton = q s'''
                                  | otherwise = (s, [])
                                  where q r  = (r, redraw r $ MouseUp (mx, my))
                                        q'   = (s { selectedCell = Just (a, b) }, [GraphPrompt ("Enter a number", "Number(1-9) or leave emtpy")])                                        
                                        s'   = fixSudoku s                                
                                        s''  = s { sudoku = examples !! example s, example = (((example s) + 1) `mod` (length examples)), sudokuSolution = initial }
                                        s''' = s { sudoku = initial, sudokuSolution = initial }
                                        (a, b) = getCell mx my            

handleEvents s (Prompt ("Enter a number", n)) | length n == 1 && elem  (head n) "123456789" = (s'' , redraw s''  $ Prompt ("Enter a number", n))                                             
                                              | length n == 0                               = (s''', redraw s''' $ Prompt ("Enter a number", n))
                                              | otherwise = (s', redraw s' $ Prompt ("Enter a number", n))
                                              where s' = s { invalidInput = True }
                                                    (x, y) = fromJust $ selectedCell s
                                                    s'' = tryChangeNumber (head n) x y s
                                                    s''' = tryChangeNumber '.' x y s
handleEvents s _ = (s, []) 

isCell :: Float -> Float -> Bool
isCell  x y = x > (-132-w) && y > (35-w) && x<(-132+w) && y < (35+w)
            where w = (width) / 2

getCell :: (Integral t, Integral t1) => Float -> Float -> (t, t1)
getCell x' y' = (floor (x / (width/9)), 8-floor (y / (width/9)))
              where x = x'-(-132-(width/2));  y = y'+(-35+(width/2))                     

tryChangeNumber :: Char -> Int -> Int -> State -> State
tryChangeNumber n x y s | accepted == True = s'
                        | otherwise        = s {invalidOutcome = True}
                        where accepted = isSolvable (x, y) n (sudoku s)
                              s' = s { sudoku = insertNum (x, y) n (sudoku s)}

fixSudoku' :: State -> MySudoku -> State
fixSudoku' s (Result o)     = s { sudokuSolution = o }
fixSudoku' s (PartResult o) = s { sudokuSolution = o, partiallySolved = True }

fixSudoku :: State -> State
fixSudoku s = fixSudoku' s $ solve (sudoku s)