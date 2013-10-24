module Sudoku.Intelligence where

import Prelude
import System.Environment
import Data.String
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Char
import Sudoku.GUI.State
 
data MySudoku = Result String | PartResult String | Error
              deriving (Eq, Show)

getElement :: Int -> [a] -> a
getElement i r = r !! i

getRow :: Int -> [Char] -> [Char]
getRow i s =  rows !! i
           where rows = splitOn "\n" s

getColumn :: Int -> [Char] -> [Char]
getColumn i s = map (getElement i) rows
              where rows = splitOn "\n" s   

getBlock :: Int -> [Char] -> [Char]
getBlock i s = f (getRow x s) ++ f (getRow (x + 1) s) ++ f (getRow (x + 2) s)
             where x = (i `div` 3) * 3
                   y = (i `mod` 3) * 3
                   f = drop y . take (y + 3)

coordinateToBlock :: (Int, Int) -> Int
coordinateToBlock (x,y) = ((y `div` 3))*3 + ((x `div` 3))
                                 
blockIndexToCoordinate :: Integral t => t -> t -> (t, t)
blockIndexToCoordinate b i = (x + (i `mod` 3), y + (i `div` 3))
                            where x = (b `mod` 3)*3 
                                  y = (b `div` 3)*3
                                                                        
insertNum :: (Int, Int) -> Char -> [Char] -> [Char]
insertNum (x, y) i s = take 89 $ unlines (take (y) s') ++ r' ++ "\n" ++ unlines (drop (y+1) s')
                         where r  = getRow y s
                               r' = take x r ++ [i] ++ drop (x+1) r  
                               s' = splitOn "\n" s  
          
missingNums :: [Char] -> [Char]
missingNums r =  "123456789" \\ (filter (/= '.') r)

fixNakedSingle :: Bool -> (Int, Int) -> [Char] -> (Bool, Bool, [Char])
fixNakedSingle p (_, 9) s = (True, p, s)
fixNakedSingle p (x, y) s | e == '.' && length res == 1 = fixNakedSingle True (0, 0) $ insertNum (x, y) (head res) s
                          | otherwise                   = fixNakedSingle p (((x + 1) `mod` 9), (y + (x `div` 8))) s
                          where e = getElement x r
                                r = getRow y s 
                                c = getColumn x s 
                                b = getBlock (coordinateToBlock (x,y)) s
                                q = filter (\x -> x /= '.') (nub $ r ++ c ++ b)  
                                res = "123456789" \\ q      

getString :: (Eq a, Num a) => a -> Int -> [Char] -> [Char]
getString 0 q s = getColumn q s
getString 1 q s = getRow q s
getString 2 q s = getBlock q s

getCoordinate :: (Eq a, Integral t, Num a) => a -> t -> t -> (t, t)
getCoordinate 0 a b = (b, a)
getCoordinate 1 a b = (a, b)
getCoordinate 2 a b = blockIndexToCoordinate b a

isAllowed :: (Eq a, Num a) => a -> Int -> Int -> Char -> [Char] -> Bool
isAllowed 0 x y n s = (elem n $ getRow y s) == False    && (elem n $ getBlock (coordinateToBlock (x,y)) s) == False
isAllowed 1 y x n s = (elem n $ getColumn x s) == False && (elem n $ getBlock (coordinateToBlock (x,y)) s) == False
isAllowed 2 b i n s = (elem n $ getRow y s) == False    && (elem n $ getColumn x s) == False
                      where (x, y) = blockIndexToCoordinate b i 

fixHiddenSingle :: (Eq a, Num a) => a -> Int -> [Char] -> (Bool, Bool, [Char])
fixHiddenSingle n q s   | lm == 0             = (True,  False, s)
                        | lm == 1 && lis > 0  = (True,  True,  insertNum (getCoordinate n (head is) q) (head m) s)
                        | lm == 1 && lis == 0 = (False, False, "")
                        | elem True wc        = (True,  True,  insertNum (getCoordinate n x q) (m !! i) s)
                        | otherwise           = (True,  False, s)
                         where r   = getString n q s
                               m   = missingNums r 
                               is  = elemIndices '.' r 
                               wb  = map (\p -> (map (\x -> isAllowed n q x p s) is)) m 
                               wc  = map (\z -> length (filter (\x -> x == True) z) == 1) wb
                               i   = fromJust $ elemIndex True wc 
                               x   = is !! (fromJust $ elemIndex True (wb !! i))
                               lm  = length m
                               lis = length is

isAllowedInRCB :: (Int, Int) -> Char -> [Char] -> Bool
isAllowedInRCB (x, y) n s = (elem n $ getRow y s) == False && (elem n $ getColumn x s) == False && (elem n $ getBlock (coordinateToBlock (x,y)) s) == False

isSolvable :: (Int, Int) -> Char -> [Char] -> Bool
isSolvable (_, _) '.' _ = True
isSolvable (x, y) n s   | (getElement x (getRow y s) == n) = True 
                        | otherwise = (ip == True) && (foldr1 (&&) $ qr ++ qc ++ qb) && (solve s' /= Error)
                        where s' = insertNum (x, y) n s
                              r  = getRow y s'
                              c  = getColumn x s'
                              b  = getBlock (coordinateToBlock (x,y)) s'
                              qr = map (\z -> foldr1 (||) $ map (\p -> isAllowedInRCB (p,y) z s') ir) rr
                              qc = map (\z -> foldr1 (||) $ map (\p -> isAllowedInRCB (x,p) z s') ic) rc
                              qb = map (\z -> foldr1 (||) $ map (\p -> isAllowedInRCB (blockIndexToCoordinate (coordinateToBlock (x,y)) p) z s') ib) rb                         
                              rr = missingNums r
                              rc = missingNums c
                              rb = missingNums b
                              ir = elemIndices '.' r
                              ic = elemIndices '.' c
                              ib = elemIndices '.' b
                              ip = isAllowedInRCB (x, y) n s

solve :: [Char] -> MySudoku
solve s = solve' 0 0 s

solve' :: (Eq a, Num a) => a -> Int -> [Char] -> MySudoku
--Round 1: Check for Naked Singles
solve' 0 a s | z == False = Error
             | x == True = solve y
             | otherwise = solve' 1 0 s
             where (z, x, y) = fixNakedSingle False (0,0) s 

--Round 2: Check for Hidden Singles - Row
solve' 1 a s | z == False = Error
             | x == True = solve y
             | a < 8     = solve' 1 (a+1) s
             | otherwise = solve' 2 0 s
             where (z, x, y) = fixHiddenSingle 0 a s

--Round 3: Check for Hidden Singles - Column
solve' 2 a s | z == False = Error
             | x == True =  solve y
             | a < 8     = solve' 2 (a+1) s
             | otherwise = solve' 3 0 s
             where (z, x, y) = fixHiddenSingle 1 a s

--Round 4: Check for Hidden Singles - Block or give result if not found
solve' 3 a s | z == False = Error
             | x == True = solve y
             | a < 8     = solve' 3 (a+1) s
             | elem '.' s = PartResult s
             | otherwise  = Result s
             where (z, x, y) = fixHiddenSingle 2 a s                               