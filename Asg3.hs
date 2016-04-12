-- Use this to start from. This contains the same data as is in the file small.txt
-- but instead of having to design functions that read from a file, you need only
-- invoke the 0-argument functions given here and assign the returned values into 
-- variables that you use.

-- This file contains only the data. It does not make real use of one of the great
-- strengths of Haskell. Namely, new types are not defined.
import Data.Maybe
import Data.List

size_of_group :: Int
size_of_group = 6

group_elements :: [Char]
group_elements = ['0','1','2','3','4','5']

cayley_table :: [[Char]]
cayley_table = [ ['0','1','2','3','4','5'],
                 ['1','0','3','2','5','4'],
                 ['2','4','0','5','1','3'],
                 ['3','5','1','4','0','2'],
                 ['4','2','5','0','3','1'],
                 ['5','3','4','1','2','0']
               ]

number_of_symbols :: Int
number_of_symbols = 4

allowed_symbols :: [Char]  
allowed_symbols = ['0','1','2','3']

board_size :: Int
board_size = 5

board_as_text :: [[Char]]
board_as_text = [ ['X','X','X','X','X'],
                  ['X','.','.','.','X'],
                  ['X','.','.','.','X'],
                  ['X','.','.','X','X'],
                  ['X','X','X','X','X']
                ]

number_of_clues :: Int
number_of_clues = 6

data Clue = MakeClue Int
                     Int
                     Char
                     Char
                     
instance Show Clue where
    show (MakeClue col row dir val) =
       "Clue " ++ (show col) ++ " "
               ++ (show row) ++ " "
               ++ (show dir) ++ " "
               ++ (show val) ++ "\n"

getDirection :: Clue -> Char
getDirection (MakeClue _ _ d _) = d
               
clues :: [Clue]
clues = [ MakeClue 1 1 '>' '2',
          MakeClue 1 1 'V' '4', 
          MakeClue 1 2 '>' '5',
          MakeClue 2 1 'V' '2', 
          MakeClue 1 3 '>' '3', 
          MakeClue 3 1 'V' '5' ]

-- Replacement in a 1D list
replace1D :: Int -> [Char] -> Char -> [Char]
replace1D 0 (_:xs) c = (c:xs)
replace1D n (x:xs) c = x:(replace1D (n-1) xs c)       
          
replace2D :: Int -> Int -> [[Char]] -> Char -> [[Char]]
replace2D 0 n (x:xs) c = (replace1D n x c) : xs
replace2D m n (x:xs) c = x:(replace2D (m-1) n xs c)

replaceDot :: Int -> Int -> [[Char]] -> Char -> Maybe [[Char]]
replaceDot m n xs y = let c = xs !! m !! n in -- could be wrong order for m and n ... test this
                        if ((c == '.') || (c == y)) then
                            Just (replace2D m n xs y)
                        else
                            Nothing
--------------------

-- Counts the amount of a given character in a list
countHorizSpots :: Char -> [Char] -> Int
countHorizSpots x [] = 0
countHorizSpots x (y:ys) | x == y    = 1 + countHorizSpots x ys
                         | otherwise = countHorizSpots x ys 
                         
countHorizSpots2 :: Int -> Int -> [[Char]] -> Maybe Int
countHorizSpots2 0 n (x:xs) = let c = goToSpot 0 n (x:xs) in
                                if(c == '.') then
                                    Just (countHorizSpots c x) : xs
                                else
                                    Nothing


-- Goes to specified spot in a given list and returns the character at the spot --
goToSpot :: Int -> Int -> [[Char]] -> Char
goToSpot 0 n (x:xs) = (goToSpotHelp n x)
goToSpot m n (x:xs) = (goToSpot (m-1) n xs)

goToSpotHelp :: Int -> [Char] -> Char
goToSpotHelp 0 (x:xs) = x
goToSpotHelp n (x:xs) = goToSpotHelp (n-1) xs


                            