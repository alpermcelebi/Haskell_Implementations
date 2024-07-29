module PE1 where

-- Question 1
findInList :: (Eq a) => [a] -> [a] -> [Int]
findInList _ [] = []
findInList pattern list = findex 0 pattern list

--helper
findex _ [] _ = []
findex _ _ [] = []
findex currentindex (a:rest1) (b:rest2) | a == b = currentindex : findex (currentindex + 1) rest1 rest2
                                        | otherwise = findex (currentindex + 1) (a:rest1) rest2

-- Question 2
findInMatrix :: (Eq a) => [[a]] -> [[a]] -> [[(Int, Int)]]
findInMatrix list1 list2 = helper2 (length (list1 !! 0))  (helper (traverseDiagonal list1) (traverseDiagonal list2))



traverseDiagonal matrix = concatMap (\d -> traverseDiagonalHelper matrix d) diagonals
    where
        numRows = length matrix
        numCols = length (head matrix)
        diagonals = [0..numRows+numCols-2]

        checkindex :: Int -> Int -> Bool
        checkindex row col = row >= 0 && row < numRows && col >= 0 && col < numCols

        traverseDiagonalHelper :: [[a]] -> Int -> [(a, (Int, Int))]
        traverseDiagonalHelper mat d = [ (mat !! row !! col, (row, col)) | row <- [0..d], let col = d - row, checkindex row col]

helper _ [] = []
helper [] _ = []
helper (a:rest1) (b:rest2) | fst a == fst b = snd b : helper rest1 rest2
                           | otherwise = helper (a:rest1) rest2



helper2 n xs = groupHelper n xs []
  where
    groupHelper _ [] acc = [acc]
    groupHelper 0 ys acc = acc : helper2 n ys
    groupHelper m (y:ys) acc = groupHelper (m - 1) ys (acc ++ [y])
                                                                                                                                                                                                      
