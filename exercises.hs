-- Exercises

memberList :: Int -> [Int] -> Bool
memberList _ [] = False
memberList x (a:z)
        | x == a = True
        | otherwise = memberList x z

digitsList :: [Char] -> [Char]
digitsList [] = []
digitsList (a:z)
        | (a >= '0') && (a <= '9') = a : digitsList z
        | otherwise = digitsList z

sumPairsList :: [(Int, Int)] -> [Int]
sumPairsList [] = []
sumPairsList (a:z) = (fst a + snd a) : sumPairsList z