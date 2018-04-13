mdc :: Int -> Int -> Int
mdc 0 _ = -1
mdc a b
    | a >= b = last [x | x <- [1..a], a `mod` x == 0 && b `mod` x == 0]
    | otherwise = last [x | x <- [1..b], a `mod` x == 0 && b `mod` x == 0]

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime a = a == last (eratosthenes [2..a])
    where
    eratosthenes :: [Int] -> [Int]
    eratosthenes [] = []
    eratosthenes (i:j) = i : (eratosthenes (filter (\x -> mod x i /= 0) j))