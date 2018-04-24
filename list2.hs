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

type Point = (Double, Double, Double)

distance :: Point -> Point -> Double
distance (xA, yA, zA) (xB, yB, zB) = sqrt ((xA - xB)**2 + (yA - yB)**2 + (zA - zB)**2)

powSum :: Int -> [Int]
powSum a = [x * x | x <- [1..a]]

--grid :: Int -> Int -> [(Int, Int)]
-- grid m n = [(x, y) | x <-[0..m]]