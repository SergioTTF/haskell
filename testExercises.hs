smaller :: [Int] -> Int
smaller [] = maxBound :: Int
smaller (a:z) = min a (smaller z)

myMap :: (a -> b -> b) -> [a] -> [b] -> [b]
myMap f [] _ = []
myMap f _ [] = []
myMap f (x:xs) (y:ys) = f x y : myMap f xs ys

fibonacci :: Int -> Int
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

subseq :: [Int] -> [[Int]]
subseq [] = [[]]
subseq [a] = [[a]]
