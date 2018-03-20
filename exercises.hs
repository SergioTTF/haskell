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

doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (a:z) = (2*a) : doubleList z

type Person = String
type Book = String
type DataBase = [(Person, Book)]

books :: DataBase -> Person -> [Book]
books [] _ = []
books (a:z) p
        | fst a == p = snd a : books z p
        | otherwise = books z p

rents :: DataBase -> Book -> [Person]
rents [] _ = []
rents (a:z) l
        | snd a == l = fst a : rents z l
        | otherwise = rents z l

rented :: DataBase -> Book -> Bool
rented [] _ = False
rented (a:z) l
        | snd a == l = True
        | otherwise = rented z l

rentsNumber :: DataBase -> Person -> Int
rentsNumber [] _ = 0
rentsNumber (a:z) p
        | fst a == p = 1 + rentsNumber z p 
        | otherwise = rentsNumber z p

rent :: DataBase -> Person -> Book -> DataBase
rent [] p l = [(p, l)]
rent d p l = (p, l) : d

returnBook :: DataBase -> Person -> Book -> DataBase
returnBook [] _ _ = []
returnBook (a:z) p l
        | fst a == p && snd a == l = z
        | otherwise = a : returnBook z p l


--*member :: [Int] -> Int -> Bool
--member []