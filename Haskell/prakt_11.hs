import Data.List
import Data.Char

main::IO()
main = do
    print(isImage [1,2,3,4] [0,1,2,3])
    print(isImage [1,2,3,4] [0,2,5,7])
    ------------------------------------
    print(chunksOf 2 [1,2,3,4])
    ------------------------------------

-- Задача 1. Нека as = [a1, a2 … , ak] и bs = [b1, b2 … , bk] са непразни списъци с еднакъв брой числа.
-- Да се дефинира предикат isImage :: [Int] -> [Int] -> Bool, който да връща „истина“ точно когато съществува такова число x, че ai = x + bi за всяко i = 1,..., k.

isImage::[Int] -> [Int] -> Bool
isImage as bs = helper (tail as) (tail bs) ((head as) - (head bs))
    where
        helper as bs x 
         | as == [] && bs == [] = True
         | not (((head as) - (head bs) == x)) = False
         | otherwise = helper (tail as) (tail bs) x

-- Задача 2. Да се дефинира функция chunksOf :: Int -> [a] -> [[a]], която разделя входния списък на подсписъци с дължина равна на подаденото число.

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = (take n xs):(chunksOf n (drop n xs))

-- Задача 3. Ако f и g са числови функции и n е естествено число, да се дефинира функция от по-висок ред (switchsum f g n), 
-- която връща като резултат функция, чиято стойност в дадена точка x е равна на f(x)+g(f(x))+f(g(f(x)))+ ... (сумата включва n събираеми).

switchsum :: Num a => ( a -> a ) -> ( a -> a ) -> Int -> ( a -> a)
switchsum f g n x = helper n 0 f f g
    where
        helper 1 sum h _ _ = sum + h x -- h -> current function
        helper n sum h f1 g1 = helper (n - 1) (sum + h x) (g1 . h) g1 f1 -- ( . -> compose )

-- Задача 4. Да се дефинира функция (repeater str), която получава като аргумент символен низ и връща анонимна функция на два аргумента - count и glue (число и низ). 
-- Оценката на обръщението към върнатата функция е низ, който се получава чрез count-кратно повтаряне на низа str, при което между всеки две съседни повторения на str стои низът glue.

repeater :: String -> (Int -> String -> String)
repeater str = helper 
    where count glue == concatMap (++ glue) (repeater (count - 1) str) ++ str
