main :: IO()
main = do
    print(myMaxDiv 10)
    print(sumOdds 1 5)
    print(isPrime' 4)
    print(isPrime' 5)
    print(isPalindrome 123)
    print(isPalindrome 12321)
    print(countDiv 9)
    
--  Да се напише функция mymin, която приема два аргумента и връща по-малкия от тях.

myMin :: Int -> Int -> Int
myMin a b = if a < b then a else b

-- Да се дефинира функцията isInside x a b, която проверява дали числото x се намира в затворения интервал [a, b].

isInside' :: Int -> Int -> Int -> Bool
isInside' x a b 
    | x > b = False 
    | x < a = False
    | otherwise = True

-- Да се напише функция myfunc, която пресмята средно аритметично на квадратите на 2 числа.
myfunc' :: Double -> Double -> Double
myfunc' a b = ((a * a) + (b * b)) / 2

-- Да се напише myfib, която получава един аргумент n и връща n-тото число на Фибоначи. 
-- Да се напише и итеративно решение

myFib :: Int -> Int 
myFib n 
    | n == 0 = 0
    | n == 1 = 1
    |otherwise = myFib( n - 1) + myFib( n - 2 )

myFibIter :: Int -> Int 
myFibIter n = helper 1 0 0
    where
        helper prev curr i =
            if i == n then curr
            else helper curr (prev + curr) (i + 1)

-- Да се напише функция mymaxdivisor x, която намира най-големия делител d на цялото число x > 1, за който d < x.

myMaxDiv :: Int -> Int
myMaxDiv x = helper (x - 1)
    where
        helper d = 
            if (mod x d) == 0 then d
            else helper (d - 1)

-- Да се дефинира функция, която намира сумата на нечетните числа в затворения интервал [a, b].

isOdd :: Int -> Bool
isOdd n =
    if (mod n 2) == 0 then False else True 

sumOdds :: Int -> Int -> Int
sumOdds a b = helper a 0
    where
        helper i res 
            | i > b = res
            | (isOdd i) = helper (i + 1) (res + i)
            | otherwise = helper (i + 1) res

-- Да се дефинира предикат, който проверява дали естественото число n е просто.

isPrime' :: Int -> Bool 
isPrime' n  
    | n <= 1 = False
    | n == 2 = True 
    | otherwise = helper 2
    where 
        helper i 
            | i == n = True
            | mod n i == 0 = False
            | otherwise = helper (i + 1)
            
-- Да се дефинира функция, която намира броя на палиндромите в интервала [a, b], където a и b са цели неотрицателни числа и a<b.

revNum :: Int -> Int 
revNum n = helper n 0
    where helper curr res =
             if curr < 10 then (res * 10) + curr 
             else helper (div curr 10) ((res * 10) + (mod curr 10))

isPalindrome :: Int -> Bool
isPalindrome n =
    if n == (revNum n) then True else False

-- Да се дефинира функция, която чрез линейно итеративен процес намира броя на естествените делители на едно естествено число.

countDiv :: Int -> Int 
countDiv n = helper 1 0
    where
        helper i cnt
            | i > n = cnt
            | (mod n i) == 0 = helper ( i + 1 ) (cnt + 1)
            | otherwise = helper ( i + 1 ) cnt
