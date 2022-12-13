main :: IO()
main = do
    print(averageSqr 5 0)
    print(averageSqr 10 13)
    -------------------------------
    print(myGCD 5 13)
    print(myGCD 13 1235)
    print(myGCDPM 5 13)
    print(myGCDPM 13 1235)
    -------------------------------
    print(rev 123)
    print(isPalindrome 123)
    print(isPalindrome 121)
    -------------------------------
    print $ areAmicable 200 300
    print $ areAmicable 220 284
    print $ areAmicable 284 220
    print $ areAmicable 1184 1210 
    print $ areAmicable 2620 2924
    print $ areAmicable 6232 6368
    ------------------------------- 
    print $ hasIncDigits 1244 
    print $ hasIncDigits 12443 
    -------------------------------
    print $ isPrimeG 1 
    print $ isPrimeG 2 
    print $ isPrimeG 3 
    print $ isPrimeG 6
    print $ isPrimeG 61
    print $ isPerfect 1 
    print $ isPerfect 6 
    print $ isPerfect 495 
    print $ isPerfect 33550336 

--Define a function that returns the average of the sum of the squares of two whole numbers.

averageSqr :: Double -> Double -> Double
averageSqr n1 n2 = ((n1 ^ 2) + (n2 ^ 2)) / 2

-- Define a function for calculating the GCD of two whole numbers.

-- guards
myGCD :: Int -> Int -> Int
myGCD x y
    | x == 0 = y
    | y == 0 = x
    | otherwise = myGCD y (mod x y)

-- pattern matching
myGCDPM :: Int -> Int -> Int
myGCDPM 0 y = y
myGCDPM x 0 = x
myGCDPM x y = myGCDPM y (mod x y)

-- Define a predicate that checks whether a non-negative number is a palindrome.

rev :: Int -> Int
rev n = helper 0 n
    where
         helper res num =
            if num < 10 then (res * 10 + num)
            else helper ((res * 10) + (mod num 10)) (div num 10)

isPalindrome :: Int -> Bool
isPalindrome n = (n == rev(n))

-- Two numbers are amicable if the sum of the divisors of one of them is equal to the other.
-- Define a predicate that checks whether two numbers are amicable.

sumDiv :: Int -> Int 
sumDiv n = helper 0 1
    where
        helper sum d
            | d > n = sum
            | (mod n d) == 0 = (helper (sum + d) (d + 1))
            | otherwise = (helper sum (d + 1))

areAmicable :: Int -> Int -> Bool
areAmicable n1 n2 = (sumDiv n1) == (sumDiv n2)

--Define a predicate that checks whether the digits of a non-negative whole number are ordered in an ascending order.

hasIncDigits ::Int -> Bool
hasIncDigits n = helper (div n 10) (mod n 10)
    where 
        helper num prev
            |prev < mod num 10 = False
            |num < 10 = True
            |otherwise = (helper (div num 10) (mod num 10))

-- Define a predicate that checks whether a number is prime.

-- solve using guards;
-- solve using list comprehension in ONE line.

-- guards
isPrimeG :: Int -> Bool 
isPrimeG 1 = False
isPrimeG 2 = True
isPrimeG n = helper 2 
    where
         helper i
             |(i == n)  = True
             |(mod n i) == 0 = False
             |otherwise = (helper (i + 1))

-- A number is perfect if and only if it is natural and equal to the sum of its divisors excluding the number itself. 
-- Define a predicate that checks whether a number is perfect.

sumDiv' :: Int -> Int 
sumDiv' n = helper 0 1
    where
        helper sum d
            | d == n = sum
            | (mod n d) == 0 = (helper (sum + d) (d + 1))
            | otherwise = (helper sum (d + 1))

isPerfect :: Int -> Bool
isPerfect n =
    if n < 0 then False
    else n == (sumDiv' n)
