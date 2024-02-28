module Power where
-- Code for the Power to the People Coursework.
-- Write all your code in this file.
-- *DO NOT CHANGE THE FUNCTION NAMES OR TYPE SIGNATURES*

---------------------------------------------
-- An example implementation of the power function.
-- *DO NOT EDIT THIS FUNCTION OR USE IT IN YOUR CODE*

power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- Task 1 -------------------------

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1


-- Task 2 -------------------------

power1 :: Integer -> Integer -> Integer
power1 n k = product (replicate (fromInteger k) n)

-- Task 3 -------------------------

power2 :: Integer -> Integer -> Integer
power2 n 0                    = 1
power2 n k  |  mod k 2 == 0   =   power2 (n * n) (div k 2)
power2 n k  |  mod k 2 /= 0   =   n * power2 n (k - 1)


-- Task 4 -------------------------

comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k |  power1 n k == power n k =  True
comparePower1 n k |  otherwise               = False

comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k |  power2 n k == power n k =  True
comparePower2 n k |  otherwise               =  False

-- Task 5 -------------------------

-- Each entry should be in this format: (n, k, result of comparePower1, result of comparePower2)
comparisonList :: [Integer] -> [Integer] -> [(Integer, Integer, Bool, Bool)]
comparisonList ns ks = [(n, k, comparePower1 n k, comparePower2 n k) | n <- ns, k <- ks]