
isPrime :: [Int] -> Int -> Bool
isPrime (p : ps) n
  = p * p > n || (n `rem` p /= 0 && isPrime ps n)

primeList :: [Int]
primeList
  = 2 : 3 : primeList'
  where
    primeList' :: [Int]
    primeList'
      = filter (isPrime primeList') [5, 7 ..]

problem50 :: [Int] -> (Int, Int) -> (Int, Int)
problem50 (current : rest) (prime, noPrimes)
  | current > 10000    = (prime, noPrimes)
  | noPrimes' > noPrimes = problem50 rest (current, noPrimes')
  | otherwise            = problem50 rest (prime, noPrimes)
  where
    noPrimes' = findSum current primeList
    
    findSum :: Int -> [Int]-> Int
    findSum prime (x : xs)
      | x < prime = if sum == 0 then findSum prime xs else sum
      | otherwise = 1
      where
        sum = findSum' (prime - x) xs 1
        
        findSum' :: Int -> [Int] -> Int -> Int
        findSum' prime (x : xs) sum
          | prime < 0  = 0
          | prime == 0 = sum
          | otherwise  = findSum' (prime - x) xs (sum + 1)
     
    
