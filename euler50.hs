
isPrime :: Int -> Bool
isPrime n
  = if (n == 0) || (n == 1) then False else isPrime' n (floor (sqrt (fromIntegral n)))
  where
    isPrime' :: Int -> Int -> Bool
    isPrime' prime divisor
      | divisor == 1             = True
      | prime `mod` divisor == 0 = False
      | otherwise                = isPrime' prime (divisor - 1)

nextPrime :: Int -> Int
nextPrime n
  | isPrime n' = n'
  | otherwise  = nextPrime n'
  where
    n' = (n + 1)

primeList :: [Int]
primeList
  = 2 : map (nextPrime) primeList

problem50 :: [Int] -> (Int, Int) -> (Int, Int)
problem50 (current : rest) (prime, noPrimes)
  | current > 100000    = (prime, noPrimes)
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
     
    
