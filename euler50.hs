
isPrime :: [Int] -> Int -> Bool
isPrime (p : ps) n
  = p * p > n || n `rem` p /= 0 && isPrime ps n

primeList :: [Int]
primeList
  = 2 : primeList'
  where
    primeList' :: [Int]
    primeList'
      = 3 : filter (isPrime primeList') [5, 7 ..]

problem50 :: [Int] -> (Int, Int) -> (Int, Int)
problem50 (current : rest) (prime, noPrimes)
  | current > 1000000    = (prime, noPrimes)
  | noPrimes' > noPrimes = problem50 rest (current, noPrimes')
  | otherwise            = problem50 rest (prime, noPrimes)
  where
    noPrimes' = findSum current primeList primeList 0 noPrimes
    
    findSum :: Int -> [Int] -> [Int] -> Int -> Int -> Int
    findSum prime (top : tops) (bottom : bottoms) noPrimes curBest
      | top == prime = 0
      | prime == 0   = noPrimes     
      | prime > 0    = findSum (prime - top) tops (bottom : bottoms) (noPrimes + 1) curBest
      | otherwise    = if curBest > noPrimes then 0 
                       else findSum (prime + bottom) (top : tops) bottoms (noPrimes - 1) curBest
