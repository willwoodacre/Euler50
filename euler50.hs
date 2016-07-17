
isPrime :: Int -> Bool
isPrime n
  = if (n == 0) || (n == 1) then False else isPrime' n (floor (sqrt (fromIntergral n)))
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
