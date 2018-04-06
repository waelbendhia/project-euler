-- After running this brute force solution for 10^5 I calculated that for 10^7
-- it should reach a solution in a little less than two hours as it runs in
-- O(n*sqrt(n)) time. So a different approach is required.
-- Check the rust repo for the algorithm. I don't Haskell well enough to be 
-- able to write an algorithm with mutable arrays.
bruteForce :: Num t => Integer -> t
bruteForce bound = helper allNumberOfDivisors
  where
    helper (x1:x2:xs)
      | fst x1 >= bound = 0
      | snd x1 == snd x2 = 1 + helper (x2 : xs)
      | otherwise = helper (x2 : xs)

allNumberOfDivisors :: [(Integer, Integer)]
allNumberOfDivisors = map (\x -> (x, numDivisors x)) [1 ..]

numDivisors :: Integer -> Integer
numDivisors n =
  (if isCube
     then 1
     else 2) +
  2 * (fromIntegral $ length $ divs)
  where
    isCube = root ^ 2 == n
    root = intSqrt n
    divs = [x | x <- [2 .. root], n `rem` x == 0]

intSqrt = truncate . sqrt . fromIntegral
