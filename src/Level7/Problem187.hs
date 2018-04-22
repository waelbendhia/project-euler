import Data.Numbers.Primes

isSemiPrime = (== 2) . length . primeFactors
-- (x+y)/(x*y) = 1/n
-- (x*y) = nx+ny
-- xy - nx - ny = 0
