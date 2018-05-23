-----   Problem 1   -----

multiples_3_5 :: Int -> Int
multiples_3_5 n = sum [x | x <- [3..n], (divisibleBy x 3) || (divisibleBy x 5)]

divisibleBy :: Int -> Int -> Bool
divisibleBy n x = (n `mod` x) == 0

-----   Problem 2   -----

-- TODO this can be done in a nicer way
fib n = fastFib 1 1 n
        where
            fastFib a b 0 = a
            fastFib a b n = fastFib b(a+b)(n-1)

fib_sum x m
    | res >= m = 0
    | even res  = res + fib_sum (x+1) m
    | otherwise = fib_sum (x+1) m
        where
            res = fib x

-----   Problem 3   -----
primes = 2 : filter (null . tail . primefactors) [3,5..]

primefactors n = factor n primes
  where
    factor n (p:ps)
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      =     factor n ps

-----   Problem 4   -----

-- TODO look if there are some properties that can help speed up this algorithm
palindrom n = let s = show n in s == reverse s
max_palindrom digit = maximum[(y*z, y, z) | y <- [min_digit..max_digit],
                                            z <- [min_digit..max_digit],
                                            palindrom (y*z)]
                    where
                        max_digit = 10^digit - 1
                        min_digit = 10^(digit-1)

-----   Problem 5   -----

smallest_multiple to = smallest_multiple' (div to 2) to step
                            where
                                smallest_multiple' f t n
                                    | all (divisibleBy n) [f..t] = n
                                    | otherwise = smallest_multiple' f t (n+step)
                                step  = highest_kgv (div to 2) to
                                highest_kgv from to = maximum [lcm y to | y <- [from..to]]

-----   Problem 6   -----
-- TODO do this more efficient
sum_of_squares n = sum (map (^2) [1..n])
square_of_sums n = (sum [1..n])^2
