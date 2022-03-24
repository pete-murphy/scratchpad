module Sieve where

primes = sieve [2 ..]

sieve (x:xs) = x : sieve (filter (\y -> mod y x /= 0) xs)
--  Create a list of consecutive integers from 2 through n: (2, 3, 4, ..., n).
--  Initially, let p equal 2, the smallest prime number.
--  Enumerate the multiples of p by counting in increments of p from 2p to n, and mark them in the list (these will be 2p, 3p, 4p, ...; the p itself should not be marked).
--  Find the first number greater than p in the list that is not marked. If there was no such number, stop. Otherwise, let p now equal this new number (which is the next prime), and repeat from step 3.
--  When the algorithm terminates, the numbers remaining not marked in the list are all the primes below n.
