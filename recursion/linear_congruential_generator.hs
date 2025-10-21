-- Linear Congruential Generator (LCG) to prosty algorytm generowania ciągów liczb pseudolosowych. Wygenerowany ciąg jest zdefiniowany w następujący sposób:

-- X0=s
-- Xn=(aXn−1+c)modm

-- Napisz funkcję, która zwraca n-ty element wygenerowanego ciągu.

-- lcg :: Integer -> Integer -> Integer -> Integer -> Int -> Integer
-- Parametry:
-- multiplier — mnożnik (a)
-- increment — przyrost (c)
-- modulus — moduł (m)
-- seed — ziarno (s)
-- n — numer elementu ciągu
-- Dla n = 0, funkcja zwraca wartość ziarna.

-- Przykłady:

-- prosty generator: a = 3, c = 5, m = 16, s = 7

-- ghci> lcg 3 5 16 7 0
-- 7
-- ghci> lcg 3 5 16 7 1  -- (3×7 + 5) mod 16 = 26 mod 16 = 10
-- 10
-- ghci> lcg 3 5 16 7 2  -- (3×10 + 5) mod 16 = 35 mod 16 = 3
-- 3
-- ghci> lcg 3 5 16 7 3  -- (3×3 + 5) mod 16 = 14 mod 16 = 14
-- 14
-- generator MINSTD (Park-Miller)

-- minstd seed n = lcg 16807 0 (2^31 - 1) seed n

lcg :: Integer -> Integer -> Integer -> Integer -> Int -> Integer
lcg _ _ _ s 0 = s
lcg a c m s n = (a* (lcg a c m s (n-1)) + c ) `mod` m


main = do 
    print(lcg 3 5 16 7 0 == 7)
    print(lcg 3 5 16 7 1 == 10)
    print(lcg 3 5 16 7 2 == 3)
    print(lcg 3 5 16 7 3 == 14)
    print(lcg 16807 0 (2^31 - 1) 5 1)
