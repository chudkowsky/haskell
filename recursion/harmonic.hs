harmonic :: Int -> Double
harmonic 1 = 1.0
harmonic n = (1.0/fromIntegral(n)) + harmonic( n-1)

main = do 
    print(harmonic 5)