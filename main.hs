-- poczatkowy kapital
-- stopa procentowa
-- ilosc kapitalizacji 
-- doplata co okres rozliczeniowy
-- warunkowy podatek belki 

calculate :: Double -> Double -> Int -> Double -> Bool -> Double
calculate initialCapital rate periods contribution tax = 

main = do 
    print (calculate (100,5,20,20, False))