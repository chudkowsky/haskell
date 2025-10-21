-- poczatkowy kapital
-- stopa procentowa
-- ilosc kapitalizacji 
-- doplata co okres rozliczeniowy
-- warunkowy podatek belki 

calculate :: Double -> Double -> Int -> Double -> Bool -> Double
calculate initialCapital _ 0 _ _ =
    initialCapital 

calculate initialCapital rate periods contribution tax = 
    let interest  = initialCapital * rate
        taxMultiplier = if tax then 0.81 else 1.0
        realInterest = interest * taxMultiplier
        newCapital = initialCapital + realInterest + contribution
    in calculate newCapital rate (periods - 1) contribution tax  

main = do 
    print (calculate 1000 0.05 3 100 True)