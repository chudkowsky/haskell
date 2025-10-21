

concatenate :: [a] -> [a] -> [a]
concatenate [] y = y
concatenate (first:tail) y = first : concatenate tail y 

member :: Eq a => a -> [a] -> Bool 
member x [] = False
member x (first:tail) = x == first || member x tail

delete_first :: Eq a => a -> [a] -> [a] 
delete_first x [] = []
delete_first x (first:tail) 
    | x == first = tail
    | otherwise = first:delete_first x tail


delete :: Eq a => a -> [a] -> [a] 
delete x [] = []
delete x (first:tail) 
    | x == first = delete x tail
    | otherwise = first:delete x tail


split :: Ord a => a -> [a] -> ([a],[a])
split _ [] = ([],[])
split x (first:tail) 
    | x < first  = (first:ll, rl)
    | otherwise = (ll, first:rl)
    where 
        (ll, rl ) = split x tail
main = do 
    print(concatenate [1,2,3] [4,5,6] == [1,2,3,4,5,6]) 
    print( member 3 [1,2,4,5,6] == False)
    print( member 3 [1,2,3,4,5,6] == True)
    print( delete_first 3 [1,2,4,3,3,3,5,6])
    print( delete 3 [1,2,4,3,3,3,5,6])
    print( split 3 [1,2,3,4,5,6])