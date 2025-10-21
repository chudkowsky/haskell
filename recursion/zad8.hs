-- owanie ciągu ruchów
-- Ciąg ruchów jest zakodowany jako pojedyncza liczba całkowita. Każdy ruch zajmuje 2 bity, a cała sekwencja jest konkatenacją tych dwubitowych wartości.

-- Aby uniknąć niejednoznaczności związanej z wiodącymi zerami, na początku zapisu binarnego (jako najbardziej znaczący bit) dopisana jest dodatkowa jedynka.

-- Przykład: Ciąg ruchów: 0, 1, 3, 3, 3, 1, 1, 2, 0, 0, 1

-- Binarnie:

-- Znacznik początku: 1
-- 0 → 00
-- 1 → 01
-- 3 → 11
-- 3 → 11
-- 3 → 11
-- 1 → 01
-- 1 → 01
-- 2 → 10
-- 0 → 00
-- 0 → 00
-- 1 → 01
-- Konkatenacja:

-- binarnie: 10001111111010110000001
-- dziesiętnie: 4715905
-- Napisz funkcję w Haskellu:

-- type Position = (Int, Int)

-- decodeMoves :: Integer -> Position
-- Funkcja przyjmuje liczbę całkowitą reprezentującą zakodowany ciąg ruchów i zwraca krotkę (x, y) z końcową pozycją robota po wykonaniu wszystkich ruchów.

-- Przykład

-- ghci> decodeMoves 4715905
-- (2,-1)


-- Move	Direction	Change
-- 0	Left	(-1, 0)
-- 1	Up	(0, +1)
-- 2	Right	(+1, 0)
-- 3	Down	(0, -1)


type Position = (Int, Int)

sumPositions :: [(Int, Int)] -> (Int, Int)
sumPositions [] = (0, 0)
sumPositions ((dx, dy):rest) =
    let (x, y) = sumPositions rest
    in (dx + x, dy + y)

processPair :: [Int] -> [Position]
processPair (x:y:rest) = (getPosition x y) : processPair rest
processPair _ = []


getPosition :: Int -> Int -> Position
getPosition 0 0 = (1, 0) --left
getPosition 0 1 = (0, -1)
getPosition 1 0 =  (-1,0) --right
getPosition 1 1 = (0, 1)


toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = reverse (helper n)
  where
    helper 0 = []
    helper x = let (q, r) = x `divMod` 2 in r : helper q



main = do 
    print(toBinary 4715905 == [1,0,0,0,1,1,1,1,1,1,1,0,1,0,1,1,0,0,0,0,0,0,1])
    let x = toBinary 4715905
    let head:tail = x
    print (head)
    print (tail)
    print(processPair tail)
    let position = processPair tail
    print(sumPositions position)
