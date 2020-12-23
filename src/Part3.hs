module Part3 where

import Data.List (group, nub, sort, find)
import Data.Bool (bool)


------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 n = getPrimeDivisors n == [n]

-- Получить все простые делители числа.
getPrimeDivisors :: Integer -> [Integer]
getPrimeDivisors = getDivisorsWithCurrent 2
    where
        getDivisorsWithCurrent :: Integer -> Integer -> [Integer]
        getDivisorsWithCurrent _ 1 = []
        getDivisorsWithCurrent divisor number
            | divisor * divisor > number = [number]
            | number `mod` divisor == 0 = divisor : getDivisorsWithCurrent divisor (number `div` divisor)
            | otherwise = getDivisorsWithCurrent (divisor + 1) number

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
prob19 number = map (\divisors -> (head divisors, length divisors)) groupEqualDivisors
    where
        groupEqualDivisors :: [[Integer]]
        groupEqualDivisors = group (getPrimeDivisors number)

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20 :: Integer -> Bool
prob20 number = sum (getUnorderedDivisors number) == number

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21 :: Integer -> [Integer]
prob21 number = (sort . getUnorderedDivisors) number ++ [number]

-- Получить все делители числа, кроме самого числа.
getUnorderedDivisors :: Integral a => a -> [a]
getUnorderedDivisors number = (leftPart++)
    $ nub
    $ concat [ [x, number `div` x] | x <- [2..limit], number `rem` x == 0 ]
    where
        limit = (floor . sqrt . fromIntegral) number
        leftPart = if number == 1 then [] else [1]

------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 []    = 0
prob22 input = product $ (map lettersCount) (words input)
    where
        lettersCount :: String -> Integer
        lettersCount word = toInteger $ length (filter (=='i') word)

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 inputString = return inputString >>= parseInput >>= getSlice
    where
        parseInput :: String -> Maybe ParseResult
        parseInput input = do
            let left = read $ takeWhile (/= '-') input
            let right = read $ takeWhile (/= ':') $ tail $ dropWhile (/= '-') input
            let string = tail $ dropWhile (/= ' ') input
            return ParseResult { leftBound = left, rightBound = right, stringToSlice = string }

        getSlice :: ParseResult -> Maybe String
        getSlice (ParseResult left right string)
            | left > length string || right > length string = Nothing
            | right >= left = Just $ leftToRightSlice left right
            | otherwise = Just $ reverse $ leftToRightSlice right left
            where
                leftToRightSlice :: Int -> Int -> String
                leftToRightSlice l r = take r $ drop (l - 1) string

data ParseResult = ParseResult
    {
        leftBound :: Int,
        rightBound :: Int,
        stringToSlice :: String
    }

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 number = iterateTriangular 1 0
    where
        iterateTriangular :: Integer -> Integer -> Bool
        iterateTriangular currentNum currentSum
            | currentSum == number = True
            | currentSum > number = False
            | otherwise = iterateTriangular (succ currentNum) (currentSum + currentNum)

------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 number = getDigits number == (reverse . getDigits) number
    where
        getDigits :: Integer -> [Integer]
        getDigits 0 = [0]
        getDigits current = digitsInternal current
            where
                digitsInternal 0 = []
                digitsInternal x = x `mod` 10 : digitsInternal (x `div` 10)

------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 left right = sumDivisors left == right && sumDivisors right == left
    where sumDivisors = sum . getUnorderedDivisors

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 _ [] = Nothing
prob27 requiredSum (curHead : curTail) = withFixedCurrent curHead curTail
    where
        withFixedCurrent :: Int -> [Int] -> Maybe (Int, Int)
        withFixedCurrent _ [] = prob27 requiredSum curTail
        withFixedCurrent current (innerHead : innerTail) =
            if current + innerHead == requiredSum
            then Just (current, innerHead)
            else withFixedCurrent current innerTail

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
--prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
--prob28 requiredSum inputList = do
--    list <- find
--            (\list -> sum list == requiredSum)
--            $ subsets 4 inputList
--    return (list !! 3, list !! 2, list !! 1, list !! 0)
--    where
--        subsets :: Int -> [a] -> [[a]]
--        subsets subLength listToHandle
--            | subLength > length listToHandle = []
--            | otherwise = subsequencesBySize listToHandle !! (length listToHandle - subLength)
--
--        subsequencesBySize :: [a] -> [[[a]]]
--        subsequencesBySize [] = [[[]]]
--        subsequencesBySize (curHead : curTail) =
--            let next = subsequencesBySize curTail
--            in zipWith (++) ([] : next) $  map (map (curHead :)) next ++ [[]]

prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 _ [] = Nothing
prob28 requiredSum list = case withFixedCurrent requiredSum list of
    (toTuple : _) -> Just (toTuple !! 3, toTuple !! 2, toTuple !! 1, toTuple !! 0)
    [] -> Nothing

withFixedCurrent :: Int -> [Int] -> [[Int]]
withFixedCurrent _ [] = []
withFixedCurrent sumOf curList@(_ : curTail) = withCurrentSum 4 sumOf curList
    where

        withCurrentSum :: Int -> Int -> [Int] -> [[Int]]
        withCurrentSum _ _ [] = []
        withCurrentSum 1 innerSum inputList = (map pure) $ filter (== innerSum) inputList
        withCurrentSum _ _ (_ : [])        = withFixedCurrent sumOf curTail

        withCurrentSum subLength innerSum (innerHead : innerTail) =
            map
                (innerHead :)
                $ withCurrentSum (pred subLength) (innerSum - innerHead) innerTail

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 kLength = maximum [(x * y) |
    x <- [minByLength .. maxByLength],
    y <- [minByLength .. maxByLength],
    (prob25 . toInteger) (x * y)]
    where
        minByLength = 10 ^ (kLength - 1)
        maxByLength = 10 ^ kLength - 1

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 reqCount = head $
    filter (\triangular -> (succ . length . getUnorderedDivisors) triangular >= reqCount)
    generateTriangular
    where
        generateTriangular :: [Integer]
        generateTriangular = triangularWithCurrent 0 1
            where
                triangularWithCurrent :: Integer -> Integer -> [Integer]
                triangularWithCurrent current next = current : triangularWithCurrent (current + next) (succ next)

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 maxValue = sum $ map (\(left, right) -> left + right) amicablePairs
    where 
        amicablePairs = concat $ map getAmicableItem [1 .. pred maxValue]
        getAmicableItem currentItem = 
            let amicableItem = divisorsSum currentItem
            in bool
               [] 
               [(currentItem, amicableItem)] 
               (currentItem < amicableItem && currentItem == divisorsSum amicableItem && amicableItem < maxValue)
        divisorsSum = sum . getUnorderedDivisors

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 coins moneySum
    | moneySum < minimum coins = []
    | otherwise = [coin : nextCoins |
        coin <- reverse coins,
        nextCoins <- [] : prob32 (filter (<= coin) coins) (moneySum - coin),
        sum (coin : nextCoins) == moneySum]