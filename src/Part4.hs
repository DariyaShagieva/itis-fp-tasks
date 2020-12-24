{-# LANGUAGE InstanceSigs #-}

module Part4 where

-- | 8 tasks:
-- * Functor for Parser
-- * Applicative for Parser
-- * Monad for Parser
-- * Alternative for Parser
-- * Functor for Cont
-- * Applicative for Cont
-- * Monad for Cont
-- * ??? for Cont

import Part4.Types

import Control.Applicative
import Control.Monad (msum)
import Data.Maybe (maybeToList, fromJust)
import Data.Char (intToDigit, isSpace, isDigit)
import Text.Read (readMaybe)
import Data.List (isInfixOf, dropWhileEnd, elemIndex)

------------------------------------------------------------
-- PROBLEM #33
--
-- Написать экземпляр класса Functor для Parser
-- (удовлетворяющий законам)
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap func (Parser parseFunc) = Parser $ (map (\(str, input) -> (str, func input))) . parseFunc

------------------------------------------------------------
-- PROBLEM #34
--
-- Написать экземпляр класса Applicative для Parser
-- (удовлетворяющий законам)
instance Applicative Parser where
    pure :: a -> Parser a
    pure value = Parser $ \str -> [(str, value)]

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser leftFunc) <*> (Parser rightFunc) = Parser parserFunc
        where
            parserFunc input = do
                (leftString, funcToApply) <- (leftFunc input)
                (rightString, item) <- (rightFunc leftString)
                return (rightString, funcToApply item)

------------------------------------------------------------
-- PROBLEM #35
--
-- Написать экземпляр класса Alternative для Parser
-- (удовлетворяющий законам)
instance Alternative Parser where

    empty :: Parser a
    empty = Parser $ \_ -> []

    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser left) <|> (Parser right) = Parser $ \str -> msum $ map ($ str) [left, right]

------------------------------------------------------------
-- PROBLEM #36
--
-- Написать экземпляр класса Monad для Parser
-- (удовлетворяющий законам)
instance Monad Parser where

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser parserFunc) >>= funcToBind = Parser resultFunc
        where
            resultFunc input = do
                (stringRem, item) <- parserFunc input
                runParser (funcToBind item) stringRem

------------------------------------------------------------
-- PROBLEM #37
--
-- Написать экземпляр класса Functor для Foo
-- (удовлетворяющий законам)

instance Functor (Foo r) where

    fmap :: (a -> b) -> Foo r a -> Foo r b
    fmap = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #38
--
-- Написать экземпляр класса Applicative для Foo
-- (удовлетворяющий законам)
instance Applicative (Foo r) where

    pure :: a -> Foo r a
    pure = error "Implement me!"

    (<*>) :: Foo r (a -> b) -> Foo r a -> Foo r b
    (<*>) = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #39
--
-- Написать экземпляр класса Monad для Foo
-- (удовлетворяющий законам)
instance Monad (Foo r) where

    (>>=) :: Foo r a -> (a -> Foo r b) -> Foo r b
    (>>=) = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #40
--
-- Написать парсер для выражений вида
--
--   <var> := <number>
--
-- Здесь вместо <var> может быть произвольное имя
-- переменной (начинается со строчной буквы, состоит из
-- букв, цифр и знаков подчеркивания), вместо <number> -
-- произвольное целое число. Между именем переменной и
-- присваиванием ":=" и между присваиванием ":=" и числом
-- может быть нуль или больше пробелов
--
-- В качестве результата парсер должен вернуть пару
-- (имя переменной, присваиваемое число)
prob40 :: Parser (String, Integer)
prob40 = (,) <$> variableNameParser <*> variableValueParser

-- Парсер, вычленяющий имя переменной из выражения присвоения.
variableNameParser :: Parser String
variableNameParser = Parser parseFunc
    where
        parseFunc :: String -> [(String, String)]
        parseFunc assignmentExpr = do
            (nameInput, _) <- maybeToList $ trySplitByAssignmentOperator assignmentExpr
            True <- return $ isValidVariableName nameInput
            return (assignmentExpr, nameInput)

-- Является ли строка валидным именем переменной.
isValidVariableName :: String -> Bool
isValidVariableName [] = False
isValidVariableName (firstChar : nameTail) = elem firstChar ['a'..'z'] && all isValidChar nameTail
    where
        isValidChar :: Char -> Bool
        isValidChar = flip elem $ concat
            [
                ['a'..'z'],
                ['A'..'Z'],
                map intToDigit [0..9],
                ['_']
            ]

-- Парсер, вычленяющий значение переменной из выражения присвоения.
variableValueParser :: Parser Integer
variableValueParser = Parser parseFunc
    where
        parseFunc :: String -> [(String, Integer)]
        parseFunc assignmentExpr = do
            (_, numberInput) <- maybeToList $ trySplitByAssignmentOperator assignmentExpr

            -- Проверяем, что между ':=' и числом нет невалидных символов.
            [] <- return $ takeWhile (\char -> (not $ isDigit char) &&  char /= '-') numberInput
            -- Проверяем, что строковой вид числа не содержит больше одного минуса.
            True <- return $ (length $ filter (=='-') numberInput) <= 1

            return $ case readMaybe numberInput of
                Just validInteger -> ("", validInteger)
                Nothing -> (takeInvalidTail numberInput, 0)

-- Получить список невалидных символов, расположенных в конце выражения (справа от числа).
takeInvalidTail :: String -> String
takeInvalidTail = reverse . takeWhile (not . isDigit) . reverse

-- Разбить строку на две подстроки, расположенные
-- соответственно слева и справа от оператора присвоения ':='.
trySplitByAssignmentOperator :: String -> Maybe (String, String)
trySplitByAssignmentOperator input
    | hasSingleOperator = Just
        (
            trim $ takeWhile (/=':') input,
            trim $ drop (succ $ fromJust $ elemIndex '=' input) input
        )
    | otherwise = Nothing
    where
        hasSingleOperator :: Bool
        hasSingleOperator =
            isInfixOf ":=" input
            && single (==':') input
            && single (=='=') input

        trim :: String -> String
        trim = dropWhile isSpace . dropWhileEnd isSpace

        single :: (a -> Bool) -> [a] -> Bool
        single predicate list = length (filter predicate list) == 1