module Main where

import Data.Char (toUpper)
import Data.Data (Data)
import Data.List qualified as L
import Text.Read (Lexeme (String))

-- GREAT Haskell reference guide: https://en.wikibooks.org/wiki/Haskell
-- Hoogle for looking up Haskell packages: https://hoogle.haskell.org/
-- >>> 2+9
-- 11

-- Every compiled Haskell program executes the `main` method if it's defined
-- which is expected to have an `IO` monadic return type
-- `IO ()` means IO with `void` return type
-- IO String means a `String` is ultimately returned, wrapped in a monad
-- The IO monad: https://en.wikibooks.org/wiki/Haskell/Understanding_monads/IO
-- GREAT monad reference: https://en.wikibooks.org/wiki/Haskell/Understanding_monads
main :: IO ()
-- Chain together inputs from one command (with possible side-effects) to
-- another with the bind (m >>= g) operator
-- and chain together unrelated commands (as if separated by semicolons in imperative languages)
-- using the "then" (>>) operator, until you finally end with `return`
-- (which wraps the given input in a monad)
-- Think of monads as nice generalized boxes/wrappers around any type
-- that can be conveniently unwrapped and operated on!
main = putStrLn "Give some input: " >> getLine >>= putStrLn

-- The `do` construct as follows is equivilant to the above
-- Semicolons (or newlines) are equivilant to `>>`
mainDo :: IO ()
mainDo = do
  putStrLn "Give some input: "
  s <- getLine
  putStrLn s

-- Use `fmap` to apply a monadic function to a monadic type
-- `List` is another monadic type
-- All a monad needs in a a way to unwrap and then squash down wrapped monadic values
-- Monads can be used to handle side effects (even errors) in a pure, functional way!
-- More references: https://wiki.haskell.org/All_About_Monads
-- https://www.haskell.org/tutorial/monads.html\

{-
  Longest common subsequence problem: given two strings s1, s2, return the longest common
  subsequence between them
  The subsequences need not be contiguous!! But they must be in the same order!
  Dynamic programming problem -- demonstrates optimal substructure!
  The subsequences of longest commen subsequence must also be the longest!
  Recurrence relation:
  A[i][j] = 1 + A[i-1][j-1] if s1[i] == s2[j] else max(A[i][j-1], A[i-1][j])
  A[i][0] = A[0][j] = 0
  adfeg ardfig -> adfg
  aedfiou lipiaedf -> aedf
-}

-- Helper method: return string of maximum length in the list
-- NOT tail recursive
-- >>> strMaxLen ["hi", "heyomyguy", "okay"]
-- "heyomyguy"
strMaxLen :: [String] -> String
strMaxLen [s] = s
strMaxLen (str : strs) =
  let s = strMaxLen strs
   in if length str > length s then str else s

-- Implementation
-- >>> longestComSubseq "adfeg" "ardfig" == "adfg"
longestComSubseq :: String -> String -> String
longestComSubseq [] _ = [] -- Return empty if either input is empty
longestComSubseq _ [] = []
longestComSubseq (c1 : s1) (c2 : s2) -- Enumerate throuigh different combinations of the input strings
  | c1 == c2 = c1 : longestComSubseq s1 s2
  | otherwise = strMaxLen [longestComSubseq s1 (c2 : s2), longestComSubseq (c1 : s1) s2]

-- list comprehension in Haskell
-- Use list comprehension to return numbers in list divisible by 5
-- http://zvon.org/other/haskell/Outputsyntax/listQcomprehension_reference.html
-- https://en.wikibooks.org/wiki/Haskell/Lists_III#List_comprehensions
-- >>> getOnlyFiveDiv [1,2,3,4,5,6,7,8,9,10]
getOnlyFiveDiv :: [Int] -> [Int]
getOnlyFiveDiv ints = [n | n <- ints, n `mod` 5 == 0]

-- Now: return tails of sublists where the first element is greater than 5
-- and the last element is divisible by 2
-- >>> getTailsWithTwoDiv [[4,3,2,1,2], [6,7,8,9,3], [9,10,8,2,4]]
-- [[10,8,2,4]]
getTailsWithTwoDiv :: [[Int]] -> [[Int]]
getTailsWithTwoDiv lists = [tail n | n <- lists, head n > 5, even (last n)]

-- Define custom operators in Haskell!
-- Returns whether two ints are not equal
-- >>> 2 >=< 3
-- >>> 7 >=< 7
-- True
-- False
(>=<) :: Int -> Int -> Bool
(>=<) a b = a /= b

-- Haskell classes and instantiating them with instances!
data List = List Int List | Null deriving (Show)

-- https://markkarpov.com/tutorial/generics#the-shape-of-a-data-type
data (f :+: g) p = L1 (f p) | R1 (g p) deriving (Show)

-- A typeclass definition
-- Any data type that instantiates this typeclass must implement the methods defined
-- then you can invoke the enclosed method on the instantiating type!
class Compare a where
  compare :: a -> a -> Bool

instance Compare Int where
  compare :: Int -> Int -> Bool
  compare a b = a == b

instance Compare [Int] where
  compare :: [Int] -> [Int] -> Bool
  compare [] [] = True
  compare (i1 : i1s) (i2 : i2s)
    | i1 /= i2 = False
    | otherwise = Main.compare i1s i2s

-- split string on a (possibly multi) character delimiter
-- >>> split "," "Hello,world,kinda,okay"
-- ["Hello","world","kinda","okay"]
split :: String -> String -> [String]
split _ [] = []
split d s = helper [] "" "" s
  where
    helper :: [String] -> String -> String -> String -> [String]
    helper strs a dtemp ""
      | d == dtemp = strs ++ [a]
      | otherwise = strs ++ [a ++ dtemp]
    helper strs a dtemp (c : s)
      | d == dtemp = helper (strs ++ [a]) "" [c] s
      | length d == length dtemp = helper strs (a ++ [head dtemp]) (tail dtemp ++ [c]) s
      | otherwise = helper strs a (dtemp ++ [c]) s

-- CodeWars challenge: https://www.codewars.com/kata/517abf86da9663f1d2000003/train/haskell
-- >>> toCamelCase "hello-there"
toCamelCase :: String -> String
toCamelCase "" = ""
toCamelCase (c : str) = helper c str
  where
    helper :: Char -> String -> String
    helper c "" = [c]
    helper prev (c : str) = if prev `elem` "-_" then helper (toUpper c) str else prev : helper c str

-- create phone number challenge: https://www.codewars.com/kata/525f50e3b73515a6db000b83/train/haskell
-- >>> createPhoneNumber [1,2,3,4,5,6,7,8,9,0]
createPhoneNumber :: [Int] -> String
createPhoneNumber ints = helper "" "" "" ints
  where
    helper :: String -> String -> String -> [Int] -> String
    helper area a b [] = "(" ++ area ++ ")" ++ " " ++ a ++ "-" ++ b
    helper area a b (i:ints) 
      | length ints > 6 = helper (area ++ show i) a b ints
      | length ints > 3 = helper area (a ++ show i) b ints
      | otherwise = helper area a (b ++ show i) ints

