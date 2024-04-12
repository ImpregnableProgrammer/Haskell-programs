module Main where

import Data.Data (Data)
import qualified Data.List as L

-- GREAT Haskell reference guide: https://en.wikibooks.org/wiki/Haskell
-- Hoogle for looking up Haskell packages: https://hoogle.haskell.org/
-- >>> 2+3
-- 5

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
mainDo = do {
  putStrLn "Give some input: ";
  s <- getLine;
  putStrLn s
}

-- Use `fmap` to apply a monadic function to a monadic type
-- `List` is another monadic type
-- All a monad needs in a a way to unwrap and then squash down wrapped monadic values
