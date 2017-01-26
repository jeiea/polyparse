module Text.ParserCombinators.Poly.Plain
  ( -- * The Parser datatype
    Parser(P)  -- datatype, instance of: Functor, Monad, PolyParse
  , Result(..) -- internal to the Parser Monad.
  , runParser  -- :: Parser t a -> [t] -> (Either String a, [t])
    -- ** Basic parsers
  , next       -- :: Parser t t
  , eof        -- :: Parser t ()
  , satisfy    -- :: (t->Bool) -> Parser t t
  , satisfyMsg -- :: (t->Bool) -> String -> Parser t t
  , onFail     -- :: Parser t a -> Parser t a -> Parser t a
    -- ** Re-parsing
  , reparse  -- :: [t] -> Parser t ()
    -- ** Debug utilities
  , mark
  , wrap
    -- * Re-export all more general combinators
  , module Text.ParserCombinators.Poly.Base
  , module Control.Applicative
  ) where

import Text.ParserCombinators.Poly.Base
import Text.ParserCombinators.Poly.Result
import Text.ParserCombinators.Poly.Parser
import Control.Applicative
import Debug.Trace

-- The only differences between a Plain and a Lazy parser are the instance
-- of Applicative, and the type (and implementation) of runParser.

-- | Apply a parser to an input token sequence.
runParser :: Parser t a -> [t] -> (Either String a, [t])
runParser (P p) = resultToEither . p

------------------------------------------------------------------------

-- | Prints message before testing parser.
mark :: String -> Parser Char ()
mark s = P (\inp -> trace (s ++ ": " ++ inp) (Success inp ()))

-- | Prints message before testing parser and show result after tested.
wrap :: String -> Parser Char a -> Parser Char a
wrap s (P p) = P inner where
  inner inp = trace (s ++ " test: " ++ inp) $ trace (fmt res) res where
    res = p inp
    fmt x = case x of
      Success r _ -> s ++ " success: " ++ r
      Failure r _ -> s ++ " failure: " ++ r
      Committed w -> fmt w