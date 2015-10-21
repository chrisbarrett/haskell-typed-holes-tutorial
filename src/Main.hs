{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Monad
import           Prelude         hiding (and, not, or)
import           Test.QuickCheck

-- This module contains exercises on typed holes. To load this file, run:
--
-- > $ ghci ./Main.hs
--
--
-- = Overview
--
-- A hole is written as an underscore ('_'). When GHC encounters a hole, it will
-- emit a compiler error, along with extra information about the context around
-- the hole. This information includes the type of the term you need to use to
-- fill the hole, as well as lexical bindings (functions and local variables in
-- scope).
--
-- = Instructions
--
-- Below are unimplemented functions and "QuickCheck" properties that test them.
--
-- - Work your way through one function at a time by replacing each 'undefined'
--   with a hole (underscore).
--
-- - GHC will provide extra information for type holes that will guide you
--   towards a correct implementation.
--
-- - You will know you have provided a valid implementation when the associated
--   QuickCheck properties pass.
--
--
-- = 1. Basic functions
--
-- As a warm-up, let's implement one of the basic functions in the Prelude.
-- Replace 'undefined' with a type hole (underscore) to get started!

id' :: a -> a
id' = undefined

prop_identity x = id' x == x

-- Once you insert the type hole and load the file in GHCI, you will see a type
-- error with some extra information. Here's what I get in GHC 7.8, cleaned up a
-- bit:
--
--    src/Main.hs:63:7: Found hole ‘_’ with type: a -> a …
--        Relevant bindings include
--          id' :: a -> a
--
-- GHC says it's expecting a term with type 'a -> a' where the hole is. That's a
-- function type. This makes sense, since we're implementing a function.
--
-- There's a useful section in the output containing 'relevant bindings', which
-- are the names and types of all the bindings in scope around the hole.
--
--        Relevant bindings include
--          id' :: a -> a
--
-- The only thing in scope right now is `id'`, which is the function we're
-- trying to define.
--
--   Aside:
--
--   `id'` appears in the list of relevant bindings because Haskell allows us to
--   call our functions recursively. Since it has the exact type of the hole
--   we're trying to fill, it's tempting to insert `id'` to solve the hole:
--
--   > id' = id'  -- this typechecks!
--
--   This looks pretty suspicious! How can this computation do anything useful?
--   Try running this in GHCI; you will produce an infinite loop at runtime
--   because there's nothing to stop the recursion. Such a computation is said
--   to _diverge_.
--
--   Some languages (Coq, Agda, Idris) reject such unsafe recursion during
--   typechecking and require you to prove that your recursive functions do
--   indeed terminate. Haskell imposes no such restriction, allowing you to
--   write infinite loops (intentionally or accidentally). In subsequent
--   sections we'll see exactly how to reason about the safety of recursive
--   functions.
--
--   End aside.
--
-- GHC has told us it needs a function in place of the hole. Replace the hole
-- with a lambda function to satisfy this, and write a new hole as its body:
--
-- > id' = \x -> _
--
-- Reload the file. We've made progress! GHC Tells us the new hole has a different type:
--
--     src/Main.hs:45:13: Found hole ‘_’ with type: a …
--
-- We've eliminated the function type, reducing it to a term of type `a`. Now
-- look at the relevant bindings section:
--
--     Relevant bindings include
--       x :: a
--       id' :: a -> a
--
-- The lambda has introduced a new binding for a term of type `a`, which is
-- precisely what we need to fill the hole. Replace the hole with `x` and
-- reload:
--
-- > id' = \x -> x
--
-- GHC accepts the definition! Run the tests from ghci. You should see the
-- 'prop_identity' test now succeeds:
--
--    >>> quickCheck prop_identity
--
--    === prop_identity from /Users/chrisb/Documents/typed-holes-tutorial/src/Main.hs:109 ===
--    +++ OK, passed 100 tests.
--
-- One final touch-up; we can simplify our functions a little bit by moving any
-- variables bound by the outermost lambda to the left of the `=` sign. I.e.,
--
-- > id' = \x -> x
--
-- is equivalent to the following:
--
-- > id' x = x
--
-- Generally we prefer to use this simpler form, but you will see explicit
-- lambdas in function definitions from time to time.
--
--
-- Exercise:
--
-- Provide implementations for the remaining functions in this section, using
-- the QuickCheck properties to check that your implementations are correct.


const' :: a -> b -> a
const' = undefined

prop_const x y = const' x y == x


-- |This is called ($) in the prelude.
apply :: (a -> b) -> a -> b
apply = undefined

prop_app x = f `apply` x == f x
  where f = (*2)


-- |This is called (.) in the prelude.
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose = undefined

prop_compose x = (f `compose` g) x == f (g x)
  where f = (*2)
        g = (+1)


-- = 2. Boolean algebra
--
-- This section introduces proofs requiring _pattern matching_. Functions will
-- now depend on the information contained in their arguments--in the case of
-- Bools, whether they are a True or False value. We use pattern matching to
-- recover this information.
--
-- Here is the definition of 'Bool', with its constructors 'True' and 'False':
--
-- data Bool = True | False
--
-- Look at the following function and property as an example:
--
-- > mystery :: Bool -> Bool -> Bool
-- > mystery = undefined
-- >
-- > prop_mystery x y = mystery x y == (x /= y)
--
-- 'mystery' is required to return true iff both Boolean arguments are unequal.
-- One way to implement this function is to pattern match on the arguments.
--
-- > mystery True False = True
-- > mystery False True = True
-- > mystery _    _     = False
--
-- Given the type and property definition above, it turns out 'mystery' has to
-- be the _exclusive or_ from boolean logic:
--
--   http://en.wikipedia.org/wiki/Exclusive_or
--
-- Complete the implementation of 'xor' using pattern matching, then complete
-- the subsequent functions.

xor :: Bool -> Bool -> Bool
xor = undefined

prop_xor x y =  x `xor` y == (x /= y)


not :: Bool -> Bool
not = undefined

prop_bool_not_inequality b = not b /= b
prop_bool_not_involutive b = not (not b) == b


or :: Bool -> Bool -> Bool
or = undefined

and :: Bool -> Bool -> Bool
and = undefined

prop_bool_deMorgan_1 x y = (not x) `and` (not y) == not (x `or` y)
prop_bool_deMorgan_2 x y = (not x) `or` (not y) == not (x `and` y)



-- * Natural Numbers

data Nat = Succ Nat | Zero
  deriving (Eq)

instance Show Nat where
  show = show . natToInt

natToInt :: Nat -> Int
natToInt Zero     = 0
natToInt (Succ n) = 1 + natToInt n


intToNat :: Int -> Nat
intToNat = undefined

prop_natToInt_Z = natToInt Zero == 0
prop_natToInt_intToNat n = n >= 0 ==> -- If n >= 0, this property will hold.
                           n == natToInt (intToNat n)

plus :: Nat -> Nat -> Nat
plus = undefined

mult :: Nat -> Nat -> Nat
mult = undefined

prop_plus_mult n = x `plus` x == x `mult` two
  where x = intToNat n
        two = Succ (Succ Zero)

-- * Maybe
--
-- data Maybe a = Just a | Nothing

fromMaybe :: a -> Maybe a -> a
fromMaybe = undefined

maybe :: b -> (a -> b) -> Maybe a -> b
maybe = undefined

-- * List


--------------------------------------------------------------------------------

return [] -- HACK: Needed for Template Haskell expansion on GHC 7.8
main = void $quickCheckAll
