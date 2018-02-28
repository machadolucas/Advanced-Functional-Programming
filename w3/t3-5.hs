-- Make a typeclass for partial order. It needs to have a "<=="  function for partial  order comparision, and to
--  be a member of the typeclass, one needs to implement that function. For strings, e.g., there is a partial order
--  by one being a substring of another (for some strings we know that one is a substring of the other). Make strings
--  a member of that typeclass. Since partial order is not defined for all pairs of elements, the result of "<=="
--  could be something like true, false or undefined, so you may define such modified boolean data type as well (but
--  should  use other names than True and False).
--EDIT: There is no point to make string itself a member of the typeclass as it should be done for lists, ie type [a].

import Data.List

data Newlean = Truee | Falsee | Undefined deriving (Show)

-- Here I had to use a wrapper because Haskell was complaining about using String as instance of a typeclass:
--  https://stackoverflow.com/questions/5941701/why-can-i-not-make-string-an-instance-of-a-typeclass
newtype Wrapper = Wrapper String

class PartOrder a where
  (<==) :: a -> a -> Newlean

instance PartOrder Wrapper where
  (Wrapper x) <== (Wrapper y)
     | length x < 1 || length y < 1 = Undefined
     | isInfixOf x y = Truee
     | otherwise = Falsee

-- Examples:
-- (Wrapper "ab") <== (Wrapper "aaabbbb")
-- Truee

-- (Wrapper "abc") <== (Wrapper "aaabbbb")
-- Falsee

-- (Wrapper "") <== (Wrapper "")
-- Undefined
