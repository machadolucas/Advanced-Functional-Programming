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
