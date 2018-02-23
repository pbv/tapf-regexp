{-|
  A simple regular expression matcher using continuations

  Based on Oliver Danvy's "Defunctionalization at work"
  BRICS tech report  RS-01-23, 2001

  Pedro Vasconcelos, 2017
-}
module TAPF.Regexp (
  Regexp,      -- type for regexps
  match,       -- matching function
  many,        -- repetition (Kleene's closure)
  string,      -- conversion from string 
  (<+>),       -- union
  (<>)         -- concatenation
  ) where

-- | data type for regular expressions
data Regexp
  = Zero                  -- empty set
  | One                   -- empty word
  | Lit Char              -- single character
  | Plus Regexp Regexp    -- union 
  | Cat  Regexp Regexp    -- concatenation 
  | Many Regexp           -- repetition ("Kleene star")
  deriving Show

-- | infix operators 
infixl 6 <+>
infixl 7 <>

-- | union of regular expressions
(<+>) :: Regexp -> Regexp -> Regexp
(<+>) = Plus

-- | concatenation of regular expressions
(<>) :: Regexp -> Regexp -> Regexp
(<>) = Cat
  
-- | convertion from a string literal into a regexp
string :: [Char] -> Regexp
string ""  = One
string xs = foldr1 Cat (map Lit xs)

-- | zero or more repetition (i.e. Kleene's closure)
many :: Regexp -> Regexp
many = Many

-- | top-level matching function
match :: Regexp -> String -> Bool
match re str = matchAux re str null

-- type synonym for continuations
type Cont = String -> Bool

-- | auxiliary worker function
matchAux :: Regexp -> String -> Cont -> Bool
matchAux (Lit c) str k
  = case str of
      []     -> False
      (x:xs) -> x==c && k xs
matchAux Zero str k
  = False
matchAux One  str k
  = k str
matchAux (Cat e1 e2) str k
  = matchAux e1 str (\str' -> matchAux e2 str' k)
matchAux (Plus e1 e2) str k
  = matchAux e1 str k || matchAux e2 str k
matchAux (Many e) str k
  = matchMany e str k

-- matching zero or more repetions of `e'
-- accept the empty string or accept `e' and repeat;
-- to ensure termination, the continuation
-- checks if some input was consumed
matchMany e str k
  = k str ||
    matchAux e str (\str' -> str'/=str && matchMany e str' k)



