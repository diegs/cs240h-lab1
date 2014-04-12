module Globber (matchGlob) where

type GlobPattern = String
type Glob = [GlobItem]
data GlobItem = Literal Char
              | AnyChar
              | AnyString
              | SetMatch MatchSet
              deriving Show
type MatchSet = [MatchItem]
data MatchItem = Range Char Char
               | Literal Char

matchGlob :: GlobPattern -> String -> Bool
matchGlob rawPattern str =
  let pattern = compilePattern pattern in
  glob pattern str

glob :: Glob -> String -> Bool
glob (AnyString:xs) _ = True
glob (AnyChar:xs) (_:ys) = glob xs ys
glob (Literal x:xs) (y:ys) = (x == y) && glob xs ys
glob (SetMatch matchSet:xs) (y:ys) = setMatch matchSet y && glob xs ys
glob [] [] = True
glob _ _ = False

setMatch :: MatchSet -> Char -> Bool
setMatch matchSet char = True

compilePattern :: GlobPattern -> Glob
compilePattern ('?':xs) = AnyChar : compilePattern xs
compilePattern ('*':xs) = AnyString : compilePattern xs
compilePattern ('\\':x:xs) = Literal x : compilePattern xs
compilePattern ('[':xs) = matchSet xs
compilePattern (x:xs) = Literal x : compilePattern xs
compilePattern [] = []

matchSet :: GlobPattern -> Glob
matchSet ('\':x:xs) = Literal x : matchSet xs
matchSet (']':xs) = compilePattern xs
matchSet (x:'-':y:xs) = Range x y : matchSet xs
matchSet (x:xs) = Literal x : matchSet xs
matchSet [] = []
