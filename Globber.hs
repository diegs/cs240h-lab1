module Globber (matchGlob) where

type GlobPattern = String

type Glob = [GlobItem]

data GlobItem = GLiteral Char
              | GAnyChar
              | GAnyString
              | GSetMatch MatchSet
              deriving Show

type MatchSet = [MatchItem]

data MatchItem = MRange Char Char
               | MLiteral Char
               deriving Show

matchGlob :: GlobPattern -> String -> Bool
matchGlob rawPattern str =
  let pattern = compilePattern rawPattern
  in glob pattern str

glob :: Glob -> String -> Bool
glob (GAnyString:xs) (y:ys) = (glob xs (y:ys)) || (glob (GAnyString:xs) ys)
glob (GAnyChar:xs) (_:ys) = glob xs ys
glob (GLiteral x:xs) (y:ys) = (x == y) && glob xs ys
glob (GSetMatch matchSet:xs) (y:ys) = setMatch matchSet y && glob xs ys
glob [] [] = True
glob _ _ = False

setMatch :: MatchSet -> Char -> Bool
setMatch matchSet char = any setMatch' matchSet
  where setMatch' (MLiteral lit) = char == lit
        setMatch' (MRange l r) = (char >= l) && (char <= r)

compilePattern :: GlobPattern -> Glob
compilePattern ('?':xs) = GAnyChar : compilePattern xs
compilePattern ('*':xs) = GAnyString : compilePattern xs
compilePattern ('\\':x:xs) = GLiteral x : compilePattern xs
compilePattern ('[':xs) =
  let (matchPart, rest) = splitMatchPart xs
  in GSetMatch (matchSet matchPart) : compilePattern rest
compilePattern (x:xs) = GLiteral x : compilePattern xs
compilePattern [] = []

splitMatchPart :: GlobPattern -> (GlobPattern, GlobPattern)
splitMatchPart xs = let matchPart = extractMatchPart xs
                        rest = drop (length matchPart + 1) xs
                    in (matchPart, rest)

extractMatchPart :: GlobPattern -> GlobPattern
extractMatchPart ('\\':x:xs) = '\\' : x : extractMatchPart xs
extractMatchPart (']':xs) = []
extractMatchPart (x:xs) = x : extractMatchPart xs
extractMatchPart [] = error "unmatched [] in match set"

matchSet :: GlobPattern -> MatchSet
matchSet ('\\':x:xs) = MLiteral x : matchSet xs
matchSet (x:'-':y:xs) = MRange x y : matchSet xs
matchSet (x:xs) = MLiteral x : matchSet xs
matchSet [] = []
