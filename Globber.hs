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
matchGlob pattern str = patternMatch (compilePattern pattern) str

-- Pattern compilation routines.

compilePattern :: GlobPattern -> Glob
compilePattern ('?':xs) = GAnyChar : compilePattern xs
compilePattern ('*':xs) = GAnyString : compilePattern xs
compilePattern ('\\':x:xs) = GLiteral x : compilePattern xs
compilePattern ('[':xs) =
  let (matchPart, rest) = splitMatchPart xs
  in GSetMatch (makeMatchSet matchPart) : compilePattern rest
compilePattern (x:xs) = GLiteral x : compilePattern xs
compilePattern [] = []

splitMatchPart :: GlobPattern -> (GlobPattern, GlobPattern)
splitMatchPart xs = let matchPart = extractMatchPart xs
                        rest = drop (length matchPart + 1) xs
                    in (matchPart, rest)

extractMatchPart :: GlobPattern -> GlobPattern
extractMatchPart ('\\':x:xs) = '\\' : x : extractMatchPart xs
extractMatchPart (']':_) = []
extractMatchPart (x:xs) = x : extractMatchPart xs
extractMatchPart [] = error "unmatched ] in match set"

makeMatchSet :: GlobPattern -> MatchSet
makeMatchSet ('\\':x:xs) = MLiteral x : makeMatchSet xs
makeMatchSet (x:'-':y:xs) = MRange x y : makeMatchSet xs
makeMatchSet (x:xs) = MLiteral x : makeMatchSet xs
makeMatchSet [] = []

-- Matching routines.

patternMatch :: Glob -> String -> Bool
patternMatch (GAnyString:xs) (y:ys) =
  (patternMatch xs (y:ys)) || (patternMatch (GAnyString:xs) ys) || (patternMatch xs ys)
patternMatch (GAnyChar:xs) (_:ys) = patternMatch xs ys
patternMatch (GLiteral x:xs) (y:ys) = (x == y) && patternMatch xs ys
patternMatch (GSetMatch matchSet:xs) (y:ys) = setMatch matchSet y && patternMatch xs ys
patternMatch [] [] = True
patternMatch _ _ = False

setMatch :: MatchSet -> Char -> Bool
setMatch set char = any setMatch' set
  where setMatch' (MLiteral lit) = char == lit
        setMatch' (MRange l r) = (char >= l) && (char <= r)
