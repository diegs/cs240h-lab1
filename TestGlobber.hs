module Main (main) where

import Test.Hspec

import Globber

main :: IO ()
main = hspec $ describe "Testing Globber" $ do

    describe "empty pattern" $ do
      it "matches empty string" $
        matchGlob "" "" `shouldBe` True
      it "shouldn't match non-empty string" $
        matchGlob "" "string" `shouldBe` False

    describe "glob pattern" $ do
      it "? matches any single character" $
        matchGlob "a?c" "abc" `shouldBe` True
      it "? should not match two characters" $
        matchGlob "?" "ab" `shouldBe` False
      it "* matches any string" $
        matchGlob "*" "abc" `shouldBe` True
      it "* matches the empty string" $
        matchGlob "*" "" `shouldBe` True
      it "a* matches a" $
        matchGlob "a*" "a" `shouldBe` True
      it "a*b*c matches aabbcc" $
        matchGlob "a*b*c" "aabbcc" `shouldBe` True

    describe "literal matcher" $ do
      it "abcde matches abcde" $
        matchGlob "abcde" "abcde" `shouldBe` True
      it "ab doesn't match aa" $
        matchGlob "ab" "aa" `shouldBe` False
      it "a doesn't match empty string" $
        matchGlob "a" "" `shouldBe` False
      it "a]b matches a]b" $
        matchGlob "a]b" "a]b" `shouldBe` True
      it "\\a\\b\\c\\d\\e matches abcde" $
        matchGlob "\\a\\b\\c\\d\\e" "abcde" `shouldBe` True
      it "-adf]ai1 matches -adf]ai1" $
        matchGlob "-adf]ai1" "-adf]ai1" `shouldBe` True

    describe "escaped literal" $ do
      it "matches [a]" $
        matchGlob "\\[a]" "[a]" `shouldBe` True
      it "matches **?" $
        matchGlob "\\*\\*\\?" "**?" `shouldBe` True
      it "matches \\a\\" $
        matchGlob "\\\\a\\\\" "\\a\\" `shouldBe` True
      it "matches ab*ba" $
        matchGlob "ab\\*ba" "ab*ba" `shouldBe` True
      it "matches ab[ba" $
        matchGlob "ab\\[ba" "ab[ba" `shouldBe` True
      it "ab[a\\]]ba matches" $ do
        matchGlob "ab[a\\]]ba" "ab]ba" `shouldBe` True
        matchGlob "ab[a\\]]ba" "ababa" `shouldBe` True

    describe "set match" $ do
      it "[-abc] matches -,a,b,c" $ do
        matchGlob "[-abc]" "-" `shouldBe` True
        matchGlob "[-abc]" "a" `shouldBe` True
        matchGlob "[-abc]" "b" `shouldBe` True
        matchGlob "[-abc]" "c" `shouldBe` True
        matchGlob "[-abc]" "z" `shouldBe` False
        matchGlob "[-abc]" "" `shouldBe` False
      it "[abc-] matches -,a,b,c" $ do
        matchGlob "[abc-]" "-" `shouldBe` True
        matchGlob "[abc-]" "a" `shouldBe` True
        matchGlob "[abc-]" "b" `shouldBe` True
        matchGlob "[abc-]" "c" `shouldBe` True
        matchGlob "[abc-]" "z" `shouldBe` False
        matchGlob "[abc-]" "" `shouldBe` False
      it "[--] matches a literal - character" $ do
        matchGlob "[--]" "-" `shouldBe` True
        matchGlob "[--]" "a" `shouldBe` False
      it "[---] is a range but of only one character, -" $ do
        matchGlob "[---]" "-" `shouldBe` True
        matchGlob "[---]" "a" `shouldBe` False
      it "[----] is a range of just - and a literal -" $ do
        matchGlob "[----]" "-" `shouldBe` True
        matchGlob "[----]" "a" `shouldBe` False
      it "[a-d-z] matches a,b,c,d,-,z" $ do 
        matchGlob "[a-d-z]" "a" `shouldBe` True
        matchGlob "[a-d-z]" "b" `shouldBe` True
        matchGlob "[a-d-z]" "c" `shouldBe` True
        matchGlob "[a-d-z]" "d" `shouldBe` True
        matchGlob "[a-d-z]" "-" `shouldBe` True
        matchGlob "[a-d-z]" "z" `shouldBe` True
        matchGlob "[a-d-z]" "A" `shouldBe` False
        matchGlob "[a-d-z]" "" `shouldBe` False
      it "[z-a] is an empty range and mathces nothing" $ do
        matchGlob "[z-a]" "a" `shouldBe` False
        matchGlob "[z-a]" "e" `shouldBe` False
        matchGlob "[z-a]" "z" `shouldBe` False
        matchGlob "[z-a]" "C" `shouldBe` False
        matchGlob "[z-a]" "" `shouldBe` False
