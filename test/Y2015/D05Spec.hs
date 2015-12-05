module Y2015.D05Spec (spec) where

import Y2015.D05

import Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "Day 5" $ do
        describe "thriceVoweled" $ do
            it "validates vowels only" $
                thriceVoweled "aei" `shouldBe` True
            it "validtes three vowels interspersed" $
                thriceVoweled "xazegov" `shouldBe` True
            it "validates a long string of only vowels" $
                thriceVoweled "aeiouaeiouaeiou" `shouldBe` True
        describe "twiceRow" $ do
            it "validates tuples" $
                twiceRow "xx" `shouldBe` True
            it "validtes three vowels interspersed" $
                twiceRow "abcdde" `shouldBe` True
            it "validates a long string of only vowels" $
                twiceRow "aabbccdd" `shouldBe` True
        describe "isNice" $ do
            it "validates three vowels" $
                isNice "ugknbfddgicrmopn" `shouldBe` True
            it "validates three vowels and double letters" $
                isNice "aaa" `shouldBe` True
            it "invalidates lack of double letters" $
                isNice "jchzalrnumimnmhp" `shouldBe` False
            it "invalidates forbidden substrings" $
                isNice "haegwjzuvuyypxyu" `shouldBe` False
            it "invalidates strings with a single vowel" $
                isNice "dvszwmarrgswjxmb" `shouldBe` False
            it "validates complicated cases" $
                isNice "iuvrelxiapllaxbg" `shouldBe` True
        describe "isNicer" $ do
            it "validates pairs and exactly one letter of separation" $
                isNicer "qjhvhtzxzqqjkmpb" `shouldBe` True
            it "validates with rule overlap" $
                isNicer "xxyxx" `shouldBe` True
            it "invalidates without repeated sequences" $
                isNicer "uurcxstgmygtbstg" `shouldBe` False
            it "invalidates without pairs" $
                isNicer "ieodomkazucvgmuy" `shouldBe` False
