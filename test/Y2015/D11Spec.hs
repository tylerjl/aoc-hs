module Y2015.D11Spec (spec) where

import Y2015.D11
import Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "Day 11" $ do
        describe "rotate" $ do
            it "finds the next password after abcdefgh" $
                rotate "abcdefgh" `shouldBe` "abcdffaa"
            it "finds the next password after ghijklmn" $
                rotate "ghijklmn" `shouldBe` "ghjaabcc"
