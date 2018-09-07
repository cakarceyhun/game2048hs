module Main where
import Game

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "shiftLine Tests" $ do
    it "[0, 0, 0, 0] --shiftLine--> [0, 0, 0, 0]" $ do
      shiftLine [0, 0, 0, 0] `shouldBe` [0, 0, 0, 0]
    it "[2, 0, 0, 0] --shiftLine--> [2, 0, 0, 0]" $ do
      shiftLine [2, 0, 0, 0] `shouldBe` [2, 0, 0, 0]
    it "[2, 4, 0, 0] --shiftLine--> [2, 4, 0, 0]" $ do
      shiftLine [2, 4, 0, 0] `shouldBe` [2, 4, 0, 0]
    it "[0, 0, 2, 0] --shiftLine--> [0, 0, 2, 0]" $ do
      shiftLine [0, 0, 2, 0] `shouldBe` [2, 0, 0, 0]
    it "[2, 0, 4, 0] --shiftLine--> [2, 0, 4, 0]" $ do
      shiftLine [2, 4, 0, 0] `shouldBe` [2, 4, 0, 0]
    it "[2, 2, 0, 0] --shiftLine--> [4, 0, 0, 0]" $ do
      shiftLine [2, 2, 0, 0] `shouldBe` [4, 0, 0, 0]
    it "[0, 0, 2, 2] --shiftLine--> [4, 0, 0, 0]" $ do
      shiftLine [0, 0, 2, 2] `shouldBe` [4, 0, 0, 0]
    it "[2, 2, 4, 0] --shiftLine--> [8, 0, 0, 0]" $ do
      shiftLine [2, 2, 4, 0] `shouldBe` [8, 0, 0, 0]

  describe "alignHorizantally Tests" $ do
    it "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5] --alignHorizontally--> [[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 0, 1], [2, 3, 4, 5]]" $ do
      alignHorizontally [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5]
        `shouldBe` [[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 0, 1], [2, 3, 4, 5]]

  describe "alignVertically Tests" $ do
    it "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5] --alignVertically--> [[0, 4, 8, 2], [1, 5, 9, 3], [2, 6, 0, 4], [3, 7, 1, 5]]" $ do
      alignVertically [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5]
        `shouldBe` [[0, 4, 8, 2], [1, 5, 9, 3], [2, 6, 0, 4], [3, 7, 1, 5]]

  describe "flattenVertically Tests" $ do
    it "[[0, 4, 8, 2], [1, 5, 9, 3], [2, 6, 0, 4], [3, 7, 1, 5]] --flattenVertically--> [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5]" $ do
      flattenVertically [[0, 4, 8, 2], [1, 5, 9, 3], [2, 6, 0, 4], [3, 7, 1, 5]]
        `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5]
