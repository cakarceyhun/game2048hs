import Test.Hspec

shiftLine :: [Int] -> [Int]
shiftLine xs =
  let
    xs' = filter (/=0) xs
    merger acc x
      | acc == []     = acc ++ [x]
      | last acc == x = init acc  ++ [2*x]
      | otherwise     = acc ++ [x]
    xs'' = foldl merger [] xs'
  in
    xs'' ++ (replicate (4 - (length xs'')) 0)

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
