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

allignHorizontally :: [Int] -> [[Int]]
allignHorizontally xs =
  let
    innerAlign xss
      | length(last xss) <= 4 = xss
      | otherwise             = innerAlign $ init xss ++ [take 4 (last xss)] ++ [drop 4 (last xss)]
  in
    innerAlign [xs]

flattenHorizontally :: [[Int]] -> [Int]
flattenHorizontally xs = concat xs

allignVertically :: [Int] -> [[Int]]
allignVertically xs =
  foldl (\acc y -> (tail acc) ++ [(head acc) ++ [y]]) [[], [], [], []] xs

flattenVertically :: [[Int]] -> [Int]
flattenVertically xss =
  let
    innerFlatten (xs, [[], [], [], []]) = (xs, [[], [], [], []])      
    innerFlatten (xs, yss) = innerFlatten (xs ++ (map (\ys -> head ys) yss), map (\ys -> tail ys) yss)
  in
    fst $ innerFlatten ([], xss)

main :: IO ()
main = hspec $ do
  describe "allignHorizantally Tests" $ do
    it "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5] --allignHorizontally--> [[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 0, 1], [2, 3, 4, 5]]" $ do
      allignHorizontally [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5]
        `shouldBe` [[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 0, 1], [2, 3, 4, 5]]
  describe "flattenHorizontally Tests" $ do
    it "[[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 0, 1], [2, 3, 4, 5]] --flattenHorizontally--> [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5]" $ do
      flattenHorizontally [[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 0, 1], [2, 3, 4, 5]]
        `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5]
  describe "allignVertically Tests" $ do
    it "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5] --allignVertically--> [[0, 4, 8, 2], [1, 5, 9, 3], [2, 6, 0, 4], [3, 7, 1, 5]]" $ do
      allignVertically [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5]
        `shouldBe` [[0, 4, 8, 2], [1, 5, 9, 3], [2, 6, 0, 4], [3, 7, 1, 5]]
  describe "flattenVertically Tests" $ do
    it "[[0, 4, 8, 2], [1, 5, 9, 3], [2, 6, 0, 4], [3, 7, 1, 5]] --flattenVertically--> [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5]" $ do
      flattenVertically [[0, 4, 8, 2], [1, 5, 9, 3], [2, 6, 0, 4], [3, 7, 1, 5]]
        `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5]
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
