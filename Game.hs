module Game where

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

alignHorizontally :: [Int] -> [[Int]]
alignHorizontally xs =
  let
    innerAlign xss
      | length(last xss) <= 4 = xss
      | otherwise             = innerAlign $ init xss ++ [take 4 (last xss)] ++ [drop 4 (last xss)]
  in
    innerAlign [xs]

flattenHorizontally :: [[Int]] -> [Int]
flattenHorizontally xs = concat xs

alignVertically :: [Int] -> [[Int]]
alignVertically xs =
  foldl (\acc y -> (tail acc) ++ [(head acc) ++ [y]]) [[], [], [], []] xs

flattenVertically :: [[Int]] -> [Int]
flattenVertically xss =
  let
    innerFlatten (xs, [[], [], [], []]) = (xs, [[], [], [], []])      
    innerFlatten (xs, yss) = innerFlatten (xs ++ (map (\ys -> head ys) yss), map (\ys -> tail ys) yss)
  in
    fst $ innerFlatten ([], xss)

eventToLeft :: [Int] -> [Int]
eventToLeft xs = flattenHorizontally (map shiftLine (alignHorizontally xs))

eventToRight :: [Int] -> [Int]
eventToRight xs = reverse (eventToLeft (reverse xs))

eventToUp :: [Int] -> [Int]
eventToUp xs = flattenVertically (map shiftLine (alignVertically xs))

eventToDown :: [Int] -> [Int]
eventToDown xs = reverse (eventToUp (reverse xs))



