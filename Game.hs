module Game where

import Data.Time.Clock.POSIX
import System.Random

shiftLine' :: [Int] -> [Int]
shiftLine' xs =
  let
    xs' = filter (/=0) xs
    merger acc x
      | acc == []     = acc ++ [x]
      | last acc == x = init acc  ++ [2*x]
      | otherwise     = acc ++ [x]
    xs'' = foldl merger [] xs'
  in
    xs'' ++ (replicate (4 - (length xs'')) 0)

shiftLine :: [Int] -> [Int]
shiftLine xs = shiftLine' $ shiftLine' xs

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

generateTile :: Int -> IO (Int, Int)
generateTile numberOfZero = do
  posixTime <- getPOSIXTime
  let generatorNumber = (truncate (posixTime * 1000000000)) `mod` 1000000000
  let randomNumber =
        fst (random (mkStdGen generatorNumber) :: (Int, StdGen))
  return (
    mod (div randomNumber 2) numberOfZero,
    2 * (mod randomNumber 2) + 2
    )

data Direction = UP | DOWN | LEFT | RIGHT | WRONG deriving (Show, Eq)

directionTable :: String -> Direction
directionTable "w" = UP
directionTable "s" = DOWN
directionTable "a" = LEFT
directionTable "d" = RIGHT
directionTable _ = WRONG

askDirection :: IO Direction
askDirection = do
  line <- getLine
  let direction = directionTable line
  if direction == WRONG then
    askDirection
  else
    return direction

placeTile :: (Int, Int) -> [Int] -> [Int]
placeTile tile tiles = take (fst tile) tiles ++ [snd tile] ++ drop (fst tile + 1) tiles

slide :: Direction -> [Int] -> [Int]
slide direction tiles
  | direction == UP    = eventToUp tiles
  | direction == DOWN  = eventToDown tiles
  | direction == LEFT  = eventToLeft tiles
  | direction == RIGHT = eventToRight tiles
  | otherwise          = error "SHOULD NOT BE HERE"

printTiles :: [Int] -> IO ()
printTiles tiles
  | length tiles >= 4 = do
      print $ take 4 tiles
      printTiles $ drop 4 tiles
  | otherwise = return ()

updateTilesWithDirection :: [Int] -> IO [Int] 
updateTilesWithDirection tiles' = do
      direction <- askDirection
      return (slide direction tiles')

mainLoop :: [Int] -> IO ()
mainLoop tiles
  | numberOfZero == 0 = do print "End"
  | otherwise          = do
    tile <- generateTile numberOfZero
    let tiles' = placeTile tile tiles
    printTiles tiles'
    tiles'' <- updateTilesWithDirection tiles'
    mainLoop tiles''
  where
    numberOfZero = length $ filter (\x -> x == 0) tiles

main :: IO ()
main =
  let
    tiles = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  in do
    mainLoop tiles
