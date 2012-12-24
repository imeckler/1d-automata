module Main where

import Data.Maybe (fromJust)
import System.Environment (getArgs)

data Cell = On | Off deriving (Eq, Ord, Show)
data Row = Row [Cell] Cell [Cell] deriving (Eq, Show)
type Neighborhood = (Cell, Cell, Cell)
type Ruleset = [(Neighborhood, Cell)]

left :: Row -> Maybe Row
left (Row [] _ _) = Nothing
left (Row (x:xs) c ys) = Just $ Row xs x (c:ys)

right :: Row -> Maybe Row
right (Row _ _ []) = Nothing
right (Row xs c (y:ys)) = Just $ Row (c:xs) y ys

scanRowR :: (Row -> a) -> Row -> [a]
scanRowR f r@(Row _ _ []) = [f r]
scanRowR f r = f r : scanRowR f (fromJust . right $ r)

scanRowL :: (Row -> a) -> Row -> [a]
scanRowL f r@(Row [] _ _) = [f r]
scanRowL f r = f r : scanRowL f (fromJust . left $ r)

scanBi :: (Row -> Cell) -> Row -> Row
scanBi f r = Row (scanRowL f . fromJust $ left r) (f r) (scanRowR f . fromJust $ right r)

hood :: Row -> Neighborhood
hood (Row [] c (y:_)) = (Off, c, y)
hood (Row (x:_) c []) = (x, c, Off)
hood (Row (x:_) c (y:_)) = (x, c, y)

stepHood :: Ruleset -> Neighborhood -> Cell
stepHood rs t = fromJust $ lookup t rs

stepRow :: Ruleset -> Row -> Row
stepRow rs = scanBi (stepHood rs . hood)

approxRow :: Int -> Row -> Row
approxRow n (Row xs c ys) = Row (take n xs) c (take n ys)

asBitlist :: Integer -> [Integer]
asBitlist n = let (q, r) = n `divMod` 2 in if q == 0 then [r] else r : asBitlist q

toRuleset :: Integer -> Ruleset
toRuleset n = zip possHoods vals 
    where toCell 0 = Off
          toCell 1 = On
          vals = (map toCell . asBitlist $ n) ++ repeat Off

rowToStr :: Row -> String
rowToStr = scanRowR f
    where f (Row _ On _) = '\9608'
          f (Row _ Off _) = ' '

strToRow :: String -> Row
strToRow s = Row (xs ++ offs) c (ys ++ offs)
    where (xs, c:ys) = splitAt (length r `div` 2) r
          offs = repeat Off
          r = map (\c -> if c == 'X' then On else Off) s

toLeftEnd :: Row -> Row
toLeftEnd r = maybe r toLeftEnd $ left r

possHoods :: [Neighborhood]
possHoods = [(a, b, c) | a <- cs, b <- cs, c <- cs]
    where cs = [Off, On]

main = do
    (ruleNum:width:startRow:_) <- getArgs
    let rs = toRuleset $ read ruleNum
    let initRow = approxRow (read width `div` 2) $ strToRow startRow
    mapM_ (putStrLn . rowToStr . toLeftEnd) $ iterate (stepRow rs) initRow
