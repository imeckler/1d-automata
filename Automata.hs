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

scanRow :: (Row -> a) -> Row -> [a]
scanRow f r@(Row _ c []) = [f r]
scanRow f r = f r : scanRow f (fromJust . right $ r)

hood :: Row -> Neighborhood
hood (Row [] c (y:_)) = (Off, c, y)
hood (Row (x:_) c []) = (x, c, Off)
hood (Row (x:_) c (y:_)) = (x, c, y)

stepHood :: Ruleset -> Neighborhood -> Cell
stepHood rs t = fromJust $ lookup t rs

stepRow :: Ruleset -> Row -> Row
stepRow rs r = let (c:cs) = scanRow (stepHood rs . hood) r in Row [] c cs

asBitlist :: Integer -> [Integer]
asBitlist n = let (q, r) = n `divMod` 2 in if q == 0 then [r] else r : asBitlist q

toRuleset :: Integer -> Ruleset
toRuleset n = zip possHoods vals 
    where toCell 0 = Off
          toCell 1 = On
          vals = (map toCell . asBitlist $ n) ++ repeat Off

strToCells :: String -> [Cell]
strToCells = map (\c -> if c == 'X' then On else Off)

strToRow :: String -> Row
strToRow s = let (c:cs) = strToCells s in Row [] c cs

rowToStr :: Row -> String
rowToStr = scanRow f
    where f (Row _ On _) = 'X'
          f (Row _ Off _) = '_'

minStrToRow :: String -> Integer -> Row
minStrToRow s w = Row (xs ++ offs) c (ys ++ offs)
    where (xs, (c:ys)) = splitAt (length r `div` 2) r
          offs = replicate (fromIntegral $ w `div` 2) Off
          r = strToCells s

toLeftEnd :: Row -> Row
toLeftEnd r = maybe r toLeftEnd $ left r

possHoods :: [Neighborhood]
possHoods = [(a, b, c) | a <- cs, b <- cs, c <- cs]
    where cs = [Off, On]

main = do
    (ruleNum:width:startRow:_) <- getArgs
    let rs = toRuleset $ read ruleNum
    let initRow = toLeftEnd . minStrToRow startRow $ read width
    mapM_ (putStrLn . rowToStr) $ iterate (stepRow rs) initRow
