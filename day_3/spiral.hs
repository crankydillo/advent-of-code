-- Surely there's a much more efficient way to do this, but I already
-- have 2 side projects going!  Ok, enough with excuses...
import Data.List
import Data.Maybe
import qualified Data.Map as M

numbersInSpiral = [ (x * 2 + 1)^2 | x <- [0..] ]

spiralContains x = fromJust $ findIndex (x <=) numbersInSpiral

start 0 = 1
start spiral = ((numbersInSpiral !! spiral - numbersInSpiral !! (spiral - 1)) `div` 2) + numbersInSpiral !! (spiral - 1)

matrixSize spirals = spirals * 2 + 1

centerPoint matrixSize = (x, x) where x = (matrixSize `div` 2)

position x
      | x == startPt                   = (0, 0)
      | x < startPt && x >= rightUpper = (x - startPt, startPt - rightUpper)
      | x < startPt                    = (rightUpper - x, startPt - rightUpper)
      | x > startPt && x <= leftLower  = (x - startPt, 0)
      | x > startPt                    = (leftLower - startPt, x - leftLower)
      where spiral     = spiralContains x
            startPt    = start spiral
            matrixS    = matrixSize spiral
            rightUpper = startPt - matrixS + 1
            leftLower  = startPt + matrixS - 1

ans square = abs (px - cx) + abs (py - cy)
      where spiral     = spiralContains square
            matrixS    = matrixSize spiral
            (px, py)   = position square
            (cx, cy)   = centerPoint matrixS

main = do putStrLn $ show $ numbersInSpiral !! 3
          putStrLn $ show $ ans 1024  -- this is wrong
          putStrLn $ show $ ans 277678 -- but this is right???
          putStrLn $ show $ spiralCoords 3
          putStrLn "--------"
          putStrLn $ show $ spiralCoords 9 
          putStrLn "--------"
          putStrLn $ show $ spiralCoords 5
          putStrLn "--------"
          putStrLn $ show $ spiralVals 5
          putStrLn "--------"
          putStrLn $ show $ head $ dropWhile (<= 277678) $ sort (map snd (spiralVals 33)) -- prob 2 answer

spiralVals s = let first = ((centerPoint s), 1)
               in first : (spiralValsH  (tail $ spiralCoords s) [first])

spiralValsH :: Integral i => [(i, i)] -> [((i, i), i)] -> [((i, i), i)]
spiralValsH [] _ = []
spiralValsH coords prev =
    let next = ((head coords), inc (head coords) prev)
    in next : (spiralValsH (tail coords) (prev ++ [next]))

inc :: Integral i => (i, i) -> [((i, i), i)] -> i
inc (x, y) previous = foldl (\acc p -> acc + (lup p)) 0 $ adjacents (x, y)
           where lup p = case (lookup p previous) of
                           Just x  -> x
                           Nothing -> 0

adjacents (x, y) = [(x-1, y), (x, y-1), (x-1, y-1), (x+1, y), (x, y+1), (x+1, y+1), (x+1, y-1), (x-1, y+1) ]

-- Should redo the first question on top of this
spiralCoords s = centerPoint s : reverse (spiralCoordsH s s 1)

{-
 - We'll spiral backwards from endpoint.  The list concatenations work like so:
 - bottom (left->right) ++ left (bottom->top) ++ top (left->right) ++ right (top->bottom-1)
 - Then we recursively generate the next spiral down
 -}
spiralCoordsH :: (Integral i) => i -> i -> i -> [(i, i)]
spiralCoordsH 1 _ _ = []
spiralCoordsH s origS ctr = [ (origS-ctr, y           ) | y <- reverse [ctr-1..origS-ctr]] ++
                             [(x-1      , ctr-1       ) | x <- reverse [ctr..origS-ctr]] ++
                             [(ctr-1    , y           ) | y <- [ctr..(origS - (ctr + 1))]] ++
                             [(x        ,  origS - ctr) | x <- [ctr-1..(origS - (ctr + 1))]] ++
                             spiralCoordsH (s - 2) origS (ctr + 1)
