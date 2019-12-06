#!/bin/runhaskell

import Debug.Trace


-- calcPart2 :: Int -> [Int] -> Int
-- calcPart2 prev input =
--     let
--         new = sum $ traceShowId $ fmap (max 0) $ fmap (\x -> x `div` 3 - 2) input
--     in
--         if new == prev then
--             new
--         else
--             calcPart2 new (input ++ [new-prev])

calcPart2 :: Int -> Int
calcPart2 input =
    let
        new = max 0 (input `div` 3 - 2)
    in
        if new <= 0 then
            0
        else
            new + calcPart2 new



day1 :: String -> String
day1 input =
    show
        $ sum
        $ fmap ((\x -> floor $ x / 3 - 2) . read)
        $ lines input

day1pt2 :: String -> String
day1pt2 input =
    show
        $ sum
        $ fmap calcPart2
        $ fmap read
        $ lines input

main :: IO ()
main = do
    interact ((++ "\n") . day1pt2)
