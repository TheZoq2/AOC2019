#!/bin/runhaskell

import Debug.Trace

import Data.List
import Data.List.Split

showRow :: [Int] -> String
showRow row =
    intercalate " " $ fmap show row

debugOutputMem :: Int -> [Int] -> [String]
debugOutputMem _ [] = [""]
debugOutputMem addr mem = debugOutputMem (addr+16) (drop 16 mem) ++ [showRow (take 16 mem)]

debugOutput :: Int -> (Int, Int, Int, Int) -> (Int, Int) -> [Int] -> String
debugOutput pc decoded operands mem =
    let
        pcstr = "PC: " ++ show pc
        instr = "Instr: " ++ show decoded
        operands = "Operands: " ++ show operands
        delim = "================="
        memStr = intercalate "\n" $ debugOutputMem 0 mem
    in
    intercalate "\n" [pcstr, instr, operands, delim, memStr]


-- Replace the value at position ix with val in the array d
replace :: Int -> Int -> [Int] -> [Int]
replace ix val d =
    fmap (\(d, i) -> if i == ix then val else d) $ zip d $ iterate (+1) 0

evalStage :: Int -> [Int] -> (Maybe Int, [Int])
evalStage instrStart memory =
    let
        -- Fetch and decode the first instruction
        instr = take 4 $ drop instrStart (memory ++ repeat 0)
        [op, s1, s2, dest] = instr

        -- Look up the operands
        v1 = memory !! s1
        v2 = memory !! s2

        -- Get the op to be performed
        mod = case op of
            1 -> Just (+)
            2 -> Just (*)
            99 -> Nothing

        -- Move program counter
        nextInstr = case op of
            99 -> Nothing
            _ -> Just $ instrStart + 4

        newMem = case mod of
            Just mod -> replace dest (mod v1 v2) memory
            Nothing -> memory

        debugString = debugOutput instrStart (op, s1, s2, dest) (v1, v2) memory
    in
    (nextInstr, traceShow debugString newMem)


evalProgram :: Int -> [Int] -> [Int]
evalProgram ix mem =
    case evalStage (traceShowId ix) (traceShowId mem) of
        (Just next, mem) -> evalProgram next mem
        (Nothing, mem) -> mem


part1 :: String -> String
part1 input =
    let
        asNumbers :: [Int]
        asNumbers = fmap read $ splitOn "," input
    in
        "Done " ++ (intercalate "," $ fmap show $ evalProgram 0 asNumbers)

main :: IO ()
main = do
    interact ((++ "\n") . part1)
