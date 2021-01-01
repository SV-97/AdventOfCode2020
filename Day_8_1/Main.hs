module Main where

import qualified Data.Set as Set
import Data.List
import Data.Maybe (isJust)
import Data.Array

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

parseInstruction :: String -> Instruction
parseInstruction raw
    | instr == "nop" = Nop n
    | instr == "acc" = Acc n
    | instr == "jmp" = Jmp n
    | otherwise = error "Invalid instruction"
    where
        [instr, op] = words raw
        n = case op of
            ('+':d) -> read d
            ('-':d) -> - read d
            _ -> error "Invalid instruction operand"


data Instruction
    = Nop Integer
    | Acc Integer
    | Jmp Integer

type Program = Array Integer Instruction
type Accumulator = Integer
type Adress = Integer
data Vm = Vm
    { ip :: Adress
    , acc :: Accumulator
    , prog :: Program
}

initVm :: Program -> Vm
initVm prog = Vm {ip=0, acc=0, prog=prog}

execute :: Vm -> Instruction -> Vm
execute vm (Acc n) = vm { ip = ip vm + 1, acc = acc vm + n }
execute vm (Nop _) = vm { ip = ip vm + 1 }
execute vm (Jmp n) = vm { ip = ip vm + n }

run :: Vm -> Vm
run vm = execute vm (prog vm ! ip vm)

-- :)
solveHaltingProblem :: Program -> Integer
solveHaltingProblem prog = acc finalState
    where
        (_, finalState) = runToLoop ([], initVm prog)
        runToLoop :: ([Adress], Vm) -> ([Adress], Vm)
        runToLoop (alreadyExecuted, vm)
            | i `elem` alreadyExecuted = (alreadyExecuted, vm)
            | otherwise = runToLoop (i : alreadyExecuted, vm')
                where
                    i = ip vm
                    vm' = run vm

main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> lines
        |> map parseInstruction
        |> (\instrList -> listArray (0, fromIntegral $ length instrList - 1) instrList)
        |> solveHaltingProblem
        |> print
