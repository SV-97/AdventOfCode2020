{-# LANGUAGE PatternGuards #-}

module Main where


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

type Program = [Instruction]
type Accumulator = Integer
type Adress = Integer
data VmState = Run | Halt deriving Eq
data Vm = Vm
    { ip :: Adress
    , acc :: Accumulator
    , prog :: Program
    , state :: VmState
    , highestAdress :: Adress
}

initVm :: Program -> Vm
initVm prog = Vm
    { ip=0
    , acc=0
    , prog=prog
    , state=Run
    , highestAdress=fromIntegral (length prog) - 1}

execute :: Vm -> Instruction -> Vm
execute vm (Acc n) = vm { ip = ip vm + 1, acc = acc vm + n }
execute vm (Nop _) = vm { ip = ip vm + 1 }
execute vm (Jmp n) = vm { ip = ip vm + n }

run :: Vm -> Vm
run vm 
    | ip vm == highestAdress vm + 1 = vm { state=Halt }
    | otherwise = execute vm (prog vm !! fromIntegral (ip vm))

data SolutionStrategy = ChangeNop | ChangeJmp

tryApplyStrategy :: SolutionStrategy -> Instruction -> Maybe Instruction
tryApplyStrategy ChangeNop (Nop a) = Just $ Jmp a
tryApplyStrategy ChangeJmp (Jmp a) = Just $ Nop a
tryApplyStrategy _ _ = Nothing

-- applyStrategy :: SolutionStrategy -> Instruction -> Instruction
-- applyStrategy s i | Just r <- tryApplyStrategy s i = r

detectCandidates :: Solver -> [(Adress, Instruction)]
detectCandidates solver = [(adr, replacement) | (adr, Just replacement) <- zip [0..] replacements]
    where
        replacements = map (tryApplyStrategy $ strategy solver) program
        vm = solverVm solver
        program = prog vm

data Solver = Solver
    { strategy :: SolutionStrategy
    , solverVm :: Vm}

runToLoop :: Vm -> Vm
runToLoop vm = snd $ runToLoop' ([], vm)
    where
        runToLoop' :: ([Adress], Vm) -> ([Adress], Vm)
        runToLoop' (alreadyExecuted, vm)
            | i `elem` alreadyExecuted = (alreadyExecuted, vm)
            | otherwise = runToLoop' (i : alreadyExecuted, vm')
                where
                    i = ip vm
                    vm' = run vm

replaceFirst :: (a -> Maybe a) -> [a] -> [a]
replaceFirst _ [] = []
replaceFirst f (a:as)
    | Just a' <- f a = a' : as
    | Nothing <- f a = a : replaceFirst f as

tryCandidate :: Vm -> (Adress, Instruction) -> Vm
tryCandidate vm (adr, repl) = runToLoop vm'
    where
        vm' = vm {prog = map snd $ replaceFirst f (zip [0..] p)}
        p = prog vm
        f :: (Adress, Instruction) -> Maybe (Adress, Instruction)
        f (i, _)
            | i == adr = Just (i, repl)
            | otherwise = Nothing

changeStrategy :: Solver -> Solver
changeStrategy solver
    | ChangeNop <- strategy solver = solver { strategy = ChangeJmp }
    | _ <- strategy solver = error "Tried every strategy"

solve :: Solver -> Integer
solve solver
    | [s] <- success = acc s
    | _ <- success = solve (changeStrategy solver)
    where
        vm = solverVm solver
        candidates = detectCandidates solver
        tries = map (tryCandidate vm) candidates
        success = filter (\vm -> state vm == Halt) tries
        
constructSolver :: Program -> Solver
constructSolver prog = Solver
    { strategy=ChangeNop
    , solverVm=initVm prog} 

main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> lines
        |> map parseInstruction
        |> constructSolver
        |> solve
        |> print
