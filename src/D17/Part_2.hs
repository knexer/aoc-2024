{-# OPTIONS_GHC -Wno-type-defaults #-}
module D17.Part_2 where

import Data.Bits (xor)
import Debug.Trace (traceShowId)
import Utils (extractNumbers)
import Data.Foldable (for_)

data Program = Program
  { a :: Integer,
    b :: Integer,
    c :: Integer,
    input :: [Integer],
    output :: [Integer],
    instructionPtr :: Integer
  }
  deriving (Show)

-- 0, 5, 3 first
step :: Integer -> Integer -> Program -> Program
-- The adv instruction (opcode 0) performs division.
--  The numerator is the value in the A register.
--  The denominator is found by raising 2 to the power of the instruction's combo operand.
--  (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.)
--  The result of the division operation is truncated to an integer and then written to the A register.
step 0 op program = program {a = result, instructionPtr = instructionPtr program + 2}
  where
    numerator = a program
    denominator = 2 ^ (comboOperand op program)
    result = numerator `div` denominator
-- The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand,
-- then stores the result in register B.
step 1 op program = program {b = result, instructionPtr = instructionPtr program + 2}
  where
    result = op `xor` b program
-- The bst instruction (opcode 2) calculates the value of its combo operand modulo 8,
-- then writes that value to the B register.
step 2 op program = program {b = result, instructionPtr = instructionPtr program + 2}
  where
    result = comboOperand op program `mod` 8
-- The jnz instruction (opcode 3) does nothing if the A register is 0.
-- However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand;
-- if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
step 3 op program = program {instructionPtr = nextIP}
  where
    nextIP = if a program /= 0 then op else instructionPtr program + 2

-- The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C,
--  then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
step 4 _ program = program {b = result, instructionPtr = instructionPtr program + 2}
  where
    result = b program `xor` c program
-- The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value.
-- (If a program outputs multiple values, they are separated by commas.)
step 5 op program = program {output = output program ++ [result], instructionPtr = instructionPtr program + 2}
  where
    result = comboOperand op program `mod` 8
step 6 op program = program {b = result, instructionPtr = instructionPtr program + 2}
  where
    numerator = a program
    denominator = 2 ^ (comboOperand op program)
    result = numerator `div` denominator
step 7 op program = program {c = result, instructionPtr = instructionPtr program + 2}
  where
    numerator = a program
    denominator = 2 ^ (comboOperand op program)
    result = numerator `div` denominator
step _ _ _ = undefined

comboOperand :: Integer -> Program -> Integer
comboOperand 0 _ = 0
comboOperand 1 _ = 1
comboOperand 2 _ = 2
comboOperand 3 _ = 3
comboOperand 4 program = a program
comboOperand 5 program = b program
comboOperand 6 program = c program
comboOperand _ _ = undefined

terminated :: Program -> Bool
terminated program = instructionPtr program >= (toInteger . length . input) program

run :: Program -> [Integer]
run program
  | terminated program = output program
  | otherwise = run nextProgram
  where
    nextOpcode = input program !! (fromInteger . instructionPtr) program
    nextOperand = input program !! ((fromInteger . instructionPtr) program + 1)
    nextProgram = step nextOpcode nextOperand program

isQuine :: Program -> Bool
isQuine program = (input program == traceShowId (run program))

-- answer is over 1 billion...
-- brute force not gonna cut it
-- so the question here is, what is this computing as a function of a?
-- and how do we invert that function?
bruteForceSmallestQuine :: Integer -> Program -> Integer
bruteForceSmallestQuine a program = if isQuine program {a} then a else bruteForceSmallestQuine (a + 1) program

parse :: String -> Program
parse contents = Program {a = a, b = b, c = c, input = input, output = [], instructionPtr = 0}
  where
    inputLines = lines contents
    a = toInteger ((extractNumbers (inputLines !! 0)) !! 0)
    b = toInteger ((extractNumbers (inputLines !! 1)) !! 0)
    c = toInteger ((extractNumbers (inputLines !! 2)) !! 0)
    input :: [Integer] = (map toInteger . extractNumbers) (inputLines !! 4)

-- helper for manual process: go from left to right and nail down the digits
-- some backtracking needed maybe but shouldn't be too bad
makeGuess :: Integer -> Integer
makeGuess digit = traceShowId guess
    where
        digits = [digit, 0, 4, 2, 2, 5, 5, 0, 1, 0, 3, 1, 5, 4, 0, 3]
        guess = sum $ map (\(digitX, expon) -> digitX * 8^expon) (zip digits [0..])

main :: String -> IO ()
main contents = do
  print $ run program {a = 40}
  putStrLn ("g =" ++ show [2,4,1,3,7,5,4,1,1,3,0,3,5,5,3,0])
  let programs = zip (map makeGuess [0..7]) [0..7]
  for_ programs (print . (\foo -> (snd foo, run program {a=fst foo})))
  print $ bruteForceSmallestQuine 108107566327030 program
  where
    program = parse contents
    -- tests = map (\a -> program {a = 8 ^ a + 40}) [0 .. 20]
