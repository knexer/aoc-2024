module D13.Part_2 where

import Data.Char (isDigit)
import Data.Maybe (catMaybes)

type Vector = (Integer, Integer)

data Machine = Machine
  { a :: Vector,
    b :: Vector,
    goal :: Vector
  }
  deriving (Show)

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split delimiter x = as : split delimiter (drop 1 bs)
  where
    (as, bs) = span (/= delimiter) x

extractNumbers :: String -> [Integer]
extractNumbers [] = []
extractNumbers string
  | length numbers == 0 = []
  | otherwise = read numbers : extractNumbers rest
  where
    (numbers, rest) = span isDigit (dropWhile (not . isDigit) string)

parseVector :: String -> Vector
parseVector = makeVector . extractNumbers
  where
    makeVector (a : b : _) = (a, b)
    makeVector _ = undefined

parseMachine :: [String] -> Machine
parseMachine strings =
  Machine
    { a = parseVector (strings !! 0),
      b = parseVector (strings !! 1),
      goal = (goalMod + fst origGoal, goalMod + snd origGoal)
    }
  where
    origGoal = parseVector (strings !! 2)
    goalMod = 10000000000000
    -- goalMod = 0

parse :: String -> [Machine]
parse input = map parseMachine (((split "") . lines) input)

-- how to do more efficiently?
-- There should only be a limited number of things we have to check
-- there should be cycles, actually, right?
-- something like lcm/gcd/etc?
-- like suppose a is 8, b is 3, goal is 100
-- then 12a is 96, leaves us with 4 remaining (1b + 1)
-- 11a is 88, leaves us with 12 remaining (4b + 0)
-- 10a is 80, leaves us with 20 remaining (6b + 2)
-- 9a is 72, leaves us with 28 remaining (9b + 1)
-- that +1, +0, +2, +1 is gonna repeat
-- it cannot take more than 3 steps to repeat because there aren't more than 3 numbers less than 3
-- right?

-- so, basically, we can say - divide goal by a, then take the remainder mod b
-- then add a to that remainder b - 1 times, and take each of those remainders mod b
-- if any of those are 0, that gets us our valid solutions
-- if none of those are 0, there are no valid solutions

-- for any valid solution, we can exchange b as for a bs and the solution is also valid (but diff num tokens)
-- that gets us all of the valid solutions (I think...)

-- but which solutions are optimal?
-- any solution will either do as many As as possible or as many Bs as possible, depends on which wins the token efficiency war
-- so, we don't have to explicitly consider the tradeoffs - just repeat the process above with a and b swapped
-- the optimal solution will be one of the a + b potential solutions we check

-- actually fuck all of that, it's totally wrong, it's for the 1d case and this is 2d and that does matter

-- this is just a simple trig thing in 2d
-- each of these vectors has an angle, and we know one side length
-- three angles and a side length = a triangle, i.e. we can just directly calculate the number of presses of each button
-- then round those to the closest integer, and check if it works (to handle floating point errors I guess)

-- however, there is the edge case of collinearity
-- if a and b are collinear, then this falls apart, and it reduces to the 1d case which is, weirdly, actually harder
-- but... I think the solution above might be applicable then ? Or something along those lines.

-- maybe I just check for collinearity now and save myself the trouble?
-- checked, there's no collinearity yay!

-- so how do I trig the trig?
-- I have all the angles, and one side length / two corners
-- need to find the third corner / the other two side lengths

-- apparently there's a thing called a sine rule
-- sine of an angle / length of opposite side is constant for any given triangle
-- so, how do we actually calculate the angles?
-- the angle of vector a relative to vector b is opposite the known side
-- this gives us our ratio
-- the angle of vector a relative to the goal vector is opposite the vector b
-- this, combined with the ratio, gives us the number of b presses

angleWithX :: Vector -> Double
angleWithX (x, y) = (atan2 (fromIntegral y) (fromIntegral x))

len :: Vector -> Double
len (x, y) = sciLen
  where
    sciLen = (sqrt . fromIntegral) (sqrX + sqrY)
    sqrX = (x * x)
    sqrY = (y * y)

-- the angle of vector a relative to vector b is opposite the known side
-- this gives us our ratio
ratio :: Machine -> Double
ratio machine = ((len . goal) machine) / (sin angleAB)
  where
    angleAX = (angleWithX . a) machine
    angleBX = (angleWithX . b) machine
    angleAB = angleAX - angleBX

-- the angle of vector A relative to the goal vector is opposite the vector B
-- this, combined with the ratio, gives us the length of the B side
bLength :: Machine -> Double
bLength machine = sin angleAGoal * ratio machine
  where
    angleAGoal = angleAX - angleGoalX
    angleAX = (angleWithX . a) machine
    angleGoalX = (angleWithX . goal) machine

bPresses :: Machine -> Integer
bPresses machine = (abs . round) fracPresses
  where
    fracPresses = bLength machine / (len . b) machine

aLength :: Machine -> Double
aLength machine = sin angleBGoal * ratio machine
  where
    angleBGoal = angleBX - angleGoalX
    angleBX = (angleWithX . b) machine
    angleGoalX = (angleWithX . goal) machine

aPresses :: Machine -> Integer
aPresses machine = (abs . round) fracPresses
  where
    fracPresses = aLength machine / (len . a) machine

tokens :: Machine -> Maybe Integer
tokens machine
  | reachedGoal = Just (3 * aPresses machine + bPresses machine)
  | otherwise = Nothing
  where
    reachedLocation = 
      ( aPresses machine * (fst . a) machine + bPresses machine * (fst . b) machine,
        aPresses machine * (snd . a) machine + bPresses machine * (snd . b) machine
      )
    reachedGoal = reachedLocation == goal machine

main :: String -> IO ()
main = print . sum . catMaybes . (map tokens) . parse
