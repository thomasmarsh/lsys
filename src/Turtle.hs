module Turtle
    ( parse
    , lsys
    , main1
    , main2
    ) where

import           Control.Monad (foldM)
import qualified Data.Map as M
import           Data.List.Split (splitOn)

deg2rad :: Float -> Float
deg2rad = (*) (pi / 180)

data Instruction
    = Forward
    | ForwardBlank
    | CounterClockwise
    | Clockwise
    | Variable Char
    | PushState
    | PopState
    deriving (Ord, Eq, Show)

type Program = [Instruction]
type Rules = M.Map Instruction Program

data State = State
    { position :: (Float, Float)
    , angle :: Float
    , stack :: [((Float, Float), Float)]
    } deriving (Show)

data StepResult
    = Point Float Float
    | Reset Float Float
    | Break
    | Nil
    deriving (Eq, Show)

forward :: State -> State
forward state@State { position=(x, y), angle=a }
    = state { position = (x', y') }
    where x' = x - cos a
          y' = y + sin a

turn :: Float -> State -> State
turn amt state@State { angle=a }
    = state { angle = a+amt }
    
step :: Float -> Instruction -> State -> (State, StepResult)
step _ Forward state
    = (state', Point x' y')
    where state' = forward state
          (x', y') = position state'

step _ ForwardBlank state = (forward state, Break)
step amt CounterClockwise state = (turn (-amt) state, Nil)
step amt Clockwise state = (turn amt state, Nil)
step _ (Variable _) state = (state, Nil)

step _ PushState state@State { position=p, angle=a, stack = s }
    = (state { stack = (p, a):s }, Nil)

step _ PopState (State _ _ []) = error "attempt to pop empty stack"
step _ PopState state@State { stack = (x:xs) }
    = (state { position = p, angle = a, stack = xs }, uncurry Reset p)
    where (p, a) = x

walk :: Float -> Program -> State -> [StepResult]
walk amt prog state@State {position=(initX, initY)} = reverse $ loop state prog [Point initX initY]
    where loop _ [] result = result
          loop s (x:xs) result = loop s' xs result'
            where (s', r) = step amt x s
                  result' = case r of
                                Nil -> result
                                _ -> r:result

initState :: State
initState = State { position = (0,0), angle=deg2rad 90, stack = [] }

balancedAlg :: Int -> Char -> Either String Int
balancedAlg i '[' = Right $ i + 1
balancedAlg i ']' | i > 0 = Right $ i - 1
balancedAlg i ']' | i <= 0 = Left "Unbalanced"
balancedAlg i _ = Right i

balanced :: String -> Bool
balanced s = case foldM balancedAlg 0 s of
               Left _ -> False
               Right i -> i == 0

parse :: String -> Program
parse [] = []
parse ('F':xs) = Forward          : parse xs
parse ('f':xs) = ForwardBlank     : parse xs
parse ('-':xs) = CounterClockwise : parse xs
parse ('+':xs) = Clockwise        : parse xs
parse ('[':xs) = PushState        : parse xs
parse (']':xs) = PopState         : parse xs
parse (x:xs)   = Variable x       : parse xs

unparse :: Program -> String
unparse [] = []
unparse (Forward          : xs) = 'F' : unparse xs
unparse (ForwardBlank     : xs) = 'f' : unparse xs
unparse (CounterClockwise : xs) = '-' : unparse xs
unparse (Clockwise        : xs) = '+' : unparse xs
unparse (PushState        : xs) = '[' : unparse xs
unparse (PopState         : xs) = ']' : unparse xs
unparse (Variable x       : xs) = x : unparse xs

rewrite :: Rules -> Program -> Program
rewrite r = foldr (\x -> (++) (M.findWithDefault [x] x r)) []

paths :: [StepResult] -> [[(Float, Float)]]
paths [] = []
paths xs = map f split
    where split = splitOn [Break] xs
          f = map (\x -> case x of
                            Reset x y -> (x, y)
                            Point x y -> (x, y)
                            _ -> error "unexpected entry in path")

-- foldl is dramatically slower, but uses very little memory
-- rewrite r = foldl' (\result x -> result ++ M.findWithDefault [x] x r) []

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes n f x = f $ nTimes (n-1) f x

printCoords :: [StepResult] -> IO ()
printCoords =
    mapM_ (\s -> case s of
                    Point x y -> putStrLn $ "    (" ++ show x ++ ", " ++ show y ++ "),"
                    Reset x y -> putStrLn $ "    (nan, nan),\n    (" ++ show x ++ ", " ++ show y ++ "),"
                    Break -> putStrLn "    (nan, nan),")

lsys :: Program -> Rules -> Int -> Float -> [[(Float, Float)]]
lsys prog r n a = paths $ walk a (nTimes n (rewrite r) prog) initState

example :: Rules
example = M.fromList
            [ (Variable 'X', parse "F[-X][X]F[-X]+FX")
            , (Forward,      parse "FF")]

emptyRules :: Rules
emptyRules = M.empty

main1 :: IO ()
main1 =
    mapM_ print (lsys (parse "X") example 6 (deg2rad 25))

main2 :: IO ()
main2 = do
    let prog = parse "F-F+F+F+f+F+F+F-F"
    mapM_ print (lsys prog emptyRules 6 (deg2rad 45))
