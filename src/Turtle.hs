module Turtle
    ( parse
    , lsys
    , main3
    ) where

import           Control.Monad (foldM)
import           Data.Bool (bool)
import qualified Data.Map as M
import           Data.List.Split (splitOn, keepDelimsL, split, whenElt)

import Graphics.Gloss

--
-- Types
--

type Coord = (Float, Float)
type Bounds = (Coord, Coord)
type Coords = [Coord]
type Segments = [Coords]

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
type Rules = [(Char, String)]
type ParsedRules = M.Map Instruction Program

data StepResult
    = Point Float Float
    | Reset Float Float
    | Break
    | Nil
    deriving (Eq, Show)

data State = State
    { position :: Coord
    , angle :: Float
    , stack :: [(Coord, Float)]
    } deriving (Show)

deg2rad :: Float -> Float
deg2rad = (*) (pi / 180)

initState :: State
initState = State { position = (0,0), angle=deg2rad 90, stack = [] }


--
-- Walking
--

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
step _ PopState state@State { stack = ((p, a):xs) }
    = (state { position = p, angle = a, stack = xs }, uncurry Reset p)

walk :: Float -> Program -> State -> [StepResult]
walk amt prog state@State {position=(initX, initY)}
    = reverse $ loop state prog [Point initX initY]
    where loop _ [] result = result
          loop s (x:xs) result = loop s' xs result'
            where (s', r) = step amt x s
                  result' = case r of
                                Nil -> result
                                _ -> r:result


--
-- Parsing
--

balanced :: String -> Bool
balanced s = case foldM go 0 s of
               Left _ -> False
               Right i -> i == 0
    where go :: Int -> Char -> Either () Int
          go i '[' = Right $ i + 1
          go i ']' | i > 0 = Right $ i - 1
          go i ']' | i <= 0 = Left ()
          go i _ = Right i

parse :: String -> Program
parse x | not (balanced x) = error ("unbalanced brackets: " ++ x)
parse x = map parseChar x

parseChar :: Char -> Instruction
parseChar 'F' = Forward
parseChar 'f' = ForwardBlank
parseChar '-' = CounterClockwise
parseChar '+' = Clockwise
parseChar '[' = PushState
parseChar ']' = PopState
parseChar x   = Variable x

unparse :: Program -> String
unparse = map unparseChar

unparseChar :: Instruction -> Char
unparseChar Forward          = 'F'
unparseChar ForwardBlank     = 'f'
unparseChar CounterClockwise = '-'
unparseChar Clockwise        = '+'
unparseChar PushState        = '['
unparseChar PopState         = ']'
unparseChar (Variable x)     = x

parseRules :: Rules -> ParsedRules
parseRules r = M.fromList $ map f r
    where f (c, p) = (parseChar c, parse p)


--
-- Rule application
--

rewrite :: ParsedRules -> Program -> Program
rewrite r = foldr (\x -> (++) (M.findWithDefault [x] x r)) []

-- foldl is dramatically slower, but uses very little memory
-- rewrite r = foldl' (\result x -> result ++ M.findWithDefault [x] x r) []

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes n f x = f $ nTimes (n-1) f x

lsys :: String -> ParsedRules -> Int -> Float -> Segments
lsys prog rules n angle
    = paths $ walk angle iterated initState
    where iterated = nTimes n (rewrite rules) (parse prog)


--
-- Conversion to screen values
--

splitSteps :: [StepResult] -> [[StepResult]]
splitSteps xs
    = dropNull $ concatMap reset broken
    where broken = dropNull $ splitOn [Break] xs
          reset = split (keepDelimsL $ whenElt isReset)
          isReset elt = case elt of
                            Reset _ _ -> True
                            _ -> False
          dropNull = filter (not . null)

paths :: [StepResult] -> Segments
paths xs = map (map toPoint) (splitSteps xs)
    where toPoint (Point x y) = (x, y)
          toPoint (Reset x y) = (x, y)
          toPoint _ = error "unexpected entry in path"

center :: Bounds -> Coord
center ((mx, my), (nx, ny)) = (cx, cy)
    where cx = (mx - nx) / 2
          cy = (my - ny) / 2

bounds :: Segments -> Bounds
bounds ls = loop ((0, 0), (0, 0)) (concat ls)
    where loop result [] = result
          loop ((mx, my), (nx, ny)) ((x, y):xs)
            = loop ((mx', my'), (nx', ny')) xs
            where mx' = max x mx
                  my' = max y my
                  nx' = min x nx
                  ny' = min y ny

margin :: Float -> Bounds -> Float
margin pct ((mx, my), (nx, ny)) = abs $ pct * w
    where wx = mx - nx
          wy = my - ny
          w = max wx wy / 2

scale' :: Bounds -> Float
scale' = margin 1

applyMargin :: Float -> Bounds -> Bounds
applyMargin pct b@((mx, my), (nx, ny))
    = ((mx+w, my+w), (nx-w, ny-w))
    where w = margin pct b


--
-- Debugging
--

printCoords :: [StepResult] -> IO ()
printCoords =
    mapM_ (\s -> case s of
                    Point x y -> putStrLn $ "    (" ++ show x ++ ", " ++ show y ++ "),"
                    Reset x y -> putStrLn $ "    (nan, nan),\n    (" ++ show x ++ ", " ++ show y ++ "),"
                    Break -> putStrLn "    (nan, nan),")

colors :: [Color]
colors = cycle [red, green, blue, yellow, cyan, magenta, rose,
                violet, azure, aquamarine, chartreuse, orange]

drawBounds :: Bounds -> Picture
drawBounds ((mx, my), (nx, ny)) = Pictures ps
    where coords = [(nx, my), (mx, my),
                    (mx, ny), (nx, ny)]
          ps = [ Translate x y $ Color c $ circleSolid 2
               | ((x, y), c) <- zip coords colors]

main3 :: IO ()
main3 = do
    let example = parseRules [
                    ('X', "F[-X][X]F[-X]+FX"),
                    ('F', "FF")]
    let screen = (800, 800)
    let ps = lsys "X" example 9 (deg2rad 25)
    let b = applyMargin 0.1 (bounds ps)
    let c = center b
    let cx = round (fst c)
    let cy = negate $ round (snd c)
    let sc = 800* scale' b
    putStrLn $ "Bounds: " ++ show (bounds ps)
    putStrLn $ "Margin: " ++ show (margin 0.1 b)
    putStrLn $ "Bounds': " ++ show (applyMargin 0.1 b)
    putStrLn $ "Center': " ++ show c
    putStrLn $ "Center::Int: " ++ show (cx, cy)
    putStrLn $ "Scale: " ++ show (scale' b)
    let n = 0.04
    let s = Scale (n * scale' b) (n * scale' b)
    let ps' = Pictures [
                drawBounds $ bounds ps,
                Pictures $ zipWith Color colors (map Line ps)]
    display (InWindow "LSys" screen (-(cx*10), -(cy*10))) black (s ps')
