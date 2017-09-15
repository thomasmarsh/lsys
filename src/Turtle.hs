module Turtle
    ( parse
    , lsys
    , examples
    , main3
    ) where

import           Control.Monad (foldM)
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

type LSystem = (String, Float, Int, Rules)

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

_unparse :: Program -> String
_unparse = map _unparseChar

_unparseChar :: Instruction -> Char
_unparseChar Forward          = 'F'
_unparseChar ForwardBlank     = 'f'
_unparseChar CounterClockwise = '-'
_unparseChar Clockwise        = '+'
_unparseChar PushState        = '['
_unparseChar PopState         = ']'
_unparseChar (Variable x)     = x

parseRules :: Rules -> ParsedRules
parseRules r = M.fromList $ map f r
    where f (c, p) = (parseChar c, parse p)


--
-- Rule application
--

rewrite :: ParsedRules -> Program -> Program
rewrite r = foldr (\x -> (++) (M.findWithDefault [x] x r)) []

-- foldl is dramatically slower, but uses very little memory
-- rewrite r = foldl' (\z x -> z ++ M.findWithDefault [x] x r) []

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes n f x = f $ nTimes (n-1) f x

lsys :: LSystem -> Segments
lsys (prog, a, n, rules)
    = paths $ walk a iterated initState
    where iterated = nTimes n (rewrite (parseRules rules)) (parse prog)


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
    where cx = nx + (mx - nx) / 2
          cy = ny + (my - ny) / 2

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

scaleToScreen :: (Int, Int) -> Bounds -> Float
scaleToScreen (sx, sy) ((mx, my), (nx, ny)) = r'
    where r s m n = fromIntegral s / ((m-n) / 2)
          r' = min (r sx mx nx) (r sy my ny)

applyMargin :: Float -> Bounds -> Bounds
applyMargin pct b@((mx, my), (nx, ny))
    = ((mx+w, my+w), (nx-w, ny-w))
    where w = margin pct b


--
-- Drawing
--

drawBounds :: Bounds -> Picture
drawBounds ((mx, my), (nx, ny)) = Pictures ps
    where coords = [(nx, my), (mx, my),
                    (mx, ny), (nx, ny)]
          ps = [ Translate x y $ Color c $ circleSolid 2
               | ((x, y), c) <- zip coords colors]

colors :: [Color]
colors = repeat green
-- colors = cycle [ red, green, blue, yellow, cyan, magenta, rose,
--                  violet, azure, aquamarine, chartreuse, orange]


draw :: Segments -> (Int, Int) -> Picture
draw ps screen = Translate sx sy $ Scale sc sc $ Pictures [
                    drawBounds $ bounds ps,
                    Pictures $ zipWith Color colors (map Line ps)]
    where
        b = applyMargin 0.1 (bounds ps)
        sc = scaleToScreen screen b / 2
        c = center b
        cx = round $ fst c :: Integer
        cy = negate $ round $ snd c :: Integer
        sx = sc * fromIntegral cx
        sy = sc * fromIntegral cy


--
-- L-systems
--

hilbert :: LSystem
hilbert
    = ("L", deg2rad 90, 6, [
        ('L', "+RF-LFL-FR+"),
        ('R', "-LF+RFR+FL-")])

plant :: LSystem
plant
    = ("X", deg2rad 25, 6, [
        ('X', "F[-X][X]F[-X]+FX"),
        ('F', "FF")])

kochCurve :: LSystem
kochCurve
    = ("F", deg2rad 60, 6, [
        ('F', "F+F--F+F")])

kochIsland :: LSystem
kochIsland
    = ("F", deg2rad 90, 6, [
        ('F', "F-F+F+FFF-F-F+F)")])

curve32Segment :: LSystem
curve32Segment
    = ("F", deg2rad 90, 4, [
        ('F', "-F+F-F-F+F+FF-F+F+FF+F-F-FF+FF-FF+F+F-FF-F-F+FF-F-F+F+F-F+")])

peanoGosperCurve :: LSystem
peanoGosperCurve
    = ("X", deg2rad 60, 4, [
        ('X', "X+YF++YF-FX--FXFX-YF+"),
        ('Y', "-FX+YFYF++YF+FX--FX-Y")])

peanoCurve :: LSystem
peanoCurve
    = ("F", deg2rad 90, 5, [
        ('F', "F+F-F-F-F+F+F+F-F")])

hilbert2 :: LSystem
hilbert2
    = ("X", deg2rad 90, 5, [
        ('X', "XFYFX+F+YFXFY-F-XFYFX"),
        ('Y', "YFXFY-F-XFYFX+F+YFXFY")])

examples :: [LSystem]
examples
    = [ hilbert
      , plant
      , kochCurve
      , kochIsland
      , curve32Segment
      , peanoGosperCurve
      , peanoCurve
      , hilbert2
      ]

--
-- Debugging
--

_printCoords :: [StepResult] -> IO ()
_printCoords =
    mapM_ (\s -> case s of
                    Point x y -> putStrLn $ "    (" ++ show x ++ ", " ++ show y ++ "),"
                    Reset x y -> putStrLn $ "    (nan, nan),\n    (" ++ show x ++ ", " ++ show y ++ "),"
                    Break -> putStrLn "    (nan, nan),"
                    Nil -> putStrLn "    Nil,")



main3 :: IO ()
main3 = do
    let ps = lsys (last examples)
    let screen = (1000, 1000)
    display (InWindow "LSys" screen (0, 0)) black (draw ps screen)
