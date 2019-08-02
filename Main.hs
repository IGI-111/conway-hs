module Main where
import           Prelude
import qualified Data.Vector                   as Vector
import           Data.Vector                    ( Vector
                                                , (!)
                                                , fromList
                                                , toList
                                                )
import           Data.List                      ( intercalate )
import           Debug.Trace

data Board = Board (Vector (Vector Bool))
type Coord = (Int, Int)

instance Show Board where
  show (Board v) = intercalate "\n" lines where
    lines = map (\l -> map (\x -> if x then 'X' else '.') $ toList l) $ toList v

width :: Board -> Int
width (Board v) = length (v ! 0)

height :: Board -> Int
height (Board v) = length v

at :: Board -> Coord -> Bool
at (Board v) (x, y) = (v ! mod y (height b)) ! mod x (width b)
  where b = Board v

neighbours :: Board -> Coord -> [Bool]
neighbours b (x, y) = v
 where
  v = map
    (at b)
    [ (x - 1, y)
    , (x + 1, y)
    , (x    , y - 1)
    , (x    , y + 1)
    , (x + 1, y + 1)
    , (x - 1, y - 1)
    , (x + 1, y - 1)
    , (x - 1, y + 1)
    ]

tick :: Board -> Board
tick b = Board $ Vector.map (\v -> Vector.map iterateCell v) coordinates
 where
  coordinates = fromList
    [ fromList [ (x, y) | x <- [0 .. (width b) - 1] ]
    | y <- [0 .. (height b) - 1]
    ]
  iterateCell c = (alive && (n == 2 || n == 3)) || (not alive) && n == 3
   where
    n     = length $ filter id $ neighbours b c
    alive = at b c


ticks :: Board -> Int -> [Board]
ticks init n =
  reverse $ foldl (\acc _ -> (tick $ head acc) : acc) [init] [1 .. n]

main :: IO ()
main = do
  putStrLn $ intercalate "\n\n" $ map show $ ticks b 30
 where
  b = Board $ fromList $ map
    fromList
    [ [False, False, False, False, False, False, False]
    , [False, False, False, False, False, False, False]
    , [False, False, False, False, False, False, False]
    , [False, False, False, False, False, False, False]
    , [False, False, False, False, False, False, False]
    , [False, False, False, True, False, False, False]
    , [False, False, True, False, False, False, False]
    , [False, False, True, True, True, False, False]
    , [False, False, False, False, False, False, False]
    , [False, False, False, False, False, False, False]
    ]
