module Main where
import           Data.List.Split
import           Data.List

data Board = Board { field :: [Bool], width :: Int }
type Coord = (Int, Int)

instance Show Board where
        show (Board { field = f, width = w }) = intercalate "\n" lines where
                        lines = chunksOf w (map (\s -> if s then 'X' else '.') f)

at :: Board -> Coord -> Bool
at b (x, y) = field b !! ((x `mod` w) + w * (y `mod` h))
 where
  h = height b
  w = width b

neighbours :: Board -> Coord -> [Bool]
neighbours b (x, y) = map
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

height :: Board -> Int
height b = (length (field b)) `quot` width b

coords :: Board -> Int -> Coord
coords b i = (mod i $ width b, quot i $ width b)

tick :: Board -> Board
tick b = Board
  { field = map iterateCell (zip [1 ..] $ field b)
  , width = width b
  }
 where
  iterateCell (i, alive) = alive && n >= 2 && n <= 3 || (not alive) && n == 3
    where n = length $ filter id $ neighbours b (coords b i)

ticks :: Board -> Int -> [Board]
ticks init n = foldl (\acc _ -> (tick $ head acc) : acc) [init] [1 .. n]

main :: IO ()
main = do
  putStrLn $ intercalate "\n\n" $ map show $ ticks b 4
 where
  b = Board
    { field = [ False
              , True
              , False
              , False
              , True
              , False
              , False
              , False
              , True
              , False
              , True
              , False
              , False
              , False
              , False
              , False
              ]
    , width = 4
    }
