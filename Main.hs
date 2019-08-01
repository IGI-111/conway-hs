module Main where
import           Data.List.Split
import           Data.List

data Board = Board { field :: [Bool], width :: Int }
type Coord = (Int, Int)

instance Show Board where
        show (Board { field = f, width = w }) = intercalate "\n" lines where
                        lines = chunksOf w (map (\s -> if s then 'X' else '.') f)

at :: Board -> Coord -> Bool
at (Board { field = f, width = w }) (x, y) = f !! (x + w * y)

iterate :: Board -> Board
iterate (Board { field = f, width = w }) = Board {field = map not f, width = w}

main :: IO ()
main = print b
 where
  b = Board
    { field = [False, True, False, True, False, False, True, False, True]
    , width = 3
    }
