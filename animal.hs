import System.Random


type Position = (Int, Int)

data Animal = Animal Int Position

data SquareType = Grass | Wall

data Square = Square SquareType (Maybe Animal)

type Grid = [[Square]]

data Action = MoveUp | MoveDown | MoveLeft | MoveRight | Rest

type Time = Int

data World = World Time StdGen Grid


instance Show Animal where
    show (Animal id position) = take 3 $ show id ++ repeat ' '

instance Show SquareType where
    show Grass = " o "
    show Wall  = " x "

instance Show Square where
    show (Square squareType Nothing) = show squareType
    show (Square squareType (Just animal)) = show animal

instance Show World where
    show (World time fate grid) = unlines $ [" " ++ show time] ++ rows where
        rows = map (unwords . map show) grid


size :: [[a]] -> (Int, Int)
size list = (nx, ny) where
    nx = length $ head list  -- assumes nested lists have same length
    ny = length list

surroundWith :: a -> [a] -> [a]
surroundWith element list = reverse $ element:(reverse $ element:list)

--decide :: World -> Animal -> Action

--move :: World -> Animal -> Action -> World

--step :: World -> World

surroundWithSq :: Square -> Grid -> Grid
surroundWithSq square innerGrid = grid where
    gridWithSides = map (surroundWith square) innerGrid
    lengthWithSides = length $ head gridWithSides
    rowOfWalls = replicate lengthWithSides square
    grid = surroundWith rowOfWalls gridWithSides

newWorld :: Int -> Int -> Int -> World
newWorld seed nx ny = World time fate grid where
    time = 0
    fate = mkStdGen seed
    innerGrid = replicate ny $ replicate nx (Square Grass Nothing)
    wall = Square Wall Nothing
    grid = surroundWithSq wall innerGrid

main = do
    putStr $ show $ newWorld 1 10 10
