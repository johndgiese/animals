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
    show (World time grid) = unlines $ [" " ++ show time] ++ rows where
        rows = map (unwords . map show) grid


size :: [[a]] -> (Int, Int)
size list = (nx, ny) where
    nx = length $ head grid - 1  -- assumes nested lists have same length
    ny = length grid - 1

surroundWith :: a -> [a] -> [a]
surroundWith element list = reverse $ element:(reverse $ element:list))

randomRn :: (RandomGen g, Random a, Num n) => g -> n -> (a, a) -> ([a], g)
randomRn gen 0 _ = ([], gen)
randomRn gen n range =
    let (value, newGen) = randomR range gen
        (restOfList, finalGen) = randomRn (n - 1) range newGen
    in  (value:restOfList, finalGen)


--decide :: World -> Animal -> Action

--move :: World -> Animal -> Action -> World

--step :: World -> World

newWorld :: Int -> Int -> Int -> Int -> World
newWorld seed nx ny numAnimals = World time fate' grid where
    time = 0
    fate = mkStdGen seed
    innerGrid = replicate ny $ replicate nx (Square Grass Nothing)
    (innerGridWithAnimals, fate') = populateWithAnimals fate numAnimals innerGrid
    grid = surroundWithWalls innerGridWithAnimals

surroundWithWalls :: Grid -> Grid
surroundWithWalls innerGrid = grid where
    wall = Square Wall Nothing
    gridWithSideWalls = map (surroundWith wall) innerGrid
    rowOfWalls = replicate (length (head gridWithSideWalls)) wall
    grid = surroundWith rowOfWalls gridWithSideWalls

populateWithAnimals :: StdGen -> Int -> Grid -> (Grid, StdGen)
populateWithAnimals fate numAnimals grid = (gridWithAnimals, fate') where
    (nx, ny) = size grid


main = do
    putStr $ show $ newWorld 1 10 10 3
