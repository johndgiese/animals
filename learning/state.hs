import System.Random
import Control.Monad.State

-- In this snippet, I figured out how to use the State monad when you have
-- functions that take multiple arguments; essentially, you use currying to
-- "attach" the state monad to the end, hence the state must be the "last
-- paramater".
--
-- NOTE: the standard libary doesn't export the "State" _value constructor_, so
-- you have to use "state" instead.  Of course the "State" _type constructor_
-- is available.

randBelow :: (RandomGen g, Num a, Random a) => a -> State g a
randBelow n = state $ randomR (1, n)

threeRandBelow :: (RandomGen g, Num a, Random a) => a -> State g (a, a, a)
threeRandBelow n = do
    a <- randBelow n
    b <- randBelow n
    c <- randBelow n
    return (a, b, c)

main = do putStrLn $ show $ runState (threeRandBelow (4 :: Int)) $ mkStdGen 10
