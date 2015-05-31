import Data.Monoid

newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    Writer (x, w) >>= f = Writer (xx, w `mappend` ww) where Writer (xx, ww) = f x

addAndLog :: Int -> Int -> Writer [String] Int
addAndLog a b = Writer (a + b, log) where
    log = [(show a) ++ " + " ++ (show b) ++ " = " ++ (show $ a + b)]

testWriter = do
    x <- addAndLog 3 5
    y <- addAndLog 10 100
    addAndLog x y

testWriter2 = addAndLog 3 5 >>= \x -> addAndLog 10 100 >>= \y -> addAndLog x y

main = putStr $ unlines $ snd $ runWriter $ testWriter2

