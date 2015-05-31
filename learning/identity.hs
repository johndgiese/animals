
data Identity a = Identity {getIdentity :: a}

instance Monad Identity where
    return x = Identity x
    Identity x >>= f = f x

testIdentity :: Identity Int
testIdentity = do
    x <- Identity 4
    y <- Identity 3
    Identity (x + y)

testIdentity2 :: Identity Int
testIdentity2 = Identity 4 >>= (\x -> (Identity 3 >>= (\y -> Identity (x + y))))

main = putStrLn $ show $ getIdentity testIdentity2
