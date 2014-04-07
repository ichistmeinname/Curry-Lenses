undefined = failed

test = undefined

test2 x = return (x,xs)
 where
  xs = undefined

test3 = return $ test test

test4 :: Int
test4 = failed failed