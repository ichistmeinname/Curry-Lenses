module Height where

import Monadic

putHeight1 :: Lens (Int,Int) Int
putHeight1 = keepFst

putHeight2 :: Lens (Int,Int) Int
putHeight2 = addFst (\p v ->
  maybe failed
        (uncurry (putSquare v))
        p)
 where
  putSquare v s1 s2 | s1 == s2  = v
                    | otherwise = failed

putHeight3 :: Int -> Lens (Int,Int) Int
putHeight3 i = addFst (\p v ->
  maybe failed
        (uncurry (putChange v))
        p)
 where
  putChange v s1 s2 | v == s2   = s1
                    | otherwise = i

-----
test1 :: Int -> IO ()
test1 x = test (Just (10,x)) 42

test s v = do
  print (putHeight1 s v)
  print (putHeight2 s v)
  print (putHeight3 0 s v)