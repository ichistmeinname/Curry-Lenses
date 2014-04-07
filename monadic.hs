import Generics.Putlenses.Language
import Generics.Putlenses.Putlens

putHeight1 :: PutlensM [] (Int,Int) Int
putHeight1 = keepfstPut

putHeight2 :: PutlensM [] (Int,Int) Int
putHeight2 = addfstPut (\p v ->
  maybe (fail "")
        (uncurry (putSquare v))
        p)
 where
  putSquare v s1 s2 | s1 == s2  = return v
                    | otherwise = []

putHeight3 :: Int -> PutlensM [] (Int,Int) Int
putHeight3 i = addfstPut (\p v ->
  maybe (fail "")
        (return . uncurry (putChange v))
        p)
 where
  putChange v s1 s2 | v == s2   = s1
                    | otherwise = i
