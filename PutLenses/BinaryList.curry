module BinaryList where

import Binary
-- import Prelude ( (*), (+), (.), fst, snd, failed, Bool(..) )
import Prelude hiding (foldr, head, tail, null)
import qualified Prelude as P

data BinaryList a = Empty | NonEmpty (L a)
data L a = LIHi a | LO (L (a,a)) | LI (L (a,a)) a

fromList :: [a] -> BinaryList a
fromList = P.foldr cons Empty

toList :: BinaryList a -> [a]
toList = foldr (:) []

length :: BinaryList a -> BinInt
length Empty           = Zero
length (NonEmpty list) = Pos (lengthL list)
 where
  lengthL :: L a -> Nat
  lengthL (LIHi _) = IHi
  lengthL (LO l)   = O (lengthL l)
  lengthL (LI l _) = I (lengthL l)

-- cons () _|_ = NonEmpty _|_
-- cons () (cons () _|_)
-- = cons () (NonEmpty _|_)
-- = NonEmpty (consL () _|_)
-- = NonEmpty _|_

cons :: a -> BinaryList a -> BinaryList a
cons x' xs' = NonEmpty (cons' x' xs')
 where
  cons' x Empty           = LIHi x
  cons' x (NonEmpty list) = consL x list
  consL :: a -> L a -> L a
  consL x (LIHi y)  = LO (LIHi (x,y))
  consL x (LO  xs)  = LI xs x
  consL x (LI xs y) = LO (consL (x,y) xs)

decons :: BinaryList a -> (a, BinaryList a)
decons Empty                    = failed
decons (NonEmpty (LIHi x))      = (x,Empty)
decons (NonEmpty list@(LO _))   = (\(x,y) -> (x, NonEmpty y)) $ deconsL list
decons (NonEmpty list@(LI _ _)) = (\(x,y) -> (x, NonEmpty y)) $ deconsL list

deconsL :: L a -> (a, L a)
deconsL xs' = case xs' of
  LIHi _          -> failed
  LI xs x         -> (x, LO xs)
  LO (LIHi (x,y)) -> (x, LIHi y)
  LO xs           -> let ((x',y'),ys) = deconsL xs
                     in (x', LI ys y')

head :: BinaryList a -> a
head = fst . decons

tail :: BinaryList a -> BinaryList a
tail = snd . decons

null :: BinaryList a -> Bool
null Empty        = True
null (NonEmpty _) = False

foldr :: (a -> b -> b) -> b -> BinaryList a -> b
foldr f e list | null list = e
               | otherwise = f x (foldr f e xs)
 where
  (x,xs) = decons list

(++) :: BinaryList a -> BinaryList a -> BinaryList a
xs ++ ys = foldr cons ys xs

drop :: BinInt -> BinaryList a -> BinaryList a
drop Zero list                   = list
drop n@(Pos _) list@(NonEmpty _) = drop (dec n) (snd (decons list))
drop (Pos _) Empty               = Empty
