module People where

import SetFunctions
import Monadic

------------------------------------------------------------
------------------------------------------------------------
------------------------------------------------------------

data Person = Person Name City
type Name = String
type City = String
-- F Person = String x String

-- inPerson :: Lens Person (Name, City)
-- inPerson s v = maybe failed (\_ -> (uncurry Person) v) s

-- outPerson :: Lens (Name, City) Person
-- outPerson mPair p = case mPair of
--   Nothing          -> case p of
--                            Person name' city' -> (name',city')
--   Just pair        -> pair

inPerson :: Lens Person (Name, City)
inPerson _ = uncurry Person

outPerson :: Lens (Name, City) Person
outPerson _ p = case p of
  Person n c -> (n, c)

inList :: Lens [a] (Either () (a,[a]))
inList xs eVal = either (\() -> []) (\(x',xs') -> x':xs') eVal

outList :: Lens (Either () (a,[a])) [a]
outList eVal xs = case xs of
  []     -> Left ()
  (x:xs) -> Right (x,xs)

mapPerson :: Lens Person Name -> Lens [Person] [Name]
mapPerson f = (inList <.> (idLens <+> (f <*> mapPerson f)) <.> outList)
-- mapPerson = undefined
-- (<+>) :: Lens s1 v1 -> Lens s2 v2 -> Lens (Either s1 s2) (Either v1 v2)
-- (<*>) :: Lens s1 v1 -> Lens s2 v2 -> Lens (s1,s2) (v1,v2)


-- f :: Lens Person Name
-- g =!= f <*> mapPerson f:: Lens (Person, [Person]) (Name,[Name])
-- h =!= idLens <+> g :: Lens (Either s1 (Person, [Person])) (Either v1 (Name, [Name]))
-- inList <.> h :: Lens [a] (Either () (a, [a]) -> Lens (Either s1 (Person, [Person])) (Either v1 (Name, [Name])) -> Lens [a] (Either v1 (Name, [Name]))

people  = [bastian, lennart, julia]
bastian = Person "Bastian" "Gaarden"
lennart = Person "Lennart" "Kronshagen"
julia   = Person "Julia"   "Schreventeich"

-- Maybe Person -> String -> Person
name :: Lens Person Name
name p = (inPerson <.> keepSnd) p

city :: Lens Person City
city p = (inPerson <.> keepFst) p

inout :: Lens Person Person
inout p = (inPerson <.> outPerson) p

peopleNames :: City -> Lens [Person] [Name]
peopleNames c ps = mapPerson (inPerson <.> addSnd cityOf) ps
 where
  cityOf s _ = maybe c snd s

------------------------------------------------------------
---------------------- Examples ----------------------------
------------------------------------------------------------

mainTest = do
  putStrLn $ nameTestPut lennart "Lennart Eric Dorian" [(Person "Lennart Eric Dorian" "Kronshagen")]
  putStrLn $ nameTestGet julia ["Julia"]
  putStrLn $ cityTestPut bastian "Kiel" [(Person "Bastian" "Kiel")]
  putStrLn $ cityTestGet lennart ["Kronshagen"]
  putStrLn $ inoutTestPut1 lennart julia [julia]
  putStrLn $ inoutTestPut2 bastian [bastian]
  putStrLn $ inoutTestGet julia [julia]
  putStrLn $ mapTestPut "Lyon"
                     [bastian, julia]
                     ["Bastian", "Hugo", "Julia"]
                     [[(Person "Bastian" "Gaarden")
                     ,(Person "Hugo" "Schreventeich")
                     ,(Person "Julia" "Lyon")]]
  putStrLn $ mapTestGet "Schreventeich" people [map (\(Person c _) -> c) people]

testPut :: Lens a b -> a -> b -> [a] -> String
testPut f p1 p2 es =
  (if erg == es then "Test succeeded:\n" else "Test failed:\n") ++ show erg
 where
  erg = sortValues $ set3 put f (Just p1) p2

testGet :: Lens a b -> a -> [b] -> String
testGet f p1 es =
  (if erg == es then "Test succeeded:\n" else "Test failed:\n") ++ show erg
 where
  erg = sortValues $ set2 get f p1

inoutTestPut1 p1 p2 es = testPut inout p1 p2 es
inoutTestPut2 p1    es = testPut inout p1 p1 es
inoutTestGet  p1    es = testGet inout p1    es

nameTestPut p1 p2 es = testPut name p1 p2 es
nameTestGet p1    es = testGet name p1    es

cityTestPut p1 p2 es = testPut city p1 p2 es
cityTestGet p1    es = testGet city p1    es

mapTestPut city p1 p2 es = testPut (peopleNames city) p1 p2 es
mapTestGet city p1    es = testGet (peopleNames city) p1    es