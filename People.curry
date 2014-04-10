module People where

import SetFunctions (set0, set1, set2, sortValues, chooseValue, Values(..))
import Monadic
import Maybe (fromMaybe)
------------------------------------------------------------
------------------------------------------------------------
------------------------------------------------------------

data Person = Person Name City
type Name = String
type City = String

inPerson :: Lens Person (Name, City)
inPerson = isoLens inn out
 where
  inn (n,c)        = Person n c
  out (Person n c) = (n,c)

outPerson :: Lens (Name, City) Person
outPerson = isoLens out inn
 where
  inn (n,c)        = Person n c
  out (Person n c) = (n,c)

people  = [bastian, lennart, julia]
bastian = Person "Bastian" "Gaarden"
lennart = Person "Lennart" "Kronshagen"
julia   = Person "Julia"   "Schreventeich"
-- julia   = Person "Julia"   "Kiel"

nameOrCity :: Lens Person String
nameOrCity = name ? city

name :: Lens Person Name
name = inPerson <.> keepSnd

city :: Lens Person City
city = inPerson <.> keepFst

inout :: Lens Person Person
inout = inPerson <.> outPerson

peopleNames :: City -> Lens [Person] [Name]
peopleNames c = mapLens (inPerson <.> addSnd cityOf)
 where
  cityOf s _ = maybe c snd s

------------------------------------------------------------
---------------------- Examples ----------------------------
------------------------------------------------------------

mainTest = do
  -- putStrLn $ nameTestPut lennart "Lennart Eric Dorian" [(Person "Lennart Eric Dorian" "Kronshagen")]
  -- putStrLn $ nameTestGet julia ["Julia"]
  -- putStrLn $ cityTestPut bastian "Kiel" [(Person "Bastian" "Kiel")]
  -- putStrLn $ cityTestGet lennart ["Kronshagen"]
  -- putStrLn $ inoutTestPut1 lennart julia [julia]
  -- putStrLn $ inoutTestPut2 bastian [bastian]
  -- putStrLn $ inoutTestGet julia [julia]
  putStrLn $ mapTestPut "Lyon"
                     [bastian, julia]
                     ["Bastian", "Julia"]
                     [[(Person "Bastian" "Gaarden")
                      ,(Person "Julia" "Schreventeich")
                      ]]
  putStrLn $ mapTestGet "Schreventeich" people [map (\(Person c _) -> c) people]

testPut :: Lens a b -> a -> b -> [a] -> String
testPut l p1 p2 es =
  (if erg == es then "Test succeeded:\n" else "Test failed:\n") ++ show erg
 where
  erg = sortValues $ set2 (put' l) (Just p1) p2

testGet :: Lens a b -> a -> [b] -> String
testGet l p1 es =
  (if erg == es then "Test succeeded:\n" else "Test failed:\n") ++ show erg
 where
  erg = sortValues $ set1 (get' l) p1

inoutTestPut1 p1 p2 es = testPut inout p1 p2 es
inoutTestPut2 p1    es = testPut inout p1 p1 es
inoutTestGet  p1    es = testGet inout p1    es

nameTestPut p1 p2 es = testPut name p1 p2 es
nameTestGet p1    es = testGet name p1    es

cityTestPut p1 p2 es = testPut city p1 p2 es
cityTestGet p1    es = testGet city p1    es

mapTestPut c p1 p2 es = testPut (peopleNames c) p1 p2 es
mapTestGet c p1    es = testGet (peopleNames c) p1    es