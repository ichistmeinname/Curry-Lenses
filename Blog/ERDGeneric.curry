------------------------------------------------------------------------------
--- Generic operations and integrity tests
--- to support the database code generated from ERDs
------------------------------------------------------------------------------

module ERDGeneric where

import KeyDatabase
import List
import ReadShowTerm
import Read
import Char(isDigit)

------------------------------------------------------------------------------
-- Handling of database keys

--- The general type of database keys.
type Key = Int

--- Shows a database key for an entity name as a string.
--- Useful if a textual representation of a database key is necessary,
--- e.g., as URL parameters in web pages. This textual representation
--- should not be used to store database keys in attributes!
showDatabaseKey :: String -> (enkey -> Key) -> enkey -> String
showDatabaseKey en fromenkey enkey = en ++ show (fromenkey enkey)

--- Transforms a string into a key for an entity name.
--- Nothing is returned if the string does not represent a reasonable key.
readDatabaseKey :: String -> (Key -> enkey) -> String -> Maybe enkey
readDatabaseKey en toenkey s =
  let (ens,ks) = splitAt (length en) s
   in if ens==en && all isDigit ks then Just (toenkey (readNat ks))
                                   else Nothing


------------------------------------------------------------------------------
-- Generic operations to modify the database

--- Insert a new entity and assign a new key for it.
newEntry :: (Key -> t -> Dynamic) -> (Key -> t -> en) -> t -> Transaction en
newEntry pred info2entry info =
  newDBEntry pred info |>>= \k -> returnT (info2entry k info)

-- Insert new relationship represented as an entity.
newEntryR :: (Key -> (a,b) -> Dynamic) -> a -> b -> Transaction ()
newEntryR entrypred key1 key2 = newDBEntry entrypred (key1,key2) |>> doneT

getEntry :: (Key -> t -> Dynamic) -> (Key -> t -> en) -> Key -> Transaction en
getEntry pred info2entry key = seq pred $ seq key $
  getDB (getDBInfo pred key) |>>=
  maybe (errorT (TError KeyNotExistsError
                        ("database contains no entry for key: "++show key)))
        (\info -> returnT (info2entry key info))

-- Delete a relationship represented as an entity.
-- If the relationship does not exist, a NoRelationshipError is raised.
deleteEntryR :: (Key -> (a,b) -> Dynamic) -> a -> b -> Transaction ()
deleteEntryR entrypred key1 key2 =
  getDB (transformQ (map fst . filter (\ (_,i) -> i==(key1,key2)))
                    (allDBKeyInfos entrypred)) |>>= \kis ->
  if null kis
   then errorT (TError NoRelationshipError 
                       ("relationship for deletion not found for keys: "
                        ++show key1++" "++show key2))
   else deleteDBEntries entrypred kis


------------------------------------------------------------------------------
-- Generic integrity tests for keys.

-- If there is no entry with a given key, raise a transaction error.
existsEntryWithDBKey :: String -> (Key -> t -> Dynamic) -> Key -> Transaction ()
existsEntryWithDBKey ename entrypred key =
  getDB (getDBInfo entrypred key) |>>=
  maybe (errorT (TError KeyNotExistsError
                        ("database contains no entry for key: "++show key 
                         ++" in table: "++ename))  )
        (const doneT)

-- If a given key occurs in a (foreign key) attribute of an entity,
-- raise a transaction error.
requiredForeignDBKey :: String -> (Key -> t -> Dynamic) -> (Key -> t -> en)
                     -> (en -> k) -> k -> Transaction ()
requiredForeignDBKey ename entrypred info2entry keyf key =
  getDB (getAllEntities entrypred info2entry) |>>= \ens ->
  if null (filter (\e -> keyf e == key) ens)
   then doneT
   else errorT (TError KeyRequiredError
                       ("key: "++show key ++ " required in table: " ++ ename))

getAllEntities :: (Key -> t -> Dynamic) -> (Key -> t -> en) -> Query [en]
getAllEntities entrypred info2entry =
  transformQ (map (uncurry info2entry)) (allDBKeyInfos entrypred)

duplicateKeyTest :: (Key -> t -> Dynamic) -> Transaction ()
duplicateKeyTest pred =
  getDB (allDBKeys pred) |>>= \keys ->
  if length (nub keys) == length keys
     then doneT
     else errorT (TError DuplicateKeyError
                             ("database contains duplicate key for table: " 
                                ++show pred)) 
     
duplicatePTest :: [a] -> Transaction ()
duplicatePTest xs =
  if length (nub xs) == length xs
  then doneT
  else errorT (TError DuplicateKeyError "duplicate parameters in new-function")

-------------------------------------------------------------------------
-- Uniqueness tests.
 
-- Test whether an attribute value does not yet exist
unique :: String -> (Key -> t -> Dynamic) -> (Key -> t -> en) -> (en -> a) -> a
       -> Transaction ()
unique ename entrypred info2entry selector attrval =
  getDB (allDBKeyInfos entrypred) |>>= \kis ->
  if null (filter (\e -> selector e == attrval)
                  (map (\(k,i) -> info2entry k i) kis))
   then doneT
   else errorT (TError UniqueError
                       (ename++" entry for unique attribute "
                        ++show attrval++" already exists"))

uniqueUpdate :: String -> (Key -> t -> Dynamic) -> (Key -> t -> en)
             -> (en -> Key) -> (en -> a) -> en -> Transaction ()
uniqueUpdate ename entrypred info2entry keyf selector obj =
  let oldkey = keyf obj
  in
  getDB (getDBInfo entrypred oldkey) |>>=
  maybe (errorT (TError KeyNotExistsError
                    ("database contains no entry for key: "++show oldkey)))
        (\oldt -> getDB (allDBKeyInfos entrypred) |>>= \kis ->
           let oldentry = info2entry oldkey oldt
               entries = filter (\e -> selector obj == selector e)
                                (map (uncurry info2entry) kis)
            in if null entries ||
                  (length entries == 1 && selector oldentry == selector obj)
               then doneT
               else errorT (TError UniqueError
                                   (ename++" entry for unique attribute "
                                    ++show (selector obj)++" already exists")))

uniqueC :: String -> (Key -> t -> Dynamic) -> (Key -> t -> en)
        -> (en -> a) -> en -> Transaction ()
uniqueC ename entrypred info2entry selector obj =
  getDB (allDBKeyInfos entrypred) |>>= \kis ->
  let entries = filter (\e -> selector obj == selector e)
                       (map (uncurry info2entry) kis)
   in if length entries <= 1
      then doneT
      else errorT (TError UniqueError
                     (ename++" unique attribute "
                      ++show (selector obj)++" is not unique"))

-- Uniqueness of a combination of two attributes.
-- Check whether this combination already exists.
-- If it exists, a transaction error is generated, otherwise everything is ok.
unique2 :: (Key -> (a,b) -> Dynamic) -> a -> b -> Transaction ()
unique2 entrypred k1 k2 =
  getDB (allDBInfos entrypred) |>>= \is ->
  if null (filter (== (k1,k2)) is)
   then doneT
   else errorT (TError UniqueError "relationship already exists")

unique2C :: (Key -> (a,b) -> Dynamic) ->  a -> b -> Transaction ()
unique2C entrypred k1 k2 =
  getDB (allDBInfos entrypred) |>>= \is ->
  if length (filter (== (k1,k2)) is) > 1
   then errorT (TError UniqueError "relationship not unique")
   else doneT

-------------------------------------------------------------------------
-- Maximum and minimum tests.

maxPTest :: Int -> [a] -> Transaction ()
maxPTest max xs = 
  if length xs > max
  then errorT (TError MaxError "max reached in parameter list in new function")
  else doneT


maxTest :: String -> (Key -> t -> Dynamic) -> (Key -> t -> en)
        -> (en -> a) -> Int -> a -> Transaction ()
maxTest ename entrypred info2entry selector max attr =
  getDB (getAllEntities entrypred info2entry) |>>= \es ->
  let entries = filter (\e -> attr == selector e) es in 
  if length entries < max
   then doneT
   else errorT (TError MaxError ("max reached for attribute " 
                                 ++show attr++" in entity "++ename))

maxTestUpdate :: String -> (Key -> t -> Dynamic) -> (Key -> t -> en)
              -> (en -> Key) -> (en -> a) -> Int -> en -> Transaction ()
maxTestUpdate ename entrypred info2entry keyf selector max obj =
  getDB (getAllEntities entrypred info2entry) |>>= \es ->
  let entries = filter (\e -> selector obj == selector e) es in 
  getEntry entrypred info2entry (keyf obj) |>>= \old ->
  if (length entries < max
        || (length entries == max && selector old == selector obj))
   then doneT
   else errorT (TError MaxError ("max reached for attribute "
                                 ++show (selector obj)++" in entity "++ename))

maxTestC :: String -> (Key -> t -> Dynamic) -> (Key -> t -> en)
         -> (en -> a) -> Int -> a -> Transaction ()
maxTestC ename entrypred info2entry selector max attr =
  getDB (getAllEntities entrypred info2entry) |>>= \es ->
  if length (filter (\e -> selector e == attr) es) <= max
   then doneT
   else errorT (TError MaxError ("maximum exceeded for attribute " 
                                 ++show attr++" in entity "++ename))

minTestC :: String -> (Key -> t -> Dynamic) -> (Key -> t -> en)
         -> (en -> a) -> Int -> a -> Transaction ()
minTestC ename entrypred info2entry selector min attr =
  getDB (getAllEntities entrypred info2entry) |>>= \es ->
  if length (filter (\e -> selector e == attr) es) >= min
     then doneT
     else errorT (TError MinError ("below min for attribute " 
                                   ++show attr++" in entity "++ename))

-- Maximum test before inserting a relationship with a given key:
maxTestInsert :: String -> (Key -> t -> Dynamic) -> (Key -> t -> en)
              -> (en -> a) -> Int -> a -> Transaction ()
maxTestInsert ename entrypred info2entry selector maxrange attr =
  getDB (getAllEntities entrypred info2entry) |>>= \es ->
  if length (filter (\e -> selector e == attr) es) < maxrange
   then doneT
   else errorT (TError MaxError ("maximum reached for attribute " 
                                 ++show attr++" in entity "++ename))

-- Minimum test before deleting a relationship
minTestDelete :: String -> (Key -> t -> Dynamic) -> (Key -> t -> en)
              -> (en -> a) -> Int -> a -> Transaction ()
minTestDelete ename entrypred info2entry selector min attr =
  getDB (getAllEntities entrypred info2entry) |>>= \es ->
  if length (filter (\e -> selector e == attr) es) > min
     then doneT
     else errorT (TError MinError ("below min for attribute " 
                                   ++show attr++" in entity "++ename))


-------------------------------------------------------------------------
-- Saving and restoring dynamic predicates.

saveDBTerms :: String -> String -> (Key -> a -> Dynamic)
            -> (Key -> a -> _) -> IO ()
saveDBTerms path ename dynpred toentity = do
  keyinfos <- runQ (allDBKeyInfos dynpred)
  let savefile = path++"/"++ename++".terms"
      terms = map (uncurry toentity) keyinfos
  if null path
   then putStrLn (unlines (map showQTerm terms)) -- show only
   else do putStrLn $ "Saving into "++savefile
           writeQTermListFile savefile terms

restoreDBTerms :: String -> String -> (Key -> a -> Dynamic)
               -> (en->Key) -> (en->a)  -> IO ()
restoreDBTerms path ename dynpred enkey eninfo = do
  let savefile = path++"/"++ename++".terms"
  putStrLn $ "Restoring from "++savefile
  terms <- readQTermListFile savefile
  runJustT (mapT_ (\t -> newDBKeyEntry dynpred (enkey t) (eninfo t)) terms)

restoreDBRelTerms :: String -> String -> (Key -> a -> Dynamic)
                  -> (en->a)  -> IO ()
restoreDBRelTerms path ename dynpred eninfo = do
  let savefile = path++"/"++ename++".terms"
  putStrLn $ "Restoring from "++savefile
  terms <- readQTermListFile savefile
  runJustT (mapT_ (\t -> newDBEntry dynpred (eninfo t)) terms)

-------------------------------------------------------------------------
-- If the second argument is a null string, return the first argument
-- (the default string), otherwise return the second argument.
defaultString :: String -> String -> String
defaultString def s = if null s then def else s

-------------------------------------------------------------------------
