module GenerateLenses (
  writeFileForModule
  )
where

import FlatCurryRead (readFlatCurryWithImports)
import FlatCurry ( Prog(..), FuncDecl(..), TypeDecl(..), TypeExpr(..), Expr(..)
                 , QName, Visibility(..), CombType(..), Rule(..), ConsDecl(..)
                 , BranchExpr(..), Pattern(..), CaseType( Flex )
                 , writeFCY, showQNameInModule )
import FlatCurryGoodies ( progName, progFuncs
                        , funcType, funcName, funcRule
                        , typeName, typeConsDecls, typeParams
                        , consArgs
                        , updProgImports, updProgFuncs, updProgTypes )
import FlatCurryPretty ( pPrint, ppProg )

import List ( isPrefixOf, deleteBy, replace )
import Char ( toLower )

type ModuleName = String
type FilePath   = String

writeFileForModule :: ModuleName -> IO ()
writeFileForModule mName = do
  prog <- readFlatCurryForModule mName
  let newProg = addImport (transformRecords prog)
  putStrLn (pPrint (ppProg newProg))
  writeFCY (mName ++ ".fcy") newProg

readFlatCurryForModule :: ModuleName -> IO Prog
readFlatCurryForModule moduleName = do
  progs <- readFlatCurryWithImports moduleName
  case filter (\p -> moduleName == progName p) progs of
       [p] -> return p
       _   -> error ("Couldn't find module with name " ++ moduleName ++ ".")

addImport :: Prog -> Prog
addImport = updProgImports ("Lens" :)

transformRecords :: Prog ->  Prog
transformRecords prog@(Prog _ _ ts fs _) =
  let recFs = filter isSelectFunction fs
      newFs = generateLensesForDatatypes (filter (not . isRecordType) ts)
                                         recFs
  in updProgFuncs (\fs' -> foldr (:) fs' newFs) prog

generateLensesForDatatypes :: [TypeDecl]
                           -> [FuncDecl]
                           -> [FuncDecl]
generateLensesForDatatypes ts fs =
  let (recs,datas)  = split (\t -> typeNameOnly t `elem` (map stripPrefix fs))
                            ts
      pairs         = map (\func -> lexRecordLabel func) fs
      namesAndFuncs = map (generateFuncNamesFromRecord pairs) ts
      funcDecls     = concatMap (uncurry generateFuncDeclsFromDatatype)
                                namesAndFuncs
  in funcDecls
 where
  lexRecordLabel :: FuncDecl -> (String,QName)
  lexRecordLabel funcDecl =
    (stripPrefix funcDecl,(fst $ funcName funcDecl,strip '.' funcDecl))
  strip :: Char -> FuncDecl -> String
  strip c = tail . dropWhile (/= c) . funcNameOnly
  stripPrefix :: FuncDecl -> String
  stripPrefix = takeWhile (/= '.') . strip '@'
  split :: (a -> Bool) -> [a] -> ([a],[a])
  split p xs = (filter p xs, filter (not . p) xs)

generateFuncNamesFromRecord :: [(String,QName)]
                            -> TypeDecl
                            -> ([Maybe QName],TypeDecl)
generateFuncNamesFromRecord fdMap t =
  let names = lookupAll (typeNameOnly t) fdMap
  in (names,t)
 where
  lookupAll :: k -> [(k,a)] -> [Maybe a]
  -- lookupAll key = foldr (\pair@(key',value) vs ->
  --                          if key == key'
  --                            then pair : vs
  --                            else vs)
  lookupAll _   []               = []
  lookupAll key ((key',value):kvs)
    | key == key' = Just value : lookupAll key kvs
    | otherwise   = lookupAll key kvs

generateFuncDeclsFromDatatype:: [Maybe QName] -> TypeDecl -> [FuncDecl]
generateFuncDeclsFromDatatype names typeDecl =
  concatMap (genFromDatatype names (typeName typeDecl)) $ typeConsDecls typeDecl

genFromDatatype :: [Maybe QName] -> QName -> ConsDecl -> [FuncDecl]
genFromDatatype names tName (Cons constrName@(mName,cName) arity _ constrs)
  | length names == length constrs =
      concatMap (uncurry genFromField)
                (zip [1..] (zip names constrs))
  | null names = concatMap (uncurry genFromField)
                           (zip [1..] (zip (repeat Nothing) constrs))
  | otherwise = error "This shouldn't happen"
 where
  genFromField :: Int -> (Maybe QName,TypeExpr) -> [FuncDecl]
  genFromField no (maybeName,tExpr) =
    let source           = typeFromName tName
        getTypeExpr      = getType source tExpr
        putTypeExpr      = putType source tExpr
        qName = case maybeName of
                     Just name -> name
                     Nothing   -> ( mName
                                  , map toLower cName ++ "Acc" ++ (show no)
                                  )
        (getName,getFct) = lambdaFunction qName
                                          1
                                          getTypeExpr
                                          (getRule constrName arity no)
        (putName,putFct) = lambdaFunction qName
                                          2
                                          putTypeExpr
                                          (putRule constrName arity no)
    in [ Func qName
              0
              Public
              (TCons ("Prelude","(,)") [getTypeExpr, putTypeExpr])
              (lensRule getName putName)
       , getFct
       , putFct ]

lambdaFunction :: QName -> Int -> TypeExpr -> Rule -> (QName,FuncDecl)
lambdaFunction (mName,fName) funArity tExpr rule =
  (name,Func name funArity Private tExpr rule)
 where
  name = (mName,fName ++ "_#lambda" ++ show funArity)

getRule :: QName -> Int -> Int -> Rule
getRule constrName arity no =
  Rule [1]
       (Case Flex
             (Var 1)
             [Branch (Pattern constrName varInts) (Var (no+1))])
 where
  varInts = [2..arity+1]

putRule :: QName -> Int -> Int -> Rule
putRule constrName arity no =
  Rule [1,2] (Case Flex
                   (Var 1)
                   [Branch (Pattern constrName varInts)
                           (Comb ConsCall constrName vars)])
 where
  varInts = [3..arity+2]
  vars    = map Var (replace 2 (no-1) varInts)

lensRule :: QName -> QName -> Rule
lensRule sel upd =
  Rule []
       (Comb ConsCall
             ("Prelude","(,)")
             [funcPartCall 1 sel [], funcPartCall 2 upd []])

consName :: String -> String
consName = (++) "#_42"

funcPartCall :: Int -> QName -> [Expr] -> Expr
funcPartCall = Comb . FuncPartCall

lensTypeExpr :: TypeExpr -> TypeExpr -> TypeExpr
lensTypeExpr source view = TCons ("Prelude","(,)")
                                 [ getType source view
                                 , putType source view ]

typeFromName :: QName -> TypeExpr
typeFromName = (flip TCons) []

getType :: TypeExpr -> TypeExpr -> TypeExpr
getType source view = FuncType source view

putType :: TypeExpr -> TypeExpr -> TypeExpr
putType source view = FuncType source (FuncType view source)

isRecordType :: TypeDecl -> Bool
isRecordType = isPrefixOf "_#Rec:" . snd . typeName

isUpdateFunction :: FuncDecl -> Bool
isUpdateFunction = isPrefixOf "_#updR@" . funcNameOnly

isSelectFunction :: FuncDecl -> Bool
isSelectFunction = isPrefixOf "_#selR@" . funcNameOnly

funcNameOnly :: FuncDecl -> String
funcNameOnly = snd . funcName

typeNameOnly :: TypeDecl -> String
typeNameOnly = snd . typeName

{-

### REC-Version

Type ("Address","Person")
     Public
     []
     [Cons ("Address","Person")
           2
           Private
           [ TCons ("Prelude","[]") [TCons ("Prelude","Char") []]
           , TCons ("Prelude","[]") [TCons ("Prelude","Char") []]
           ]
     ]

### type Person = { _person :: String, _street :: String }

Generate (with labels)
========>

Func ("Address","person")
     0
     Public
     (TCons ("Prelude","(,)")
            [ FuncType (TCons ("Address","Address") [])
                      (TCons ("Address","Person") [])
            , FuncType (TCons ("Address","Address") [])
                       (FuncType (TCons ("Address","Person") [])
                                 (TCons ("Address","Address") []))
            ])
     (Rule []
           (Comb ConsCall
                 ("Prelude","(,)")
                 [ Comb (FuncPartCall 1)
                        ("Address","person_#lambda1")
                        []
                 , Comb (FuncPartCall 2)
                        ("Address","person_#lambda2")
                        []
                 ]))

### person :: (Address -> Person, Address -> Person -> Address)
### person = (person_#lambda1,person_#lambda2)

Func ("Address","person_#lambda1")
     1
     Private
     (FuncType (TCons ("Address","Address") [])
               (TCons ("Address","Person") []))
     (Rule [1]
           (Case Flex (Var 1)
                 [Branch (Pattern ("Address","Address") [2,3])
                          (Var 2)]))

### person_#lambda2 :: Address -> Person
### person_#lambda2 addr = case addr of
                                Address p s -> p


Func ("Address","person_#lambda2")
     2
     Private
     (FuncType (TCons ("Address","Address") [])
               (FuncType (TCons ("Address","Person") [])
                         (TCons ("Address","Address") [])))
     (Rule [1,2]
           (Case Flex
                 (Var 1)
                 [Branch (Pattern ("Address","Address") [3,4])
                         (Comb ConsCall
                               ("Address","Address")
                               [Var 2,Var 4])]))

### person_#lambda2 :: Address -> Person -> Address
### person_#lambda2 addr new = case addr of
                                    Address p s -> Address n s

(same for `street`)


### NonREC-Version

Type ("Address","Test")
     Public
     []
     [Cons ("Address","Test123")
           2
           Public
           [ TCons ("Prelude","Int")
                  []
           , TCons ("Prelude","[]")
                   [TCons ("Prelude","Char") []]
           ]
     ]

### data Test = Test123 Int String


Generate
=========>

Func ("Address","test123Acc1")
     0
     Public
     (TCons ("Prelude","(,)")
            [ FuncType (TCons ("Address","Test") [])
                      (TCons ("Prelude","Int") [])
            , FuncType (TCons ("Address","Test") [])
                       (FuncType (TCons ("Prelude","Int") [])
                                 (TCons ("Address","Test") []))
            ])
     (Rule []
           (Comb ConsCall
                 ("Prelude","(,)")
                 [ Comb (FuncPartCall 1)
                        ("Address","test123Acc1_#lambda1")
                        []
                 , Comb (FuncPartCall 2)
                        ("Address","test123Acc1_#lambda2")
                        []
                 ]))

### test123Acc1 :: (Test -> Int, Test -> Int -> Test)
### test123Acc1 = (test123Acc1_#lambda1,test123Acc1_#lambda2)


Func ("Address","test123Acc1_#lambda1")
     1
     Private
     (FuncType (TCons ("Address","Test") [])
               (TCons ("Prelude","Int") []))
     (Rule [1]
           (Case Flex
                 (Var 1)
                 [Branch (Pattern ("Address","Test123") [2,3])
                         (Var 2)]))

### test123Acc1_#lambda1 :: Test -> Int
### test123Acc1_#lambda1 test = case test of
                                     Test123 i s -> s


Func ("Address","test123Acc1_#lambda2")
     2
     Private
     (FuncType (TCons ("Address","Test") [])
               (FuncType (TCons ("Prelude","Int") [])
                         (TCons ("Address","Test") [])))
     (Rule [1,2]
           (Case Flex
                 (Var 1)
                 [Branch (Pattern ("Address","Test123") [3,4])
                         (Comb ConsCall
                               ("Address","Test123")
                               [Var 2,Var 4])]))

### test123Acc1_#lambda2 :: Test -> Int -> Test
### test123Acc1_#lambda2 test new = case test of
                                         Test123 i s -> Test123 new s
-}