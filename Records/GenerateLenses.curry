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
addImport = id -- updProgImports ("Lens" :)

transformRecords :: Prog ->  Prog
transformRecords prog@(Prog _ _ ts fs _) =
  -- updProgFuncs (\fs -> foldr (:) fs $ generateLensesForRecords fs)
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
      pairs         = map (\func -> qName func) fs
      namesAndFuncs = map (generateFuncNamesFromRecord pairs) recs
      funcDecls1    = map (uncurry generateFuncDeclsFromDatatype)
                          namesAndFuncs
      funcDecls2    = map (uncurry generateFuncDeclsFromDatatype)
                          (zip (repeat []) datas)
  in concat (funcDecls1 ++ funcDecls2)
 where
  qName :: FuncDecl -> (String,QName)
  qName funcDecl = (stripPrefix funcDecl,(fst $ funcName funcDecl,strip '.' funcDecl))
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
generateFuncDeclsFromDatatype names =
  concatMap (genFromDatatype names) . typeConsDecls

genFromDatatype :: [Maybe QName] -> ConsDecl -> [FuncDecl]
genFromDatatype names (Cons constrName@(mName,tName) arity _ constrs)
  | length names == length constrs =
      concatMap (uncurry genFromField)
                (zip [1..] (zip names constrs))
  | null names = concatMap (uncurry genFromField)
                           (zip [1..] (zip (repeat Nothing) constrs))
  | otherwise = error $ show $ (length names, length constrs)
 where
  genFromField :: Int -> (Maybe QName,TypeExpr) -> [FuncDecl]
  genFromField no (maybeName,tExpr) =
    let source           = typeFromName constrName
        getTypeExpr      = getType source tExpr
        putTypeExpr      = putType source tExpr
        qName = case maybeName of
                     Just name -> name
                     Nothing   -> (mName,map toLower tName ++ "Rec" ++ (show no))
        (getName,getFct) = lambdaFunction (snd qName)
                                          1
                                          getTypeExpr
                                          (getRule constrName arity no)
        (putName,putFct) = lambdaFunction (snd qName)
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
  lambdaFunction :: String -> Int -> TypeExpr -> Rule -> (QName,FuncDecl)
  lambdaFunction fName funArity tExpr rule =
    let name = (mName,fName ++ "_#lambdaLens" ++ show (funArity - 1))
    in (name,Func name arity Private tExpr rule)
                        

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
Func ("Address","_#selR@Person.first")
     1
     Public
     (FuncType (TCons ("Address","Person") [])
               (TCons ("Prelude","[]") [TCons ("Prelude","Char") []]))
     (Rule [1]
           (Case Flex
                 (Var 1)
                 [Branch (Pattern ("Address","Person") [2,3]) (Var 2)]))

 Func ("Address","personLens")
      0
      Public
      (TCons ("Prelude","(,)")
             [FuncType (TCons ("Address","Address") [])
                       (TCons ("Address","Person") [])
             ,FuncType (TCons ("Address","Address") [])
                       (FuncType (TCons ("Address","Person") [])
                                 (TCons ("Address","Address") []))
             ])
      (Rule []
            (Comb ConsCall
                  ("Prelude","(,)")
                  [Comb (FuncPartCall 1)
                        ("Address","personLens._#lambda1")
                        []
                  ,Comb (FuncPartCall 2)
                        ("Address","personLens._#lambda2")
                        []
                  ]))

 Func ("Address","personLens._#lambda1")
      1
      Private
      (FuncType (TCons ("Address","Address") [])
                (TCons ("Address","Person") []))
      (Rule [1]
            (Comb FuncCall
                  ("Address","_#selR@Address.person")
                  [Var 1]))

 Func ("Address","personLens._#lambda2")
      2
      Private
      (FuncType (TCons ("Address","Address") [])
                (FuncType (TCons ("Address","Person") [])
                          (TCons ("Address","Address") [])))
      (Rule [1,2]
            (Comb FuncCall
                  ("Address","_#updR@Address.person")
                  [Var 1,Var 2]))

Func ("Address","street")
     0
     Public
     (TCons ("Prelude","(,)")
            [FuncType (TCons ("Address","Address") [])
                      (TCons ("Prelude","[]") [TCons ("Prelude","Char") []])
            ,FuncType (TCons ("Address","Address") [])
                      (FuncType (TCons ("Prelude","[]") [TCons ("Prelude","Char")
                                                               []])
                                (TCons ("Address","Address") []))])
     (Rule []
           (Comb ConsCall
                 ("Prelude","(,)")
                 [Comb (FuncPartCall 1)
                       ("Address","street._#lambda1")
                       []
                 ,Comb (FuncPartCall 2)
                       ("Address","street._#lambda2") []]))

Func ("Address","street._#lambda1")
     1
     Private
     (FuncType (TCons ("Address","Address") [])
               (TCons ("Prelude","[]") [TCons ("Prelude","Char") []]))
     (Rule [1]
           (Case Flex
                 (Var 1)
                 [Branch (Pattern ("Address","Address42") [2,3]) (Var 3)]))

Func ("Address","street._#lambda2")
     2
     Private
     (FuncType (TCons ("Address","Address") [])
               (FuncType (TCons ("Prelude","[]") [TCons ("Prelude","Char") []])
                         (TCons ("Address","Address") [])))
     (Rule [1,2]
           (Case Flex
                 (Var 1)
                 [Branch (Pattern ("Address","Address42") [3,4])
                         (Comb ConsCall
                               ("Address","Address42")
                               [Var 3,Var 2])]))
-}