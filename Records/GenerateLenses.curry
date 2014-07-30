module GenerateLenses (
  writeFileForModule
  )
where

import FlatCurryRead (readFlatCurryWithImports)
import FlatCurry ( Prog(..), FuncDecl(..), TypeDecl(..), TypeExpr(..), Expr(..)
                 , QName, Visibility(..), CombType(..), Rule(..), ConsDecl(..)
                 , writeFCY, showQNameInModule )
import FlatCurryGoodies ( progName, progFuncs
                        , funcType, funcName, funcRule
                        , typeName, typeConsDecls, typeParams
                        , consArgs
                        , updProgImports, updProgFuncs, updProgTypes )
import FlatCurryPretty ( pPrint, ppProg )
import List ( isPrefixOf, deleteBy )

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
  let selAndUpdFs = ( filter isSelectFunction fs
                      , filter isUpdateFunction fs )
      newFs = generateLensesForDatatypes ts selAndUpdFs
  in updProgFuncs (\fs' -> foldr (:) fs' newFs) prog

generateLensesForDatatypes :: [TypeDecl]
                           -> ([FuncDecl],[FuncDecl])
                           -> [FuncDecl]
generateLensesForDatatypes ts (sel,upd) =
  let recs       = filter (\t -> typeNameOnly t `elem` (map fst pairs)) ts
      pairs      = map (findPair sel) upd
      funcDecls1 = map (generateFuncNamesFromRecord pairs)
                       recs
      funcDecls2 = [] -- TODO
  in concat (funcDecls1 ++ funcDecls2)
 where
  findPair (x:xs) y
    | strip x == strip y = (stripPrefix x, (x,y))
    | otherwise          = findPair xs y
  strip :: FuncDecl -> String
  strip = tail . dropWhile (/='@') . funcNameOnly
  stripPrefix :: FuncDecl -> String
  stripPrefix = takeWhile (/='.') . strip

generateFuncNamesFromRecord :: [(String,(FuncDecl,FuncDecl))]
                            -> TypeDecl
                            -> [FuncDecl]
generateFuncNamesFromRecord fdMap (Type name@(mName,tName) _ _ [cDecl]) =
  genFromRecord fdMap (zip [2..] (consArgs cDecl))
 where
  genFromRecord :: [(String,(FuncDecl,FuncDecl))]
                -> [(Int,TypeExpr)]
                -> [FuncDecl]
  genFromRecord _     []                       = []
  genFromRecord aMap ((_, tExpr) : pairs) =
    let (Just (sel,upd),newMap) = lookupAndDelete tName aMap
        fName                   = tail (dropWhile (/= '.') (funcNameOnly sel))
        func                    =
          Func (mName,fName)
               0
               Private
               (lensTypeExpr (typeFromName name) tExpr)
               (lensRule (funcName sel) (funcName upd))
    in func : genFromRecord newMap pairs
  lookupAndDelete :: k -> [(k,a)] -> (Maybe a, [(k,a)])
  lookupAndDelete _   []                      = (Nothing,[])
  lookupAndDelete key (pair@(key',value):kvs)
    | key == key' = (Just value, kvs)
    | otherwise   = case lookupAndDelete key kvs of
                         (maybeValue, kvs') -> (maybeValue, pair : kvs')

generateLenses :: FuncDecl -> FuncDecl -> FuncDecl
generateLenses sel upd =
  Func (mName, fName)
       0
       Public
       (lensTypeExpr (funcType sel) (funcType upd))
       (lensRule (funcName sel) (funcName upd))
 where
  fName = tail (dropWhile (/= '.') (funcNameOnly sel))
  mName = fst (funcName sel)

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
                                 [ selectionType source view
                                 , updateType source view ]

typeFromName :: QName -> TypeExpr
typeFromName = (flip TCons) []

selectionType :: TypeExpr -> TypeExpr -> TypeExpr
selectionType source view = FuncType source view

updateType :: TypeExpr -> TypeExpr -> TypeExpr
updateType source view = FuncType source (FuncType view source)

-- isRecordType :: TypeDecl -> Bool
-- isRecordType = isPrefixOf "_#Rec:" . snd . typeName

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