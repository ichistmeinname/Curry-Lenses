module GenerateLenses (
  writeFileForModule
  )
where

import FlatCurryRead (readFlatCurryWithImports)
import FlatCurry ( Prog(..), FuncDecl(..), TypeDecl(..), TypeExpr(..), Expr(..)
                 , QName, Visibility(..), CombType(..), Rule(..) )
import FlatCurryGoodies ( progName, progFuncs
                        , funcType, funcName, funcRule
                        , typeName
                        , updProgImports, updProgFuncs )
import FlatCurryPretty (ppProg, pPrint)

import List (isPrefixOf)

type ModuleName = String
type FilePath   = String

writeFileForModule :: ModuleName -> FilePath -> IO ()
writeFileForModule mName path = do
  prog <- readFlatCurryForModule mName
  let newProg = addImport (transformRecords prog)
  writeFile path (pPrint (ppProg newProg))

readFlatCurryForModule :: ModuleName -> IO Prog
readFlatCurryForModule moduleName = do
  progs <- readFlatCurryWithImports moduleName
  case filter (\p -> moduleName == progName p) progs of
       [p] -> return p
       _   -> error ("Couldn't find module with name " ++ moduleName ++ ".")

addImport :: Prog -> Prog
addImport = updProgImports ("Lens" :)

transformRecords :: Prog -> Prog
transformRecords =
  updProgFuncs (\fs -> foldr (:) fs $ generateLensesForRecords fs)

generateLensesForRecords :: [FuncDecl] -> [FuncDecl]
generateLensesForRecords fs =
  let sel = filter isSelectFunction fs
      upd = filter isUpdateFunction fs
      pairs = map (findPair sel) upd
  in map (uncurry generateLenses) pairs
 where
  findPair (x:xs) y
    | strip (funcNameOnly x) == strip (funcNameOnly y) = (x,y)
    | otherwise                                        = findPair xs y
  strip :: String -> String
  strip = tail . dropWhile (/= '@')

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

funcPartCall :: Int -> QName -> [Expr] -> Expr
funcPartCall = Comb . FuncPartCall

lensTypeExpr :: TypeExpr -> TypeExpr -> TypeExpr
lensTypeExpr sel upd = TCons ("Prelude","(,)") [sel, upd]

isUpdateFunction, isSelectFunction :: FuncDecl -> Bool
isUpdateFunction = isPrefixOf "_#updR@" . funcNameOnly
isSelectFunction = isPrefixOf "_#selR@" . funcNameOnly

isRecordType :: TypeDecl -> Bool
isRecordType = isPrefixOf "_#Rec:" . snd . typeName

funcNameOnly :: FuncDecl -> String
funcNameOnly = snd . funcName

typeNameOnly :: TypeDecl -> String
typeNameOnly = snd . typeName


{-
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
-}


{-
 Func ("Address","_#selR@Address.street") 1 Public
  (FuncType (TCons ("Address","Address") [])
            (TCons ("Prelude","[]") [TCons ("Prelude","Char") []]))
  (Rule [1] (Case Flex
                  (Var 1)
                  [Branch (Pattern ("Address","Address") [2,3]) (Var 3)]
            )
  )
-}

{-
 Func ("Address","_#updR@Address.street") 2 Public
  (FuncType (TCons ("Address","Address") [])
            (FuncType (TCons ("Prelude","[]")
                        [TCons ("Prelude","Char") []])
                      (TCons ("Address","Address") [])
            ))
  (Rule [1,2] (Case Flex
                    (Var 1)
                    [Branch (Pattern ("Address","Address") [3,4])
                            (Comb ConsCall ("Address","Address") [Var 3,Var 2])]
              ))
-}