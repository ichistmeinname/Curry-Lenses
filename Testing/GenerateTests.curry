module GenerateTests (
  writeLensTestsForModule,
  Config(..), Verbosity(..), MainConfig(..)
  )
where

import List ( intercalate, isSuffixOf )
import FileGoodies ( baseName, stripSuffix )
import FlatCurryRead (readFlatCurryWithImportsInPath,readFlatCurryWithImports)
import FlatCurry ( Prog(..), FuncDecl(..), TypeExpr(..), Rule(..), CombType(..)
                 , Expr(..), QName, Visibility(..) )
import FlatCurryGoodies ( progName, progFuncs, tConsName, funcType, funcName
                        , domain, funcArity, funcVisibility )
import FlatCurryShow ( showCurryExpr, showCurryType )
import Distribution ( lookupFileInLoadPath )
import LensCheck


--- data type and constant declarations

data Config     = Config Verbosity MainConfig
data MainConfig = Main | MainWithPutPut | MainOnlyPutStabDet | NoMain
data Verbosity  = Verbose | Default
data CurryFile  = CurryFile String
type FilePath   = String
type ModuleName = String

-- consists of function name, signature and rule expr
data FuncString = FuncString String TypeExpr Expr

easyCheck,lensCheck :: CurryFile
easyCheck = CurryFile "EasyCheck.curry"
lensCheck = CurryFile "LensCheck.curry"

testFunctionNames,listTestFunctionNames :: [String]
testFunctionNames    = ["checkGetPut"
                       ,"checkPutGet"
                       ,"checkPutPut"
                       ,"checkPutDet"
                       ,"checkPutStab"]
listTestFunctionNames = ["checkListGetPut"
                        ,"checkListPutGet"
                        ,"checkListPutPut"
                        ,"checkListPutDet"
                        ,"checkListPutStab"]


--- file helper

readFlatCurryForModule :: [FilePath] -> ModuleName -> IO Prog
readFlatCurryForModule loadPath moduleName = do
  checkFile (toCurryFile moduleName)
  progs <- readFlatCurryWithImportsInPath loadPath moduleName
  case filter (\p -> moduleName == progName p) progs of
       [p] -> return p
       _   -> error ("Couldn't find module with name " ++ moduleName ++ ".")

toCurryFile :: ModuleName -> CurryFile
toCurryFile file = CurryFile (file ++ ".curry")

toCurryModule :: CurryFile -> ModuleName
toCurryModule (CurryFile file) = stripSuffix file

checkFile :: CurryFile -> IO ()
checkFile (CurryFile file) = do
  mCheck <- lookupFileInLoadPath file
  return $ maybe err (const ()) mCheck
 where
  err = error ("Couldn't find " ++ file
               ++" in loadpath, please check your current path.")

-- Collects lens functions from a given module, provided that the module exists
--  in the given load path
readLensFuncDeclsForModule :: [FilePath] -> ModuleName -> IO [FuncDecl]
readLensFuncDeclsForModule loadPath modulePath = do
  prog <- readFlatCurryForModule loadPath modulePath
  let funcs     = progFuncs prog
      lensFuncs = filter isTopLevelLensFunc funcs
  return lensFuncs


--- Tests for function declarations

-- Checks if a given function declaration is of type "Lens _ _"
isTopLevelLensFunc :: FuncDecl -> Bool
isTopLevelLensFunc funcDecl =
  case funcType funcDecl of
       FuncType x (FuncType _ y) -> x == y && funcVisibility funcDecl == Public
       _                         -> False

-- Checks if the given type expression is of type List
isListType :: TypeExpr -> Bool
isListType typeExpr = case typeExpr of
                           TCons ("Prelude","[]") _ -> True
                           _                        -> False


--- Operations on function declarations

-- looks up a function name in a list of function declaration
--  and yields this function, if it exists
--  and throws an error otherwise
lookupFunction :: [FuncDecl] -> String -> FuncDecl
lookupFunction functions name =
  case filter (\f -> snd (funcName f) == name) functions of
       [x] -> x
       _   -> error ("Couldn't find function " ++ name ++ " in module "
                     ++ toCurryModule lensCheck ++ ".")

funcDeclToTestFuncs :: Verbosity
                    -> [FuncDecl]
                    -> [FuncDecl]
                    -> FuncDecl
                    -> [FuncString]
funcDeclToTestFuncs config testFuncs listTestFuncs (Func qName _ _ typeExpr _) =
  zipWith (mkFuncString config qName) labels funcs
 where
  labels = ["GetPut","PutGet","PutPut","PutDet","PutStab"]
  funcs | isListType (domain typeExpr) = listTestFuncs
        | otherwise                    = testFuncs


--- String generations

--- generates a main test function, if `mainConfig` is set
mainTestFunction :: MainConfig -> [String] -> String
mainTestFunction mainConfig funcNames =
  case mainConfig of
       NoMain -> ""
       _      -> "mainTest = do\n" ++ concatMap addFunction funcNames'
 where
  funcNames'
    | mainConfig == Main               =
        filter (not . isSuffixOf "PutPut") funcNames
    | mainConfig == MainOnlyPutStabDet =
        filter (\fn -> isSuffixOf "PutDet" fn || isSuffixOf "PutStab" fn)
               funcNames
    | otherwise                        = funcNames
  addFunction, printName :: String -> String
  addFunction name = printName name ++ replicate 2 ' ' ++ name ++ "\n"
  printName   name = replicate 2 ' '
                   ++ "putStrLn " ++ show ("\n+++++" ++ name ++ "+++++")
                   ++ "\n"

progToString :: MainConfig -> String -> [FuncString] -> (String,String)
progToString mainConfig modName funcs =
   (path, moduleDecl ++ importModule (toCurryModule lensCheck)
            ++ importModule modName ++ importModule "EasyCheck"
            ++ "\n" ++ intercalate "\n\n" (map funcToString funcs) ++ "\n\n"
            ++ mainTestFunction mainConfig
                                (map (\ (FuncString fName _ _) -> fName) funcs))
 where
  name = "Test" ++ modName
  path = name ++ ".curry"
  importModule moduleName = "import " ++ moduleName ++ "\n"
  moduleDecl = "module " ++ name ++ " where\n\n"

funcToString :: FuncString -> String
funcToString (FuncString name typeExpr expr) =
  mkSignature name typeExpr ++ "\n" ++ mkRule name expr

mkSignature :: String -> TypeExpr -> String
mkSignature name typeExpr = name ++ " :: " ++ showCurryType snd False typeExpr

mkRule :: String -> Expr -> String
mkRule name expr = name ++ " = " ++ showCurryExpr snd False 1 expr

mkFuncString :: Verbosity -> QName -> String -> FuncDecl -> FuncString
mkFuncString config (modName,funName) funSuffix funDecl =
  FuncString (funName ++ funSuffix) ioTypeExpr testExpr
 where
  ioTypeExpr = TCons ("Prelude","IO") [TCons ("Prelude","()") []]
  testExpr   = Comb FuncCall
                    ("EasyCheck", checkFun ++ arity)
                    [Comb FuncCall
                          ("LensCheck", snd (funcName funDecl))
                          [Comb FuncCall (modName,funName) []]
                    ]
  arity      = case funcArity funDecl - 1 of
                    0 -> ""
                    x -> show x
  checkFun = case config of
                  Verbose -> "verboseCheck"
                  Default -> "easyCheck"


-- main function to generate tests for a specific module;
--  the given config determines `EasyCheck`s test function and
--  if a main function is generated as well
writeLensTestsForModule :: Config -> [String] -> ModuleName -> IO ()
writeLensTestsForModule (Config vConfig mConfig) loadPath moduleName = do
  mapIO checkFile [easyCheck,lensCheck]
  lensFuncs <- readLensFuncDeclsForModule loadPath moduleName
  checkModule <- readFlatCurryForModule loadPath (toCurryModule lensCheck)
  let funcs           = progFuncs checkModule
      checkFuncs      = map (lookupFunction funcs) testFunctionNames
      checkListFuncs  = map (lookupFunction funcs) listTestFunctionNames
      lensFuncStrings = map (funcDeclToTestFuncs vConfig
                                                 checkFuncs
                                                 checkListFuncs)
                            lensFuncs
  uncurry writeFile (progToString mConfig modName (concat lensFuncStrings))
 where
  modName = baseName moduleName