module FormLenses where

import Time
import Monadic
import HTML
import WUILenses
import List (intercalate)
import Maybe (maybe)
import ReadShowTerm (readTerm,showQTerm)

type Env = [(String,String)]
type FormLens a = Maybe a -> [Int] -> (Html, Env -> Maybe a, [Int])
type Html = [HtmlExp]

-- type WuiParams a = (Rendering, String, a -> Bool)
-- type Rendering = [HtmlExp] -> HtmlExp
-- type HtmlState = (HtmlExp,WuiState)
-- data WuiHandler = WHandler HtmlHandler
-- type HtmlHandler = CgiEnv -> IO HtmlForm
-- type CgiEnv = CgiRef -> String
-- data WuiSpec a = WuiSpec (WuiParams a)
--                          (WuiParams a -> a -> HtmlState)
--                          (WuiParams a -> CgiEnv -> WuiState -> (Maybe a, HtmlState))
-- data WuiState = Ref CgiRef
--               | Hidden String
--               | CompNode [WuiState]
--               | AltNode (Int,WuiState)
-- data HtmlForm =
--         HtmlForm String [FormParam] [HtmlExp]
--       | HtmlAnswer String String 

lmap :: Lens a b -> FormLens b -> FormLens a
lmap lens f = \mVal i -> let (html, c, i') = f (fmap (get' lens) mVal) i
                         in (html, fmap (put' lens mVal) . c, i')

-- wuiMap :: Lens a b -> WuiLens b -> WuiLens a
-- wuiMap lens (WuiSpec paramsA htmlA readA) = \val ref ->
  

-- (<<^>>) ::(a1 -> b1) -> FormLens a2 b2 -> FormLens (a1,a2) (b1,b2)
-- unitL :: Iso FormLens ((),a) a
-- unitR :: Iso FormLens (a,()) a
-- assoc :: Iso c (a,(b,d)) ((a,b),d)

-- type Iso c a b = { fwdI :: c a b, bwdI :: c b a }
noHtml :: Html
noHtml = []

unitForm :: FormLens ()
unitForm = \_ is -> (noHtml, const (Just ()), is)

unitWui :: WuiSpec ()
unitWui = wHidden

(<<*>>) :: FormLens a -> FormLens b -> FormLens (a,b)
(fa <<*>> fb) mVal is = let (a,b) = split mVal
                            (html,  ca, is' ) = fa a is
                            (html', cb, is'') = fb b is'
                        in (html ++ html', \e -> maybeLift (ca e, cb e), is'')
 where
  split (Just (x,y)) = (Just x, Just y)
  split Nothing      = (Nothing, Nothing)
  maybeLift :: (Maybe a, Maybe b) -> Maybe (a,b)
  maybeLift mVal' = case mVal' of
                       (Just x, Just y) -> Just (x,y)
                       _                -> Nothing

(<<*) :: FormLens a -> FormLens () -> FormLens a
fa <<* fu = lmap remSndOne (fa <<*>> fu)

(*>>) :: FormLens () -> FormLens a -> FormLens a
fu *>> fa = lmap remFstOne (fu <<*>> fa)

(<*) :: WuiSpec a -> WuiSpec Unit -> WuiSpec a
wa <* wu = transformWSpec remSndOne' (wa `wPair` wu)

(*>) :: WuiSpec Unit -> WuiSpec a -> WuiSpec a
wu *> wa = transformWSpec remFstOne' (wu `wPair` wa)

remSndOne' :: Lens a (a,Unit)
remSndOne' = remSnd (const Unit)

remFstOne' :: Lens a (Unit,a)
remFstOne' = remFst (const Unit)

htmlL :: HtmlExp -> FormLens ()
htmlL h = \_ i -> ([h], \_ -> Just (), i)

htmlWui :: HtmlExp -> WuiSpec Unit
htmlWui h = wConstant (\Unit -> h)

textL :: String -> FormLens ()
textL s = htmlL (htxt s)

textWui :: String -> WuiSpec Unit
textWui s = wHtml (htxt s)

inputIntL :: FormLens Int
inputIntL = \v i@(h:t) ->
  let n = intercalate "_" (map show i)
  in ( [numberField n (maybe "" show v)]
     , \e -> fmap readTerm (lookup n e)
     , (h+1):t)

-- inputIntWui :: WuiSpec Int
-- inputIntWui = wInt

numberField :: String -> String -> HtmlExp
numberField name contents =
  HtmlStruct "input" [ ("type","number")
                      , ("name", name)
                      , ("value", htmlQuote contents)
                      ] []

data Date = Date Int Int

dateLens :: Lens Date (Int,Int)
dateLens = isoLens inn out
 where
  inn (m, d) = Date m d
  out (Date m d) = (m,d)

dateL :: FormLens Date
dateL = lmap dateLens (textL "Month: " *>> inputIntL <<* htmlL breakline
                   <<* textL "Day: " <<*>> inputIntL <<* htmlL breakline)

dateWui :: WuiSpec Date
dateWui = transformWSpec dateLens (textWui "Month: " *> wInt <* wHtml breakline
                                 <* textWui "Day: " `wPair` wInt <* wHtml breakline)

dateTest :: WuiSpec Int
dateTest = textWui "test" *> wInt

mainForm = let (html,_,_) = dateL (Just date) [1..10]
           in return $ form "test" html

dateForm = mainWUI dateWui date resultForm

date :: Date
date = Date 4 15

--                   Key first  last   address
data Person = Person Int String String String

address :: Lens Person String
address = isoLens inn out <.> keepFst
 where
  inn ((key, first, last), address)   = Person key first last address
  out (Person key first last address) = ((key, first, last), address) 

addressWui :: WuiSpec Person
addressWui = transformWSpec address (textWui "Address: " *> wString <* wHtml breakline)

addressForm = mainWUI addressWui (Person 1 "Bastian" "Holst" "Gaarden") resultForm

resultForm :: a -> IO HtmlForm
resultForm v = return $ form "Result"
                 [htxt ("Modified value: " ++ showQTerm v)]