module FormLenses where

import Time
import HTML
import WUILenses
import List (intercalate)
import Maybe (maybe)
import ReadShowTerm (readTerm,showQTerm)
import Lens
import LensExamples
import SetFunctions

-- type Env = [(String,String)]
-- type FormLens a = Maybe a -> [Int] -> (Html, Env -> Maybe a, [Int])
-- type Html = [HtmlExp]

-- lmap :: Lens a b -> FormLens b -> FormLens a
-- lmap lens f = \mVal i -> let (html, c, i') = f (fmap (get' lens) mVal) i
--                          in (html, fmap (put' lens mVal) . c, i')

-- (<<*>>) :: FormLens a -> FormLens b -> FormLens (a,b)
-- (fa <<*>> fb) mVal is = let (a,b) = split mVal
--                             (html,  ca, is' ) = fa a is
--                             (html', cb, is'') = fb b is'
--                         in (html ++ html', \e -> maybeLift (ca e, cb e), is'')
--  where
--   split (Just (x,y)) = (Just x, Just y)
--   split Nothing      = (Nothing, Nothing)
--   maybeLift :: (Maybe a, Maybe b) -> Maybe (a,b)
--   maybeLift mVal' = case mVal' of
--                        (Just x, Just y) -> Just (x,y)
--                        _                -> Nothing

-- (<<*) :: FormLens a -> FormLens () -> FormLens a
-- fa <<* fu = lmap remSndOne (fa <<*>> fu)

-- (*>>) :: FormLens () -> FormLens a -> FormLens a
-- fu *>> fa = lmap remFstOne (fu <<*>> fa)

-- noHtml :: Html
-- noHtml = []

-- unitForm :: FormLens ()
-- unitForm = \_ is -> (noHtml, const (Just ()), is)

-- htmlL :: HtmlExp -> FormLens ()
-- htmlL h = \_ i -> ([h], \_ -> Just (), i)

-- textL :: String -> FormLens ()
-- textL s = htmlL (htxt s)

-- inputIntL :: FormLens Int
-- inputIntL = \v i@(h:t) ->
--   let n = intercalate "_" (map show i)
--   in ( [numberField n (maybe "" show v)]
--      , \e -> fmap readTerm (lookup n e)
--      , (h+1):t)

-- numberField :: String -> String -> HtmlExp
-- numberField name contents =
--   HtmlStruct "input" [ ("type","number")
--                       , ("name", name)
--                       , ("value", htmlQuote contents)
--                       ] []

-- dateL :: FormLens Date
-- dateL = lmap dateLens (textL "Month: " *>> inputIntL <<* htmlL breakline
--                    <<* textL "Day: " <<*>> inputIntL <<* htmlL breakline)

-- mainForm = let (html,_,_) = dateL (Just date) [1..10]
--            in return $ form "test" html

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

remSndOne' :: Lens a (a,Unit)
remSndOne' = remSnd (const Unit)

remFstOne' :: Lens a (Unit,a)
remFstOne' = remFst (const Unit)

unitWui :: WuiLensSpec Unit
unitWui = wHidden

(<*) :: WuiLensSpec a -> WuiLensSpec Unit -> WuiLensSpec a
wa <* wu = transformWSpec remSndOne' (wa `wPair` wu)

(*>) :: WuiLensSpec Unit -> WuiLensSpec a -> WuiLensSpec a
wu *> wa = transformWSpec remFstOne' (wu `wPair` wa)

htmlWui :: HtmlExp -> WuiLensSpec Unit
htmlWui h = wConstant (const h)

textWui :: String -> WuiLensSpec Unit
textWui s = wHtml (htxt s)

-- inputIntWui :: WuiLensSpec Int
-- inputIntWui = wInt

dateWui :: WuiLensSpec Date
dateWui = transformWSpec dateLens (textWui "Month: " *> wInt <* wHtml breakline
                                 <* textWui "Day: " `wPair` wInt <* wHtml breakline)

dateTest :: WuiLensSpec Int
dateTest = textWui "test" *> wInt

addressWui :: WuiLensSpec Person
addressWui = transformWSpec addressLens wRequiredString

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

testPage person = form "WUI" [ addressHtml
                             , wuiHandler2button "Change Address" addressHandler]
 where
  (addressHtml, addressHandler) = wui2html addressWui person personResultForm


personResultForm :: Person -> IO HtmlForm
personResultForm p = return $ form "Result"
                 [ htxt ("Modified value: " ++ showQTerm p)
                 , breakline
                 , button "back" (\_ -> return (testPage p))]

resultForm :: a -> IO HtmlForm
resultForm v = return $ form "Result"
                 [ htxt ("Modified value: " ++ showQTerm v)
                 , breakline]

date :: Date
date = Date 4 15

bastian :: Person
bastian = Person "Bastian" "Gaarden"

main :: IO HtmlForm
main = return $ testPage bastian

test :: IO HtmlForm
test = mainWUI dateTest 5 resultForm

test2 :: IO HtmlForm
test2 = mainWUI addressWui bastian resultForm

test3 :: IO HtmlForm
test3 = mainWUI (\a -> selectValue $ set1 addressWui a) bastian resultForm