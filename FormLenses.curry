module FormLenses where

import Time
import Monadic
import HTML (HtmlExp (..), htxt, htmlQuote, CgiRef (..), breakline, form)
import List (intercalate)
import Maybe (maybe)
import ReadShowTerm (readTerm)

type Env = [(String,String)]
type FormLens a = Maybe a -> [Int] -> (Html, Env -> Maybe a, [Int])
type Html = [HtmlExp]

noHtml :: Html
noHtml = []

lmap :: Lens a b -> FormLens b -> FormLens a
lmap lens f = \mVal i -> let (html, c, i') = f (fmap (get' lens) mVal) i
                      in (html, fmap (put' lens mVal) . c, i')

-- (<<^>>) ::(a1 -> b1) -> FormLens a2 b2 -> FormLens (a1,a2) (b1,b2)
-- unitL :: Iso FormLens ((),a) a
-- unitR :: Iso FormLens (a,()) a
-- assoc :: Iso c (a,(b,d)) ((a,b),d)

-- type Iso c a b = { fwdI :: c a b, bwdI :: c b a }

unitForm :: FormLens ()
unitForm = \_ is -> (noHtml, const (Just ()), is)


(<<*>>) :: FormLens a -> FormLens b -> FormLens (a,b)
(fa <<*>> fb) mVal is = let (a,b) = split mVal
                            (html,  ca, is' ) = fa a is
                            (html', cb, is'') = fb b is'
                        in (html ++ html', \e -> maybeLift (ca e, cb e), is'')
 where
  split (Just (x,y)) = (Just x, Just y)
  split Nothing      = (Nothing, Nothing)
  maybeLift :: (Maybe a, Maybe b) -> Maybe (a,b)
  maybeLift mVal = case mVal of
                       (Just x, Just y) -> Just (x,y)
                       _                -> Nothing

(<<*) :: FormLens a -> FormLens () -> FormLens a
fa <<* fu = lmap remSndOne (fa <<*>> fu)

(*>>) :: FormLens () -> FormLens a -> FormLens a
fu *>> fa = lmap remFstOne (fu <<*>> fa)

htmlL :: HtmlExp -> FormLens ()
htmlL h = \_ i -> ([h], \_ -> Just (), i)

textL :: String -> FormLens ()
textL s = htmlL (htxt s)

inputIntL :: FormLens Int
inputIntL = \v i@(h:t) ->
  let n = intercalate "_" (map show i)
  in ( [numberField n (maybe "" show v)]
     , \e -> fmap readTerm (lookup n e)
     , (h+1):t)

numberField :: String -> String -> HtmlExp
numberField name contents =
  HtmlStruct "input" [ ("type","number")
                      , ("name", name)
                      , ("value", htmlQuote contents)
                      ] []

type Date = { month :: Int, day :: Int }

dateLens :: Lens Date (Int,Int)
dateLens = isoLens inn out
 where
  inn (m, d) = { month := m, day := d }
  out date = (date :> month, date :> day)

dateL :: FormLens Date
dateL = lmap dateLens (textL "Month: " *>> inputIntL <<* htmlL breakline
                   <<* textL "Day: " <<*>> inputIntL <<* htmlL breakline)

mainForm = let (html,_,_) = dateL (Just {month := 4, day := 15}) [1..10]
           in return $ form "test" html