------------------------------------------------------------------------------
--- A library to support the type-oriented construction of Web User Interfaces
--- (WUIs). In contrast to the original WUI library, this library does
--- not use functional patterns and, thus, has a different interface.
---
--- The ideas behind the application and implementation of WUIs are
--- described in a paper that is available via
--- [this web page](http://www.informatik.uni-kiel.de/~pakcs/WUI).
---
--- @author Michael Hanus
--- @version November 2011
------------------------------------------------------------------------------

module WUILenses(--WuiState,cgiRef2state,state2cgiRef,value2state,state2value,
           --states2state,state2states,altstate2state,state2altstate,
           Rendering,WuiLensSpec,Unit(..),
           withRendering,withError,withCondition,transformWSpec,
           wHidden,wConstant,wHtml,wInt,
           wString,wStringSize,wRequiredString,wRequiredStringSize,wTextArea,
           wSelect,wSelectInt,wSelectBool,wRadioSelect,wRadioBool,wCheckBool,
           wMultiCheckSelect,
           wPair,wTriple,w4Tuple,w5Tuple,w6Tuple,w7Tuple,w8Tuple,
           w9Tuple,w10Tuple,w11Tuple,w12Tuple,
           --wCons2,wCons3,wCons4,wCons5,wCons6,wCons7,wCons8,
           --wCons9,wCons10,wCons11,wCons12,
           wJoinTuple,wMaybe,wCheckMaybe,wRadioMaybe,
           wList,wListWithHeadings,wHList,wMatrix,wEither,
           WTree(..),wTree,
           WuiHandler,wuiHandler2button,
           renderTuple,renderTaggedTuple,renderList,
           mainWUI,wui2html,wuiInForm,wuiWithErrorForm)
 where

import Monadic
import HTML
import Read(readNat)
import List(elemIndex)
import Maybe
import Char(isDigit,isSpace)
import ReadShowTerm

infixl 0 `withRendering`
infixl 0 `withError`
infixl 0 `withCondition`

data Unit = Unit

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing  = Nothing
maybeMap f (Just v) = Just (f v)

------------------------------------------------------------------------------
--- An internal WUI state is used to maintain the cgi references of the input
--- fields as a structure that corresponds to the structure of the edit data.
data WuiState =
     Ref CgiRef             -- reference to elementary input field
   | Hidden String          -- string representation of a hidden value
   | CompNode [WuiState]    -- composition of trees (substructures)
   | AltNode (Int,WuiState) -- alternative of trees (union of substructures)

cgiRef2state :: CgiRef -> WuiState
cgiRef2state cr = Ref cr

state2cgiRef :: WuiState -> CgiRef
state2cgiRef (Ref cr) = cr

value2state :: a -> WuiState
value2state v = Hidden (showQTerm v)

state2value :: WuiState -> a
state2value (Hidden s) = readQTerm s

states2state :: [WuiState] -> WuiState
states2state sts = CompNode sts

state2states :: WuiState -> [WuiState]
state2states (CompNode sts) = sts

altstate2state :: (Int,WuiState) -> WuiState
altstate2state alt = AltNode alt

state2altstate :: WuiState -> (Int,WuiState)
state2altstate (AltNode alt) = alt

------------------------------------------------------------------------------
--- A rendering is a function that combines the visualization of components
--- of a data structure into some HTML expression.
type Rendering = [HtmlExp] -> HtmlExp

--- WuiParams specify the parameters of an individual Wui component type:
--- * the standard rendering
--- * an error message shown in case of illegal inputs
--- * a condition to specify legal input values
type WuiParams a = (Rendering, String, a->Bool)

renderOf (render,_,_) = render

errorOf (_,err,_) = err

conditionOf (_,_,c) = c

------------------------------------------------------------------------------
--- The type HtmlSate are values consisting of an HTML expression
--- (usually containing some input elements) and a WUI state containing
--- references to input elements in the HTML expression.

type HtmlState = (HtmlExp,WuiState)

------------------------------------------------------------------------------
--- A handler for a WUI is an event handler for HTML forms possibly with some
--- specific code attached (for future extensions).
data WuiHandler = WHandler HtmlHandler

--- Transform a WUI handler into a submit button with a given label string.
wuiHandler2button :: String -> WuiHandler -> HtmlExp
wuiHandler2button title (WHandler handler) = button title handler

------------------------------------------------------------------------------
--- The type of WUI specifications.
--- The first component are parameters specifying the behavior of this WUI type
--- (rendering, error message, and constraints on inputs).
--- The second component is a "show" function returning an HTML expression for
--- the edit fields and a WUI state containing the CgiRefs to extract
--- the values from the edit fields.
--- The third component is "read" function to extract the values from
--- the edit fields for a given cgi environment (returned as (Just v)).
--- If the value is not legal, Nothing is returned. The second component
--- of the result contains an HTML edit expression
--- together with a WUI state to edit the value again.
type WuiLensSpec a = Maybe a -> WuiSpec a

data WuiSpec a =
  WuiSpec (WuiParams a)
          (WuiParams a -> a -> HtmlState)
          (WuiParams a -> CgiEnv -> WuiState -> (Maybe a,HtmlState))

--- Puts a new rendering function into a WUI specification.
withRendering :: WuiLensSpec a -> Rendering -> WuiLensSpec a
withRendering wuiSpec render mVal =
  WuiSpec (render,errmsg,legal) showhtml readvalue

 where
  WuiSpec (_,errmsg,legal) showhtml readvalue = wuiSpec mVal

--- Puts a new error message into a WUI specification.
withError :: WuiLensSpec a -> String -> WuiLensSpec a
withError wuiSpec errmsg mVal =
  WuiSpec (render,errmsg,legal) showhtml readvalue
 where
  WuiSpec (render,_,legal) showhtml readvalue = wuiSpec mVal

--- Puts a new condition into a WUI specification.
withCondition :: WuiLensSpec a -> (a -> Bool) -> WuiLensSpec a
withCondition wuiSpec legal mVal =
  WuiSpec (render,errmsg,legal) showhtml readvalue
 where
   WuiSpec (render,errmsg,_) showhtml readvalue = wuiSpec mVal

--- Transforms a WUI specification from one type to another.
transformWSpec :: Lens b a -> WuiLensSpec a -> WuiLensSpec b
transformWSpec lens wuiSpec mValB =
  WuiSpec (transParam (get' lens) wparamsa)
          (\wparamsb b -> showhtmla (transParam (put' lens mValB) wparamsb)
                                    (get' lens b))
          (\wparamsb env wst ->
            let (mba,errv) = readvaluea (transParam (put' lens mValB) wparamsb) env wst
             in (maybeMap (put' lens mValB) mba, errv))
 where
  transParam :: (b->a) -> WuiParams a -> WuiParams b
  transParam toa (render,errmsg,legal) = (render,errmsg,legal . toa)
  WuiSpec wparamsa showhtmla readvaluea = wuiSpec (maybeMap (get' lens) mValB)

------------------------------------------------------------------------------
-- A collection of basic WUIs and WUI combinators:

--- A hidden widget for a value that is not shown in the WUI.
--- Usually, this is used in components of larger
--- structures, e.g., internal identifiers, data base keys.
wHidden :: WuiLensSpec a
wHidden _ =
  WuiSpec (head,"?",const True) -- dummy values, not used
          (\_ v -> (hempty, value2state v))
          (\_ _ s -> (Just (state2value s), (hempty,s)))

wHtml :: HtmlExp -> WuiLensSpec Unit
wHtml h _ =
  WuiSpec (head,"?",const True)
          (\wparams v -> ((renderOf wparams) [h], value2state v))
          (\(render,_,_) _ s -> let v = state2value s in
                                (Just v, (render [h],s)))

--- A widget for values that are shown but cannot be modified.
--- The first argument is a mapping of the value into a HTML expression
--- to show this value.
wConstant :: (a->HtmlExp) -> WuiLensSpec a
wConstant showhtml _ =
  WuiSpec (head,"?",const True)
          (\wparams v -> ((renderOf wparams) [showhtml v], value2state v))
          (\(render,_,_) _ s -> let v = state2value s in
                                (Just v, (render [showhtml v],s)))

--- A widget for editing integer values.
wInt :: WuiLensSpec Int
wInt _ =
  WuiSpec (head,"Illegal integer:",const True)
          (\wparams v -> intWidget (renderOf wparams) (show v))
          (\(render,errmsg,legal) env s ->
            let input = env (state2cgiRef s)
                renderr = renderError render errmsg
             in maybe (Nothing, intWidget renderr input)
                      (\v -> if legal v
                             then (Just v,  intWidget render input)
                             else (Nothing, intWidget renderr input))
                      (readMaybeInt (stripSpaces input)))
 where
  intWidget render s = let ref free in
    (render [textfield ref s `addAttr` ("size","6")], cgiRef2state ref)

-- Remove leading and ending spaces in a string.
stripSpaces :: String -> String
stripSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Read a (possibly negative) integer in a string.
-- Return Nothing is this is not an integer string.
readMaybeInt :: String -> Maybe Int
readMaybeInt "" = Nothing
readMaybeInt (v:s) | v=='-'  = maybe Nothing (\i->Just (-i)) (acc 0 s)
                   | isDigit v  = acc 0 (v:s)
                   | otherwise  = Nothing
 where
  acc n "" = Just n
  acc n (c:cs) | isDigit c = acc (10*n + ord c - ord '0') cs
               | otherwise = Nothing


checkLegalInput :: WuiParams a -> (Rendering -> a -> HtmlState) -> a
                   -> (Maybe a,HtmlState)
checkLegalInput (render,errmsg,legal) value2widget value =
  if legal value
  then (Just value, value2widget render value)
  else (Nothing,    value2widget (renderError render errmsg) value)


--- A predefined filter for processing string inputs.
--- Here, we replace \r\n by \n:
filterStringInput :: String -> String
filterStringInput = removeCRs

--- Replace all \r\n by \n:
removeCRs :: String -> String
removeCRs [] = []
removeCRs [c] = [c]
removeCRs (c1:c2:cs) =
  if c1=='\r' && c2=='\n' then '\n' : removeCRs cs
                          else c1 : removeCRs (c2:cs)

--- A widget for editing string values.
wString :: WuiLensSpec String
wString mVal = wStringAttrs [] mVal

--- A widget for editing string values with a size attribute.
wStringSize :: Int -> WuiLensSpec String
wStringSize size mVal = wStringAttrs [("size",show size)] mVal

--- A widget for editing string values with some attributes for the
--- text field.
wStringAttrs :: [(String,String)] -> WuiLensSpec String
wStringAttrs attrs _ =
  WuiSpec (head, "?", const True)
          (\wparams v -> stringWidget (renderOf wparams) v)
          (\wparams env s ->
                checkLegalInput wparams stringWidget
                                (filterStringInput (env (state2cgiRef s))))
 where
  stringWidget render v =
    let ref free
    in (render [foldr (flip addAttr) (textfield ref v) attrs], cgiRef2state ref)

--- A widget for editing string values that are required to be non-empty.
wRequiredString :: WuiLensSpec String
wRequiredString =
  wString `withError`     "Missing input:"
               `withCondition` (not . null)

--- A widget with a size attribute for editing string values
--- that are required to be non-empty.
wRequiredStringSize :: Int -> WuiLensSpec String
wRequiredStringSize size =
  wStringSize size `withError`     "Missing input:"
                        `withCondition` (not . null)

--- A widget for editing string values in a text area.
--- The argument specifies the height and width of the text area.
wTextArea :: (Int,Int) -> WuiLensSpec String
wTextArea dims _ =
  WuiSpec (head, "?", const True)
          (\wparams v -> textareaWidget (renderOf wparams) v)
          (\wparams env s ->
               checkLegalInput wparams textareaWidget
                                       (filterStringInput (env (state2cgiRef s))))
 where
  textareaWidget render v = let ref free in
                            (render [textarea ref dims v], cgiRef2state ref)


--- A widget to select a value from a given list of values.
--- The current value should be contained in the value list and is preselected.
--- The first argument is a mapping from values into strings to be shown
--- in the selection widget.
wSelect :: (a->String) -> [a] -> WuiLensSpec a
wSelect showelem selset _ =
  WuiSpec (head,"?",const True)
          (\wparams v -> selWidget (renderOf wparams) v)
          (\wparams env s ->
             checkLegalInput wparams selWidget
                             (selset !! readNat (env (state2cgiRef s))))
 where
  selWidget render v =
    let ref free
        idx = elemIndex v selset
        namevalues = zip (map showelem selset) (map show [0..])
     in (render [maybe (selection ref namevalues)
                       (\i -> selectionInitial ref namevalues i)
                       idx],
         cgiRef2state ref)

--- A widget to select a value from a given list of integers (provided as
--- the argument).
--- The current value should be contained in the value list and is preselected.
wSelectInt :: [Int] -> WuiLensSpec Int
wSelectInt mVal = wSelect show mVal

--- A widget to select a Boolean value via a selection box.
--- The arguments are the strings that are shown for the values
--- True and False in the selection box, respectively.
--- @param true - string for selection of True
--- @param false - string for selection of False
--- @return a WUI specification for a Boolean selection widget
wSelectBool :: String -> String -> WuiLensSpec Bool
wSelectBool true false mVal = wSelect (\b->if b then true else false) [True,False] mVal

--- A widget to select a Boolean value via a check box.
--- The first argument are HTML expressions that are shown after the
--- check box.  The result is True if the box is checked.
wCheckBool :: [HtmlExp] -> WuiLensSpec Bool
wCheckBool hexps _ =
  WuiSpec (head, "?", const True)
          (\wparams v -> checkWidget (renderOf wparams) v)
          (\wparams env s ->
             checkLegalInput wparams checkWidget (env (state2cgiRef s)=="True"))
 where
  checkWidget render v = let ref free in
    (render [inline ((if v then checkedbox else checkbox) ref "True" : hexps)],
     cgiRef2state ref)

--- A widget to select a list of values from a given list of values
--- via check boxes.
--- The current values should be contained in the value list and are preselected.
--- The first argument is a mapping from values into HTML expressions
--- that are shown for each item after the check box.
wMultiCheckSelect :: (a->[HtmlExp]) -> [a] -> WuiLensSpec [a]
wMultiCheckSelect showelem selset _ =
  WuiSpec (renderTuple, tupleError, const True)
          (\wparams vs -> checkWidget (renderOf wparams) vs)
          (\wparams env st ->
             checkLegalInput wparams checkWidget
                   (concatMap (\ (ref,s) -> if env ref=="True" then [s] else [])
                              (zip (map state2cgiRef (state2states st)) selset)))
 where
  checkWidget render vs =
    let refs = take (length selset) newVars
        numsetitems = zip refs selset
        showItem (ref,s) =
           inline ((if s `elem` vs then checkedbox else checkbox)
                                                       ref "True" : showelem s)
     in (render (map showItem numsetitems),
         states2state (map cgiRef2state refs))

newVars = unknown : newVars

--- A widget to select a value from a given list of values via a radio button.
--- The current value should be contained in the value list and is preselected.
--- The first argument is a mapping from values into HTML expressions
--- that are shown for each item after the radio button.
wRadioSelect :: (a->[HtmlExp]) -> [a] -> WuiLensSpec a
wRadioSelect showelem selset _ =
  WuiSpec (renderTuple, tupleError, const True)
          (\wparams v -> radioWidget (renderOf wparams) v)
          (\wparams env s ->
             checkLegalInput wparams radioWidget
                             (selset !! readNat (env (state2cgiRef s))))
 where
  radioWidget render v =
    let ref free
        idx = maybe 0 id (elemIndex v selset)
        numhitems = zip [0..] (map showelem selset)
        showItem (i,s) = table [[[(if i==idx then radio_main else radio_other)
                                        ref (show i)],s]]
     in (render (map showItem numhitems),
         cgiRef2state ref)

--- A widget to select a Boolean value via a radio button.
--- The arguments are the lists of HTML expressions that are shown after
--- the True and False radio buttons, respectively.
--- @param true - HTML expressions for True radio button
--- @param false - HTML expressions for False radio button
--- @return a WUI specification for a Boolean selection widget
wRadioBool :: [HtmlExp] -> [HtmlExp] -> WuiLensSpec Bool
wRadioBool truehexps falsehexps =
  wRadioSelect (\b->if b then truehexps else falsehexps) [True,False]


--- WUI combinator for pairs.
wPair :: WuiLensSpec a -> WuiLensSpec b -> WuiLensSpec (a,b)
wPair wuiSpecA wuiSpecB mValAB =
  WuiSpec (renderTuple, tupleError, const True) showc readc
 where
  showc wparams (va,vb) =
    let (hea,rta) = showa rendera va
        (heb,rtb) = showb renderb vb
     in ((renderOf wparams) [hea,heb], states2state [rta,rtb])

  readc (render,errmsg,legal) env s =
    let [ra,rb] = state2states s
        (rav,(hea,rta)) = reada rendera env ra
        (rbv,(heb,rtb)) = readb renderb env rb
        errhexps = [hea,heb]
        errstate = states2state [rta,rtb]
     in if rav==Nothing || rbv==Nothing
        then (Nothing, (render errhexps, errstate))
        else let value = (fromJust rav, fromJust rbv) in
             if legal value
             then (Just value, (render errhexps, errstate))
             else (Nothing,    (renderError render errmsg errhexps, errstate))
  WuiSpec rendera showa reada = wuiSpecA mValA
  WuiSpec renderb showb readb = wuiSpecB mValB
  (mValA,mValB) = case mValAB of
               Nothing    -> (Nothing, Nothing)
               Just (a,b) -> (Just a, Just b)


--- WUI combinator for triples.
wTriple :: WuiLensSpec a -> WuiLensSpec b -> WuiLensSpec c -> WuiLensSpec (a,b,c)
wTriple wuiSpecA wuiSpecB wuiSpecC mValABC =
  WuiSpec (renderTuple, tupleError, const True) showd readd
 where
  showd wparams (va,vb,vc) =
    let (hea,rta) = showa rendera va
        (heb,rtb) = showb renderb vb
        (hec,rtc) = showc renderc vc
     in ((renderOf wparams) [hea,heb,hec], states2state [rta,rtb,rtc])

  readd (render,errmsg,legal) env s =
    let [ra,rb,rc] = state2states s
        (rav,(hea,rta)) = reada rendera env ra
        (rbv,(heb,rtb)) = readb renderb env rb
        (rcv,(hec,rtc)) = readc renderc env rc
        errhexps = [hea,heb,hec]
        errstate = states2state [rta,rtb,rtc]
     in if rav==Nothing || rbv==Nothing || rcv==Nothing
        then (Nothing, (render errhexps, errstate))
        else let value = (fromJust rav, fromJust rbv, fromJust rcv) in
             if legal value
             then (Just value, (render errhexps, errstate))
             else (Nothing,    (renderError render errmsg errhexps, errstate))
  WuiSpec rendera showa reada = wuiSpecA mValA
  WuiSpec renderb showb readb = wuiSpecB mValB
  WuiSpec renderc showc readc = wuiSpecC mValC
  (mValA,mValB,mValC) = case mValABC of
                 Nothing      -> (Nothing, Nothing, Nothing)
                 Just (a,b,c) -> (Just a, Just b, Just c)

--- WUI combinator for tuples of arity 4.
w4Tuple :: WuiLensSpec a -> WuiLensSpec b -> WuiLensSpec c -> WuiLensSpec d -> WuiLensSpec (a,b,c,d)
w4Tuple wa wb wc wd =
  transformWSpec (isoLens inn out)
                 (wJoinTuple (wPair wa wb) (wPair wc wd))
 where
  out (a,b,c,d)     = ((a,b),(c,d))
  inn ((a,b),(c,d)) = (a,b,c,d)

--- WUI combinator for tuples of arity 5.
w5Tuple :: WuiLensSpec a -> WuiLensSpec b -> WuiLensSpec c -> WuiLensSpec d -> WuiLensSpec e ->
           WuiLensSpec (a,b,c,d,e)
w5Tuple wa wb wc wd we =
  transformWSpec (isoLens inn out)
                 (wJoinTuple (wTriple wa wb wc) (wPair wd we))
 where
  out (a,b,c,d,e)     = ((a,b,c),(d,e))
  inn ((a,b,c),(d,e)) = (a,b,c,d,e)

--- WUI combinator for tuples of arity 6.
w6Tuple :: WuiLensSpec a -> WuiLensSpec b -> WuiLensSpec c -> WuiLensSpec d -> WuiLensSpec e ->
           WuiLensSpec f -> WuiLensSpec (a,b,c,d,e,f)
w6Tuple wa wb wc wd we wf =
  transformWSpec (isoLens inn out)
                 (wJoinTuple (wTriple wa wb wc) (wTriple wd we wf))
 where
  out (a,b,c,d,e,f)     = ((a,b,c),(d,e,f))
  inn ((a,b,c),(d,e,f)) = (a,b,c,d,e,f)

--- WUI combinator for tuples of arity 7.
w7Tuple :: WuiLensSpec a -> WuiLensSpec b -> WuiLensSpec c -> WuiLensSpec d -> WuiLensSpec e ->
           WuiLensSpec f -> WuiLensSpec g -> WuiLensSpec (a,b,c,d,e,f,g)
w7Tuple wa wb wc wd we wf wg =
  transformWSpec (isoLens inn out)
                 (wJoinTuple (w4Tuple wa wb wc wd) (wTriple we wf wg))
 where
  out (a,b,c,d,e,f,g)     = ((a,b,c,d),(e,f,g))
  inn ((a,b,c,d),(e,f,g)) = (a,b,c,d,e,f,g)

--- WUI combinator for tuples of arity 8.
w8Tuple :: WuiLensSpec a -> WuiLensSpec b -> WuiLensSpec c -> WuiLensSpec d -> WuiLensSpec e ->
           WuiLensSpec f -> WuiLensSpec g -> WuiLensSpec h -> WuiLensSpec (a,b,c,d,e,f,g,h)
w8Tuple wa wb wc wd we wf wg wh =
  transformWSpec (isoLens inn out)
             (wJoinTuple (w4Tuple wa wb wc wd) (w4Tuple we wf wg wh))
 where
  out (a,b,c,d,e,f,g,h)   = ((a,b,c,d),(e,f,g,h))
  inn ((a,b,c,d),(e,f,g,h)) = (a,b,c,d,e,f,g,h)

--- WUI combinator for tuples of arity 9.
w9Tuple :: WuiLensSpec a -> WuiLensSpec b -> WuiLensSpec c -> WuiLensSpec d -> WuiLensSpec e ->
           WuiLensSpec f -> WuiLensSpec g -> WuiLensSpec h -> WuiLensSpec i ->
           WuiLensSpec (a,b,c,d,e,f,g,h,i)
w9Tuple wa wb wc wd we wf wg wh wi =
  transformWSpec (isoLens inn out)
                 (wJoinTuple (w5Tuple wa wb wc wd we) (w4Tuple wf wg wh wi))
 where
  out (a,b,c,d,e,f,g,h,i)     = ((a,b,c,d,e),(f,g,h,i))
  inn ((a,b,c,d,e),(f,g,h,i)) = (a,b,c,d,e,f,g,h,i)

--- WUI combinator for tuples of arity 10.
w10Tuple :: WuiLensSpec a -> WuiLensSpec b -> WuiLensSpec c -> WuiLensSpec d -> WuiLensSpec e ->
            WuiLensSpec f -> WuiLensSpec g -> WuiLensSpec h -> WuiLensSpec i -> WuiLensSpec j ->
            WuiLensSpec (a,b,c,d,e,f,g,h,i,j)
w10Tuple wa wb wc wd we wf wg wh wi wj =
  transformWSpec (isoLens inn out)
             (wJoinTuple (w5Tuple wa wb wc wd we) (w5Tuple wf wg wh wi wj))
 where
  out (a,b,c,d,e,f,g,h,i,j)     = ((a,b,c,d,e),(f,g,h,i,j))
  inn ((a,b,c,d,e),(f,g,h,i,j)) = (a,b,c,d,e,f,g,h,i,j)

--- WUI combinator for tuples of arity 11.
w11Tuple :: WuiLensSpec a -> WuiLensSpec b -> WuiLensSpec c -> WuiLensSpec d -> WuiLensSpec e ->
            WuiLensSpec f -> WuiLensSpec g -> WuiLensSpec h -> WuiLensSpec i -> WuiLensSpec j ->
            WuiLensSpec k -> WuiLensSpec (a,b,c,d,e,f,g,h,i,j,k)
w11Tuple wa wb wc wd we wf wg wh wi wj wk =
  transformWSpec (isoLens inn out)
                 (wJoinTuple (w5Tuple wa wb wc wd we) (w6Tuple wf wg wh wi wj wk))
 where
  out (a,b,c,d,e,f,g,h,i,j,k)     = ((a,b,c,d,e),(f,g,h,i,j,k))
  inn ((a,b,c,d,e),(f,g,h,i,j,k)) = (a,b,c,d,e,f,g,h,i,j,k)

--- WUI combinator for tuples of arity 12.
w12Tuple :: WuiLensSpec a -> WuiLensSpec b -> WuiLensSpec c -> WuiLensSpec d -> WuiLensSpec e ->
            WuiLensSpec f -> WuiLensSpec g -> WuiLensSpec h -> WuiLensSpec i -> WuiLensSpec j ->
            WuiLensSpec k -> WuiLensSpec l -> WuiLensSpec (a,b,c,d,e,f,g,h,i,j,k,l)
w12Tuple wa wb wc wd we wf wg wh wi wj wk wl =
  transformWSpec (isoLens inn out)
                 (wJoinTuple (w6Tuple wa wb wc wd we wf) (w6Tuple wg wh wi wj wk wl))
 where
   out (a,b,c,d,e,f,g,h,i,j,k,l)     = ((a,b,c,d,e,f),(g,h,i,j,k,l))
   inn ((a,b,c,d,e,f),(g,h,i,j,k,l)) = (a,b,c,d,e,f,g,h,i,j,k,l)

--- WUI combinator to combine two tuples into a joint tuple.
--- It is similar to wPair but renders both components as a single
--- tuple provided that the components are already rendered as tuples,
--- i.e., by the rendering function <code>renderTuple</code>.
--- This combinator is useful to define combinators for large tuples.
wJoinTuple :: WuiLensSpec a -> WuiLensSpec b -> WuiLensSpec (a,b)
wJoinTuple wuiSpecA wuiSpecB mValAB =
  WuiSpec (renderTuple, tupleError, const True) showc readc
 where
  render2joinrender render [h1,h2] =
    let h1s = unRenderTuple h1
        h2s = unRenderTuple h2
     in render (h1s++h2s)

  showc wparams (va,vb) =
    let (hea,rta) = showa rendera va
        (heb,rtb) = showb renderb vb
     in (render2joinrender (renderOf wparams) [hea,heb],states2state [rta,rtb])

  readc (orgrender,errmsg,legal) env s =
    let [ra,rb] = state2states s
        (rav,(hea,rta)) = reada rendera env ra
        (rbv,(heb,rtb)) = readb renderb env rb
        errhexps = [hea,heb]
        errstate = states2state [rta,rtb]
        render = render2joinrender orgrender
     in if rav==Nothing || rbv==Nothing
        then (Nothing, (render errhexps, errstate))
        else let value = (fromJust rav, fromJust rbv) in
             if legal value
             then (Just value, (render errhexps, errstate))
             else (Nothing,    (renderError render errmsg errhexps, errstate))
  WuiSpec rendera showa reada = wuiSpecA mValA
  WuiSpec renderb showb readb = wuiSpecB mValB
  (mValA,mValB) = case mValAB of
                       Nothing    -> (Nothing, Nothing)
                       Just (a,b) -> (Just a, Just b)


--- WUI combinator for list structures where the list elements are vertically
--- aligned in a table.
wList :: WuiLensSpec a -> WuiLensSpec [a]
wList wuiSpecA mValLA =
  WuiSpec (renderList,"Illegal list:",const True)
          (\wparams vas ->
              listWidget (renderOf wparams) (unzip (map (showa rendera) vas)))
          (\ (render,errmsg,legal) env s ->
            let rvs = map (reada rendera env) (state2states s)
             in if Nothing `elem` (map fst rvs)
                then (Nothing, listWidget render (unzip (map snd rvs)))
                else let value = map (fromJust . fst) rvs in
                     if legal value
                     then (Just value, listWidget render (unzip (map snd rvs)))
                     else (Nothing, listWidget (renderError render errmsg)
                                               (unzip (map snd rvs))) )
 where
  listWidget render (hes,refs) = (render hes, states2state refs)
  WuiSpec rendera showa reada = wuiSpecA mValA
  mValA = case mValLA of
               Nothing     -> Nothing
               Just (x:xs) -> Just x

--- Add headings to a standard WUI for list structures:
wListWithHeadings :: [String] -> WuiLensSpec a -> WuiLensSpec [a]
wListWithHeadings headings wspec =
  wList wspec `withRendering` renderHeadings
 where
  renderHeadings hs = addHeadings (renderList hs) (map (\s->[htxt s]) headings)

--- WUI combinator for list structures where the list elements are horizontally
--- aligned in a table.
wHList :: WuiLensSpec a -> WuiLensSpec [a]
wHList wspec = wList wspec `withRendering` renderTuple


--- WUI for matrices, i.e., list of list of elements
--- visualized as a matrix.
wMatrix :: WuiLensSpec a -> WuiLensSpec [[a]]
wMatrix wspec mVal = wList (wHList wspec) mVal


--- WUI for Maybe values. It is constructed from a WUI for
--- Booleans and a WUI for the potential values. Nothing corresponds
--- to a selection of False in the Boolean WUI.
--- The value WUI is shown after the Boolean WUI.
--- @param wspecb - a WUI specification for Boolean values
--- @param wspeca - a WUI specification for the type of potential values
--- @param def - a default value that is used if the current value is Nothing
wMaybe :: WuiLensSpec Bool -> WuiLensSpec a -> a -> WuiLensSpec (Maybe a)
wMaybe wuiSpecBool wuiSpecA def mValA =
 WuiSpec
   (renderTuple, tupleError, const True)
   (\wparams mbs ->
     let (heb,rtb) = showb paramb (mbs/=Nothing)
         (hea,rta) = showa parama (maybe def id mbs)
      in ((renderOf wparams) [heb,hea], states2state [rtb,rta]))
   (\ (render,errmsg,legal) env s ->
     let [rb,ra] = state2states s
         (rbv,(heb,rtb)) = readb paramb env rb
         (rav,(hea,rta)) = reada parama env ra
         errhexps = [heb,hea]
         errstate = states2state [rtb,rta]
      in if rbv==Nothing || rav==Nothing
         then (Nothing, (render errhexps, errstate))
         else let value = if fromJust rbv
                          then Just (fromJust rav)
                          else Nothing in
              if legal value
              then (Just value, (render errhexps, errstate))
              else (Nothing,    (renderError render errmsg errhexps, errstate)))
 where
  WuiSpec paramb showb readb = wuiSpecBool (Just (mVal /= Nothing))
  WuiSpec parama showa reada = wuiSpecA mVal
  mVal = case mValA of
              Just v -> v
              Nothing -> Nothing

--- A WUI for Maybe values where a check box is used to select Just.
--- The value WUI is shown after the check box.
--- @param wspec - a WUI specification for the type of potential values
--- @param hexps - a list of HTML expressions shown after the check box
--- @param def - a default value if the current value is Nothing
wCheckMaybe :: WuiLensSpec a -> [HtmlExp] -> a -> WuiLensSpec (Maybe a)
wCheckMaybe wspec exps def = wMaybe (wCheckBool exps) wspec def

--- A WUI for Maybe values where radio buttons are used to switch
--- between Nothing and Just.
--- The value WUI is shown after the radio button WUI.
--- @param wspec - a WUI specification for the type of potential values
--- @param hexps - a list of HTML expressions shown after the Nothing button
--- @param hexps - a list of HTML expressions shown after the Just button
--- @param def - a default value if the current value is Nothing
wRadioMaybe :: WuiLensSpec a -> [HtmlExp] -> [HtmlExp] -> a -> WuiLensSpec (Maybe a)
wRadioMaybe wspec hnothing hjust def mVal= wMaybe wBool wspec def mVal
 where
  wBool = wRadioSelect (\b->if b then hjust else hnothing) [False,True]


--- WUI for union types.
--- Here we provide only the implementation for Either types
--- since other types with more alternatives can be easily reduced to this case.
wEither :: WuiLensSpec a -> WuiLensSpec b -> WuiLensSpec (Either a b)
wEither wuiSpecA wuiSpecB mValEitherAB =
 WuiSpec (head, "?", const True) showEither readEither
 where
  showEither wparams (Left va) =
    let (hea,rta) = showa rendera va
     in ((renderOf wparams) [hea], altstate2state (1,rta))
  showEither wparams (Right vb) =
    let (heb,rtb) = showb renderb vb
     in ((renderOf wparams) [heb], altstate2state (2,rtb))

  readEither (render,errmsg,legal) env s =
    let (altindex,rab) = state2altstate s
     in case altindex of
         1 -> let (rv,(he,rst)) = reada rendera env rab
               in checkValue (rv==Nothing) (Left (fromJust rv))
                             he (altstate2state(1,rst))
         2 -> let (rv,(he,rst)) = readb renderb env rab
               in checkValue (rv==Nothing) (Right (fromJust rv))
                             he (altstate2state(2,rst))
   where
     checkValue isnothing value hexp altstate =
        if isnothing
        then (Nothing, (render [hexp], altstate))
        else if legal value
             then (Just value, (render [hexp], altstate))
             else (Nothing,    (renderError render errmsg [hexp], altstate))
  WuiSpec rendera showa reada = wuiSpecA mValA
  WuiSpec renderb showb readb = wuiSpecB mValB
  (mValA,mValB) = case mValEitherAB of
                       Nothing        -> (Nothing, Nothing)
                       Just (Left v)  -> (Just v, Nothing)
                       Just (Right v) -> (Nothing, Just v)

--- A simple tree structure to demonstrate the construction of WUIs for tree
--- types.
data WTree a = WLeaf a | WNode [WTree a]

--- WUI for tree types.
--- The rendering specifies the rendering of inner nodes.
--- Leaves are shown with their default rendering.
wTree :: WuiLensSpec a -> WuiLensSpec (WTree a)
wTree wuiSpecA mValTree =
 WuiSpec (renderList, "Illegal tree:", const True) showTree readTree
 where
  showTree _ (WLeaf va) =
    let (hea,rta) = showa rendera va
     in (hea, altstate2state (1,rta))
  showTree wparams (WNode ns) =
    let (hes,sts) = unzip (map (showTree wparams) ns)
     in ((renderOf wparams) hes, altstate2state (2,states2state sts))

  readTree wpar env s =
    let (altindex,rab) = state2altstate s
     in case altindex of
         1 -> let (rv,(he,rst)) = reada rendera env rab
               in checkValue (rv==Nothing) (WLeaf (fromJust rv)) head
                             [he] (altstate2state(1,rst))
         2 -> let rvs = map (readTree wpar env) (state2states rab)
               in checkValue (Nothing `elem`  (map fst rvs))
                             (WNode (map (fromJust . fst) rvs)) (renderOf wpar)
                             (map (fst . snd) rvs)
                          (altstate2state(2,states2state (map (snd . snd) rvs)))
   where
     checkValue isnothing value rendertree hexps altstate =
        if isnothing
        then (Nothing, (rendertree hexps, altstate))
        else if conditionOf wpar value
             then (Just value, (rendertree hexps, altstate))
             else (Nothing,    (renderError rendertree (errorOf wpar) hexps,
                                altstate))
  WuiSpec rendera showa reada = wuiSpecA mValA
  mValA = case mValTree of
               Just (WLeaf v) -> Just v
               Just (WNode _) -> Nothing
               Nothing        -> Nothing


-------------------------------------------------------------------------------
-- Definition of standard rendering functions

--- Standard rendering of tuples as a table with a single row.
--- Thus, the elements are horizontally aligned.
renderTuple :: Rendering
renderTuple hexps = table [map (\h->[h]) hexps]

--- Inverse operation of renderTuple. If the argument has not the
--- shape of the renderTuple output, it is returned unchanged.
--- In future versions, this operation is better implemented using
--- functional logic features, but currently the encapsulated search
--- is a bit weak for this purpose.
unRenderTuple :: HtmlExp -> [HtmlExp]
unRenderTuple hexp =
  if isTupleTable hexp
  then getTupleTableElems hexp
  else [hexp]
 where
  isTupleTable he = case he of
    HtmlStruct "table" [] [HtmlStruct "tr" [] tds] -> all isSingleElem tds
    _ -> False

  isSingleElem he = case he of
    HtmlStruct "td" _ [_] -> True
    _ -> False

  getTupleTableElems (HtmlStruct "table" [] [HtmlStruct "tr" [] tds]) =
    map (\ (HtmlStruct "td" _ [e]) -> e) tds

-- Standard error message for tuples:
tupleError = "Illegal combination:"

--- Standard rendering of tuples with a tag for each element.
--- Thus, each is preceded by a tag, that is set in bold, and all
--- elements are vertically aligned.
renderTaggedTuple :: [String] -> Rendering
renderTaggedTuple tags hexps =
  table (map (\(t,h)->[[bold [htxt t]],[h]]) (zip tags hexps))

--- Standard rendering of lists as a table with a row for each item:
--- Thus, the elements are vertically aligned.
renderList :: Rendering
renderList hexps = mergeTableOfTable (table (map (\h->[[h]]) hexps))
                                                `addAttr` ("border","1")

-- Combine a rendering with an error message.
-- The error message is put as the first row of a table with background color
-- yellow.
renderError :: Rendering -> String -> Rendering
renderError render errmsg hexps =
  table [[[boldRedTxt errmsg]], [[render hexps]]] 
                  `addAttr` ("bgcolor","#ffff00") -- background color: yellow

boldRedTxt s = HtmlStruct "font" [("color","#ff0000")] [bold [htxt s]]


mergeTableOfTable :: HtmlExp -> HtmlExp
mergeTableOfTable (HtmlStruct "table" attrs rows) =
  HtmlStruct "table" attrs
             (if all isRowWithSingleTableData rows
              then map mergeRowWithSingleTableData rows
              else rows )

isRowWithSingleTableData row = case row of
   (HtmlStruct "tr" []
        [HtmlStruct "td" []
            [HtmlStruct "table" _ [HtmlStruct "tr" _ _]]]) -> True
   _ -> False

mergeRowWithSingleTableData 
  (HtmlStruct "tr" [] [HtmlStruct "td" [] [HtmlStruct "table" _ [row]]]) = row


-------------------------------------------------------------------------------
-- Main operations to generate HTML structures and handlers from
-- WUI specifications:

--- Generates an HTML form from a WUI data specification,
--- an initial value and an update form.
mainWUI :: WuiLensSpec a -> a -> (a -> IO HtmlForm) -> IO HtmlForm
mainWUI wuispec val store = do
  let (hexp,handler) = wui2html wuispec val store
  return $ form "WUI" [hexp, breakline, wuiHandler2button "Submit" handler]

--- Generates HTML editors and a handler from a WUI data specification,
--- an initial value and an update form.
wui2html :: WuiLensSpec a -> a -> (a -> IO HtmlForm) -> (HtmlExp,WuiHandler)
wui2html wspec val store = wuiWithErrorForm wspec val store standardErrorForm

--- A standard error form for WUIs.
standardErrorForm :: HtmlExp -> WuiHandler -> IO HtmlForm
standardErrorForm hexp whandler =
  return $ standardForm "Input error"
                        [hexp, wuiHandler2button "Submit" whandler]


--- Puts a WUI into a HTML form containing "holes" for the WUI and the
--- handler.
wuiInForm :: WuiLensSpec a -> a -> (a -> IO HtmlForm)
             -> (HtmlExp -> WuiHandler -> IO HtmlForm) -> IO HtmlForm
wuiInForm wspec val store userform =
  answerForm (wuiWithErrorForm wspec val store userform)
 where
  answerForm (hexp,whandler) = userform hexp whandler

--- Generates HTML editors and a handler from a WUI data specification,
--- an initial value and an update form. In addition to wui2html,
--- we can provide a skeleton form used to show illegal inputs.
wuiWithErrorForm :: WuiLensSpec a -> a -> (a -> IO HtmlForm)
                    -> (HtmlExp -> WuiHandler -> IO HtmlForm)
                    -> (HtmlExp,WuiHandler)
wuiWithErrorForm wspec val store errorform =
        showAndReadWUI val wspec store errorform (generateWUI wspec val)

generateWUI :: WuiLensSpec a -> a -> (HtmlExp, CgiEnv -> (Maybe a,HtmlState))
generateWUI wuiSpecA val = hst2result (showhtml wparams val)
  where
    hst2result (htmledits,wstate) = (htmledits, \env -> readval wparams env wstate)
    WuiSpec wparams showhtml readval = wuiSpecA (Just val)

showAndReadWUI :: a -> WuiLensSpec a -> (a -> IO HtmlForm)
                            -> (HtmlExp -> WuiHandler -> IO HtmlForm)
                            -> (HtmlExp,CgiEnv -> (Maybe a,HtmlState))
                            -> (HtmlExp,WuiHandler)
showAndReadWUI valA wSpecA store errorForm (htmlEdits,readEnv) =
  (htmlEdits, WHandler htmlHandler)
 where
  htmlHandler env =
    let (mValNew, (htmlErrorForm,errorWState)) = readEnv env
        WuiSpec wParams _ readVal = wSpecA (Just valA)
        (errorExp,errorHandler) =
          showAndReadWUI valA
                         wSpecA
                         store
                         errorForm
                         ( htmlErrorForm
                         , \env -> readVal wParams env errorWState )
    in maybe (errorForm errorExp errorHandler)
             (\newVal -> seq (normalForm newVal) -- to strip off unused lvars
                             (store newVal))
             mValNew
  

--------------------------------------------------------------------------
