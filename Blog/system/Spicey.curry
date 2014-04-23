--------------------------------------------------------------------------
--- This module implements some auxiliary operations to support the
--- generic implementation of the Spicey entities.
--------------------------------------------------------------------------

module Spicey (
  module System, 
  module HTML, 
  module ReadNumeric, 
  module WUILenses,
  Controller,
  nextController, nextControllerForData, confirmNextController,
  getControllerURL,getControllerParams, showControllerURL,
  getForm, wDateType, wBoolean, wUncheckMaybe,
  displayError, cancelOperation,
  wuiEditForm, wuiFrameToForm, nextInProcessOr,
  renderLabels,
  stringToHtml, maybeStringToHtml,
  intToHtml,maybeIntToHtml, floatToHtml, maybeFloatToHtml,
  boolToHtml, maybeBoolToHtml, calendarTimeToHtml, maybeCalendarTimeToHtml,
  spButton, spPrimButton, spSmallButton, spTable,
  setPageMessage, getPageMessage,
  saveLastUrl, getLastUrl, getLastUrls
  ) where

import System
import HTML
import ReadNumeric
import KeyDatabase
import WUILenses
import Time
import Routes
import Processes
import UserProcesses
import Session
import Global
import Authentication
import Monadic

---------------- vvvv -- Framework functions -- vvvv -----------------------

-- a viewable can be turned into a representation which can be displayed
-- as interface
-- here: a representation of a HTML page
type Viewable = HtmlPage

type ViewBlock = [HtmlExp]

--- Controllers contains all logic and their result should be a Viewable.
--- if the behavior of controller should depend on URL parameters
--- (following the first name specifying the controller), one
--- can access these URL parameters by using the operation
--- Spicey.getControllerParams inside the controller.
type Controller = IO ViewBlock

nextController :: Controller -> _ -> IO HtmlForm
nextController controller _ = do
  view <- controller
  getForm view

-- for WUIs
nextControllerForData :: (a -> Controller) -> a -> IO HtmlForm
nextControllerForData controller param = do
  view <- controller param
  getForm view

--- Call the next controller after a user confirmation.
--- The Boolean user answer is passed as an argument to the controller.
confirmNextController :: HtmlExp -> (Bool -> Controller) -> _ -> IO HtmlForm
confirmNextController question controller _ = do
  getForm [question,
           spButton "Yes" (nextController (controller True)),
           spButton "No"  (nextController (controller False))]

--- If we are in a process, execute the next process depending on
--- the provided information passed in the second argument,
--- otherwise execute the given controller (first argument).
nextInProcessOr :: Controller -> Maybe ControllerResult -> Controller
nextInProcessOr controller arg = do
  isproc <- isInProcess
  if isproc then advanceInProcess arg >> return [htxt ""] -- triggers redirect
            else controller


--------------------------------------------------------------------------
-- Operations for handling URL parameters

--- Parse the URL parameter passed to the main script. The result is a pair
--- consisting of the route and the list of parameters separated by '/'.
parseUrl :: String -> (String, [String])
parseUrl urlparam =
  let (url:ctrlparams) = splitUrl urlparam
  in  (url,ctrlparams)
  
--- Splits the URL parameter passed to the main script into a list of
--- strings. The strings are separated in the URL by '/'.
splitUrl :: String -> [String]
splitUrl url =
  let (ys,zs) = break (== '/') url
   in if null zs then [ys]
                 else ys : splitUrl (tail zs)

--- Gets the controller URL and the control parameters (separated by '/').
--- For instance, if the spicey script is called with the URL
--- "spicey.cgi?listEntity/arg1/arg2", this operation returns
--- ("listEntity",["arg1","arg2"]).
getControllerURL :: IO (String, [String])
getControllerURL = getUrlParameter >>= return . parseUrl

--- Gets the control parameters from the current URL.
getControllerParams :: IO [String]
getControllerParams = getUrlParameter >>= return . snd . parseUrl

--- Shows the URL corresponding to the control parameters.
--- The first argument is the URL of the controller (e.g., "listEntity")
--- and the second argument is the list of control parameters.
showControllerURL :: String -> [String] -> String
showControllerURL ctrlurl params = '?' : ctrlurl ++ concatMap ('/':) params

--------------------------------------------------------------------------

-- A standard HTML frame for editing data with WUIs.
wuiEditForm :: String -> String -> Controller
           -> HtmlExp -> WuiHandler -> [HtmlExp]
wuiEditForm title buttontag controller hexp handler =
  [h1 [htxt title],
   blockstyle "editform" [hexp],
   wuiHandler2button buttontag handler `addClass` "btn btn-primary",
   spButton "cancel" (nextController (cancelOperation >> controller))]

--- Transforms a WUI frame into a standard form.
wuiFrameToForm :: (HtmlExp -> WuiHandler -> [HtmlExp])
               -> HtmlExp -> WuiHandler -> IO HtmlForm
wuiFrameToForm wframe hexp wuihandler = getForm (wframe hexp wuihandler)

--- A WUI for manipulating CalendarTime entities.
--- It is based on a WUI for dates, i.e., the time is ignored.
wDateType :: WuiLensSpec CalendarTime
wDateType = transformWSpec dateLens wDate
 where
  dateLens = isoLens inn out
  inn :: (Int, Int, Int) -> CalendarTime
  inn (day, month, year) = CalendarTime year month day 0 0 0 0

  out :: CalendarTime -> (Int, Int, Int)
  out (CalendarTime year month day _ _ _ _) = (day, month, year)

--- A WUI for manipulating date entities.
wDate :: WuiLensSpec (Int, Int, Int)
wDate = wTriple (wSelectInt [1..31])
                 (wSelectInt [1..12])
                 (wSelectInt [1950..2050])

--- A WUI for manipulating Boolean entities. In general, this view should
--- be specialized by replacing true and false by more comprehensible strings.
wBoolean :: WuiLensSpec Bool
wBoolean = wSelectBool "True" "False"

--- A WUI transformer to map WUIs into WUIs for corresponding Maybe types.
wUncheckMaybe :: a -> WuiLensSpec a -> WuiLensSpec (Maybe a)
wUncheckMaybe defval wspec =
  wMaybe (transformWSpec (isoLens not not) (wCheckBool [htxt "No value"]))
         wspec
         defval

--- The title of this application (shown in the header).
spiceyTitle :: String
spiceyTitle = "Spicey Application"

--- Adds the basic page layout to a view.
addLayout :: ViewBlock -> IO ViewBlock
addLayout viewblock = do
  routemenu <- getRouteMenu
  msg       <- getPageMessage
  login     <- getSessionLogin
  lasturl   <- getLastUrl
  return $
    stdNavBar routemenu login ++
    [blockstyle "container-fluid" $
      [HtmlStruct "header" [("class","hero-unit")] [h1 [htxt spiceyTitle]],
       if null msg
        then HtmlStruct "header" [("class","pagemessage pagemessage-empty")]
                        [htxt ("Last page: "++lasturl)]
        else HtmlStruct "header" [("class","pagemessage")] [htxt msg],
       blockstyle "row-fluid"
        [blockstyle "span12" viewblock],
       hrule,
       HtmlStruct "footer" []
        [par [htxt "powered by",
              href "http://www.informatik.uni-kiel.de/~pakcs/spicey"
                   [image "images/spicey-logo.png" "Spicey"]
                 `addAttr` ("target","_blank"),
              htxt "Framework"]]]]

-- Standard navigation bar at the top.
-- The first argument is the route menu (a ulist).
-- The second argument is the possible login name.
stdNavBar :: HtmlExp -> Maybe String -> [HtmlExp]
stdNavBar routemenu login =
  [blockstyle "navbar navbar-inverse navbar-fixed-top"
    [blockstyle "navbar-inner"
      [blockstyle "container-fluid"
         [routemenu `addClass` "nav",
          par [htxt $ "Logged in as: "++maybe "" id login]
            `addClass` "navbar-text pull-right"]
      ]
    ]
  ]

getForm :: ViewBlock -> IO HtmlForm
getForm viewBlock =
  if viewBlock == [HtmlText ""]
  then return $ HtmlForm "forward to Spicey"
                  [HeadInclude (HtmlStruct "meta"
                                 [("http-equiv","refresh"),
                                  ("content","1; url=spicey.cgi")] [])]
                  [par [htxt "You will be forwarded..."]]
  else do
    cookie  <- sessionCookie
    body    <- addLayout viewBlock
    return $ HtmlForm spiceyTitle
                      ([responsiveView, cookie, icon] ++
                       map (\f -> FormCSS $ "css/"++f++".css")
                           ["bootstrap","bootstrap-responsive","style"])
                      body
 where
   responsiveView =
     HeadInclude (HtmlStruct "meta"
                    [("name","viewport"),
                     ("content","width=device-width, initial-scale=1.0")] [])

   icon = HeadInclude (HtmlStruct "link"
                                  [("rel","shortcut icon"),
                                   ("href","favicon.ico")] [])

-- Action performed when a "cancel" button is pressed.
-- In this case, a message is shown.
cancelOperation :: IO ()
cancelOperation = do
  inproc <- isInProcess
  if inproc then removeCurrentProcess else done
  setPageMessage $ (if inproc then "Process" else "Operation") ++ " cancelled"

-- dummy-controller to display an error
displayError :: String -> Controller
displayError msg = do
  inproc <- isInProcess
  if inproc then removeCurrentProcess else done
  setPageMessage ("Error occurred!" ++
                  if inproc then " Process terminated!" else "")
  if null msg
   then return [htxt "General error (shown by function Spicey.displayError)"]
   else return [htxt msg]

-- like renderTaggedTuple from WUI Library but takes list of HtmlExp
-- instead of list of strings
renderLabels :: [[HtmlExp]] -> Rendering
renderLabels labels hexps =
  spTable (map (\(l, h) -> [l, [enlargeInput h]]) (zip labels hexps))
 where
  enlargeInput h = h `addClass` "input-xxlarge"

-- convert standard-datatype-values to html representation
stringToHtml :: String -> HtmlExp
stringToHtml s = textstyle "type_string" s

maybeStringToHtml :: Maybe String -> HtmlExp
maybeStringToHtml s = textstyle "type_string" (maybe "" id s)

intToHtml :: Int -> HtmlExp
intToHtml i = textstyle "type_int" (show i)

maybeIntToHtml :: Maybe Int -> HtmlExp
maybeIntToHtml i = textstyle "type_int" (maybe "" show i)

floatToHtml :: Float -> HtmlExp
floatToHtml i = textstyle "type_float" (show i)

maybeFloatToHtml :: Maybe Float -> HtmlExp
maybeFloatToHtml i = textstyle "type_float" (maybe "" show i)

boolToHtml :: Bool -> HtmlExp
boolToHtml b = textstyle "type_bool" (show b)

maybeBoolToHtml :: Maybe Bool -> HtmlExp
maybeBoolToHtml b = textstyle "type_bool" (maybe "" show b)

calendarTimeToHtml :: CalendarTime -> HtmlExp
calendarTimeToHtml ct = textstyle "type_calendartime" (toDayString ct)

maybeCalendarTimeToHtml :: Maybe CalendarTime -> HtmlExp
maybeCalendarTimeToHtml ct =
  textstyle "type_calendartime" (maybe "" toDayString ct)

--------------------------------------------------------------------------
-- Auxiliary HTML items:

--- Input button in Spicey (rendered as a default button):
spButton :: String -> HtmlHandler -> HtmlExp
spButton label handler =
  button label handler `addClass` "btn"

--- Primary input button in Spicey (rendered as a primary button):
spPrimButton :: String -> HtmlHandler -> HtmlExp
spPrimButton label handler =
  button label handler `addClass` "btn btn-primary"

--- Small input button in Spicey (rendered as a small button):
spSmallButton :: String -> HtmlHandler -> HtmlExp
spSmallButton label handler =
  button label handler `addClass` "btn btn-small"

--- Standard table in Spicey.
spTable :: [[[HtmlExp]]] -> HtmlExp
spTable items = table items  `addClass` "table table-hover table-condensed"

--------------------------------------------------------------------------
-- The page messages are implemented by a session store.
-- We define a global variable to store a message which is shown
-- in the next HTML page of a session.

--- Definition of the session state to store the page message (a string).
pageMessage :: Global (SessionStore String)
pageMessage = global emptySessionStore Temporary

--- Gets the page message and delete it.
getPageMessage :: IO String
getPageMessage = do
  msg <- getSessionData pageMessage
  removeSessionData pageMessage
  return (maybe "" id msg)

--- Set the page message of the current session.
setPageMessage :: String -> IO ()
setPageMessage msg = putSessionData msg pageMessage

--------------------------------------------------------------------------
-- Another example for using sessions.
-- We store the list of selected URLs into  the current session.

--- Definition of the session state to store the last URL (as a string).
lastUrls :: Global (SessionStore [String])
lastUrls = global emptySessionStore Temporary

--- Gets the list of URLs of the current session.
getLastUrls :: IO [String]
getLastUrls = getSessionData lastUrls >>= return . maybe [] id

--- Gets the last URL of the current session (or "?").
getLastUrl :: IO String
getLastUrl = do urls <- getLastUrls
                return (if null urls then "?" else head urls)

--- Saves the last URL of the current session.
saveLastUrl :: String -> IO ()
saveLastUrl url = do
  urls <- getLastUrls
  putSessionData (url:urls) lastUrls

--------------------------------------------------------------------------
