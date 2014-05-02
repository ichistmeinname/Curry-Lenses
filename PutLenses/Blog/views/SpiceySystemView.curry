--------------------------------------------------------------------------
--- This module implements the views related to the standard controllers
--- in a Spicey application.
--- In particular, it defines a default view for login
--- and a view of a list of user processes.
--------------------------------------------------------------------------

module SpiceySystemView(loginView,processListView,historyView)
 where

import UserProcesses
import Processes
import Spicey
import Authentication

-----------------------------------------------------------------------------
--- View for login/logout. If the passed login name is the empty string,
--- we offer a login dialog, otherwise a logout dialog.
loginView :: Controller -> Maybe String -> [HtmlExp]
loginView controller currlogin =
  case currlogin of
   Nothing -> [h3 [htxt "Login as:"],
               textfield loginfield "",
               spButton "Login" loginHandler]
   Just _  -> [h3 [htxt "Really logout?"],
               spPrimButton "Logout" (logoutHandler True),
               spButton "Cancel" (logoutHandler False)]
 where
  loginfield free

  loginHandler env = do
    let loginname = env loginfield
    -- In the real system, you should also verify a password here.
    if null loginname
      then done
      else do loginToSession loginname
              setPageMessage ("Logged in as: "++loginname)
    nextInProcessOr controller Nothing >>= getForm

  logoutHandler confirm _ = do
    if confirm then logoutFromSession >> setPageMessage "Logged out"
               else done
    nextInProcessOr controller Nothing >>= getForm

-----------------------------------------------------------------------------
--- A view for all processes contained in a given process specification.
processListView :: Processes a -> [HtmlExp]
processListView procs =
  [h1 [htxt "Processes"],
   ulist (map processColumn (zip (processNames procs) [1..]))]
 where
   processColumn (pname, id) =
     [href ("?spiceyProcesses/"++show id) [htxt pname]]

-----------------------------------------------------------------------------
--- A view for all URLs of a session.
historyView :: [String] -> [HtmlExp]
historyView urls =
  [h1 [htxt "History"],
   ulist (map (\url -> [href ("?"++url) [htxt url]])
              (filter (not . null) urls))]

-----------------------------------------------------------------------------
