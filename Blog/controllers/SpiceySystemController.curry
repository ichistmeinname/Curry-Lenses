--------------------------------------------------------------------------
--- This module contains some controller that might be used in in
--- Spicey application.
--- In particular, it provides a controller for login/out and
--- a controller to start selected user processes.
--------------------------------------------------------------------------

module SpiceySystemController
  (loginController,processListController,historyController)
 where

import Spicey
import Session
import UserProcesses
import Processes
import SpiceySystemView
import ReadNumeric
import Authentication
import DefaultController

-----------------------------------------------------------------------------
--- Controller for login/logout.
loginController :: Controller
loginController = do
  login <- getSessionLogin
  return $ loginView defaultController login

-----------------------------------------------------------------------------
--- Controller for showing and selecting user processes.
processListController :: Controller
processListController = do
  args <- getControllerParams
  if null args
   then return $ processListView availableProcesses
   else case (readInt (head args)) of
          Just (idInt, _) -> do
            startProcess (processNames availableProcesses !! (idInt - 1))
          Nothing ->
            displayError "could not read process id"

-----------------------------------------------------------------------------
--- Controller for the URL history.
historyController :: Controller
historyController = getLastUrls >>= return . historyView

-----------------------------------------------------------------------------
