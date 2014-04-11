--------------------------------------------------------------------------
--- This main module of a Spicey application.
--------------------------------------------------------------------------

module Main where

import ControllerMapping
import Spicey
import WUI
import HTML
import Routes
import RoutesData
import Processes


dispatcher :: IO HtmlForm
dispatcher = do
  -- get url
  (url,ctrlparams) <- getControllerURL
  
  controller <- nextControllerRefInProcessOrForUrl url >>=
                maybe (displayError "Illegal URL!")
                      getController

  form <- getForm controller
  saveLastUrl (url ++ concatMap ("/"++) ctrlparams)
  return form

--- Main function: call the dispatcher
main = dispatcher
