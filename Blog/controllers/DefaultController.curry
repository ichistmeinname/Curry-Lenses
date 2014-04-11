module DefaultController where

import EntryController
import Spicey

--- The default controller of the application.
defaultController :: Controller
defaultController = listEntryController
