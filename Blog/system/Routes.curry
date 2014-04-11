--------------------------------------------------------------------------
--- This module defines operations to support the handling of routes
--- to controllers.
--------------------------------------------------------------------------

module Routes(
  getControllerReference,
  getRouteMenu
) where

import HTML
import RoutesData

--generated in RoutesData
--type Route = (String, UrlMatch, ControllerReference)

--- Gets the reference of a controller corresponding to a given URL
--- according to the definition of all routes specified in
--- module RouteData.
getControllerReference :: String -> IO (Maybe ControllerReference)
getControllerReference url = getRoutes >>= return . findControllerReference
  where
    findControllerReference :: [Route] -> Maybe ControllerReference
    findControllerReference ((_, matcher, fktref):restroutes) =
      case matcher of
        Exact string -> if (url == string)
                        then Just fktref
                        else findControllerReference restroutes
        Matcher fkt  -> if (fkt url)
                        then Just fktref
                        else findControllerReference restroutes
        Always       -> Just fktref
    findControllerReference [] = Nothing -- no controller found for url

--- Generates the menu for all route entries put on the top of
--- each page. As a default, all routes specified with URL matcher
--- Exact in the module RouteData are taken as menu entries.
getRouteMenu :: IO HtmlExp
getRouteMenu = do
  routes <- getRoutes
  return $ ulist (getLinks routes)
 where
   getLinks :: [Route] -> [[HtmlExp]]
   getLinks ((name, matcher, _):restroutes) =
     case matcher of
       Exact string -> [(href ("?" ++ string) [htxt name])]
                        : getLinks restroutes
       _ -> getLinks restroutes
   getLinks [] = []
