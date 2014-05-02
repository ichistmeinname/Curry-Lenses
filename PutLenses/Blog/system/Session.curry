--------------------------------------------------------------------------
--- This module implements the management of sessions.
--- In particular, it defines a cookie that must be sent to the client
--- in order to enable the handling of sessions.
--- Based on session, this module defines also a session store
--- that can be used by various parts of the application in order
--- to hold some session-specific data.
--------------------------------------------------------------------------

module Session (
  sessionCookie,
  SessionStore, emptySessionStore,
  getSessionData, putSessionData, removeSessionData
  ) where

import HTML
import Time
import Global
import List

--- The life span in minutes to store data in sessions.
--- Thus, older data is deleted by a clean up that is initiated
--- whenever new data is stored in a session.
sessionLifespan = 60

--- The name of the persistent global where the last session id is stored.
sessionCookieName = "spiceySessionId"

--- This global value saves time and last session id.
lastId :: Global (Int, Int)
lastId = global (0, 0) (Persistent sessionCookieName)


--- The abstract type to represent session identifiers.
data SessionId = SessionId String

getId :: SessionId -> String
getId (SessionId i) = i

--- Creates a new unused session id.
getUnusedId :: IO SessionId
getUnusedId = do
  (ltime,lsid) <- readGlobal lastId
  clockTime <- getClockTime
  if clockTimeToInt clockTime /= ltime
    then writeGlobal lastId (clockTimeToInt clockTime, 0)
    else writeGlobal lastId (clockTimeToInt clockTime, lsid+1)
  return (SessionId (show (clockTimeToInt clockTime) ++ show (lsid+1)))

--- Gets the id of the current user session.
--- If this is a new session, a new id is created and returned.
getSessionId :: IO SessionId
getSessionId = do
    cookies <- getCookies
    case (lookup sessionCookieName cookies) of
      Just sessionCookieValue -> return (SessionId sessionCookieValue)
      Nothing                 -> getUnusedId

--- Creates a cookie to hold the current session id.
--- This cookie should be sent to the client together with every form.
sessionCookie :: IO FormParam
sessionCookie = do
    sessionId <- getSessionId
    clockTime <- getClockTime
    return (FormCookie sessionCookieName (getId (sessionId))
                       [CookiePath "/",
                        CookieExpire (addMinutes sessionLifespan clockTime)])

----------------------------------------------------------------------------
-- Implementation of session stores.

--- The type of a session store that holds particular data used in a session.
--- A session store consists of list of data items for each session in the
--- system together with the clock time of the last access.
--- The clock time is used to remove old data in the store.
data SessionStore a = SStore [(SessionId, Int, a)]

--- An initial value for the empty session store.
emptySessionStore = SStore []

--- Retrieves data for the current user session stored in a session store.
getSessionData :: Global (SessionStore a) -> IO (Maybe a)
getSessionData sessionData = do
    sid <- getSessionId
    SStore sdata <- readGlobal sessionData
    return (findInSession sid sdata)
  where
    findInSession si ((id, _, storedData):rest) =
      if getId id == getId si
      then Just storedData
      else findInSession si rest
    findInSession _ [] = Nothing
      
--- Stores data related to the current user session in a session store.
putSessionData :: a -> Global (SessionStore a) -> IO ()
putSessionData newData sessionData = do
  sid <- getSessionId
  SStore sdata <- readGlobal sessionData
  currentTime <- getClockTime
  case findIndex (\ (id, _, _) -> id == sid) sdata of
    Just i ->
      writeGlobal sessionData
                  (SStore (replace (sid, clockTimeToInt currentTime, newData) i
                                   (cleanup currentTime sdata)))
    Nothing ->
      writeGlobal sessionData
                  (SStore ((sid, clockTimeToInt currentTime, newData)
                           : cleanup currentTime sdata))

--- Removes data related to the current user session from a session store.
removeSessionData :: Global (SessionStore a) -> IO ()
removeSessionData sessionData = do
  sid <- getSessionId
  SStore sdata <- readGlobal sessionData
  currentTime <- getClockTime
  writeGlobal sessionData
              (SStore (filter (\ (id, _, _) -> id /= sid)
                              (cleanup currentTime sdata)))

-- expects that clockTimeToInt converts time into ascending integers!
-- we should write our own conversion-function
cleanup :: ClockTime -> [(SessionId, Int, a)] -> [(SessionId, Int, a)]
cleanup currentTime sessionData =
  filter (\ (_, time, _) ->
            time > clockTimeToInt (addMinutes (0-sessionLifespan) currentTime))
         sessionData

--------------------------------------------------------------------------
