----------------------------------------------------------------------------
--- This library contains operations to support the management of
--- user authentication. It provides operations for password
--- generation, password hashing (based on hashing algorithms from Unix),
--- and storing logins in sessions.
---
--- @author Michael Hanus
----------------------------------------------------------------------------

module Authentication (
  getUserHash, randomPassword,
  getSessionLogin, loginToSession, logoutFromSession
 ) where

import Random
import IO
import IOExts
import Session
import Global

--------------------------------------------------------------------------
-- Operations for hashing.

--- Gets a hash string for a user name and password. The generated
--- hash string should be stored for the user instead of the password.
getUserHash :: String -> String -> IO String
getUserHash username userpassword = do
  let systemkey = "3spicey5" -- change this key for every spicey instance
  getHash (username++userpassword++systemkey)

--- Default hashing function.
--- @param toHash - string which should be hashed
--- @return the hashSum of this str
getHash :: String -> IO String
getHash = getHashWith "md5sum"
--getHash = getHashWith "sha1sum"

--- Hashes a string with an explicit Unix hash command.
--- @param hashcmd - Unix command for hasing
--- @param toHash - string which should be hashed
--- @return the hashed string
getHashWith :: String -> String -> IO String
getHashWith hashcmd toHash = do
  (sin, sout, _) <- execCmd hashcmd
  hPutStrLn sin toHash
  hClose sin
  result <- hGetLine sout
  return (head (words result))

--- Returns a random password (a hexadecimal string) of a particular length.
--- @param length - length of the desired password
--- @return the random password
randomPassword :: Int -> IO String
randomPassword n = do
  seed <- getRandomSeed
  ranString <- getHash (show (head (nextInt seed)))
  return (take n ranString)

--------------------------------------------------------------------------
-- Operations for storing logins in the current session.

--- Definition of the session state to store the login name (as a string).
sessionLogin :: Global (SessionStore String)
sessionLogin = global emptySessionStore (Persistent "sessionLogin")

--- Gets the login name of the current session
--- (or the Nothing if there is no login).
getSessionLogin :: IO (Maybe String)
getSessionLogin = getSessionData sessionLogin

--- Stores a login name in the current session.
--- The authentication has to be done before!
loginToSession :: String -> IO ()
loginToSession loginname = putSessionData loginname sessionLogin

--- removes the login name from the current session.
logoutFromSession :: IO ()
logoutFromSession = removeSessionData sessionLogin

--------------------------------------------------------------------------
