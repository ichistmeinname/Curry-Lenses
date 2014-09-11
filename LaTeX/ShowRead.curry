module ShowRead where

import List ( intersperse )
import Maybe ( isJust )

import Lens (mapLens, get, put, Lens, (<.>) )
import PP ( (<>), (<<<), (>>>), whitespace, digit, PPrinter, pParse, pPrint )


----- definition of data structures

type Match = { team1, team2 :: Team, result :: Result }

match :: Team -> Team -> Result -> Match
match t1 t2 r = { team1 := t1, team2 := t2, result := r }

data Team = Germany | Ghana | Portugal | USA
data Score = Int :-: Int
type Result = Maybe Score

type Group = [Match]

type FilePath = String


----- example instances

match1,match2,match3,match4,match5,match6 :: Match
match1 = { team1 := Germany
         , team2 := Portugal
         , result := Nothing }
match2 = { team1 := Ghana
         , team2 := USA
         , result := Nothing }
match3 = { team1 := Germany
         , team2 := Ghana
         , result := Nothing }
match4 = { team1 := USA
         , team2 := Portugal
         , result := Nothing }
match5 = { team1 := Portugal
         , team2 := Ghana
         , result := Nothing }
match6 = { team1 := USA
         , team2 := Germany
         , result := Nothing }

groupG :: Group
groupG = [match1,match2,match3,match4,match5,match6]


----- projection lenses

matchResult :: Lens Match Result
matchResult match res = { result := res | match }

matchTeams :: Lens Match (Team,Team)
matchTeams match (team1, team2) =
  { team1 := team1, team2 := team2 | match }

matchTeam1 :: Lens Match Team
matchTeam1 = matchTeams <.> fstPair

matchTeam2 :: Lens Match Team
matchTeam2 = matchTeams <.> sndPair

-- helper lenses

fstPair :: Lens (a,b) a
fstPair (_,y) v = (v,y)

sndPair :: Lens (a,b) b
sndPair (x,_) v = (x,v)

replaceAt :: Int -> Lens [a] a
replaceAt index (x:xs) v
  | index == 0 = v : xs
  | otherwise  = x : replaceAt (index-1) xs v


----- IO actions

updateIO :: FilePath -> FilePath -> IO Group
updateIO filePath1 filePath2 = do
  bet1 <- readGroup filePath1
  bet2 <- readGroup filePath2
  return $ updateGroup bet1 bet2

writeGroup :: FilePath -> Group -> IO ()
writeGroup filePath = writeFile filePath . showGroup

readGroup :: FilePath -> IO Group
readGroup filePath = readFile filePath >>= return . bet
 where
  bet = map (pParse ppMatch) . lines

-- PPrinter :: String -> (a,String) -> String
pParse :: PPrinter a -> String -> a
pParse pPrinter str = case get pPrinter str of
                           (expr,"") -> expr
                           _         -> failed

----- data manipulation
updateGroup :: Group -> Group -> Group
updateGroup bet1 bet2 =
  foldr replace bet1 (zip [0..] bet2)
 where
  replace (index,value) list
    | isJust (value :> result) = replaceAt index list value
    | otherwise                = list


----- show and read

showGroup :: Group -> String
showGroup = concat . intersperse "\n" . map showMatch

showMatch :: Match -> String
showMatch match = pPrint ppMatch match

readMatch :: String -> Match
readMatch = pParse ppMatch


----- pretty-printer- parser

--- combinator for printer-parser

type PPrinter a = String -> (a,String) -> String

-- PPrinter (a,b) = String -> ((a,b),String) -> String
-- (<>) :: PPrinter a -> PPrinter b -> PPrinter (a,b)
-- (pA <> pB) str ((expr1,expr2),str') = pA str (expr1, newString)
--  where
--   newString = pB str (expr2,str')

-- wie (<*) bei Parsern
-- (<<<) :: PPrinter a -> PPrinter () -> PPrinter a
-- (pA <<< pB) str (exprA,str') = (pA <> pB) str ((exprA,()),str')

ppWhitespace :: PPrinter ()
ppWhitespace _ ((),str') = ' ' : str'

-- (>>>) :: PPrinter () -> PPrinter b -> PPrinter b


--- match specific printer-parser

ppMatch :: PPrinter Match
ppMatch str' (match,str) =
  (((ppTeam <<< ppWhitespace)
   <> ppResult)
   <> (ppWhitespace >>> ppTeam)) str'
                                 ((( match :> team1, match :> result)
                                   , match :> team2)
                                 , str)

ppTeam :: PPrinter Team
ppTeam _ (team,str) =
  foldr (:) str (case team of
                      Germany  -> "G"
                      Ghana    -> "G"
                      Portugal -> "Por"
                      USA      -> "USA")
ppTeam str (Germany,str') = "G" ++ str ++ str'

-- ppTeam "G" v == "G"

ppResult :: PPrinter Result
ppResult str' (Nothing,str) = ppCustom str' ("- : -",str)
ppResult str' (Just s,str)  = ppScore str' (s,str)

ppScore :: PPrinter Score
ppScore str' ((s1 :-: s2),str) =
  (digit <> ppCustom <> digit) str' (((s1," : "), s2), str)


ppCustom :: PPrinter String
ppCustom _ (customStr,str') = foldr (:) str' customStr
-- ppCustom _ = (uncurry $ flip $ foldr (:))