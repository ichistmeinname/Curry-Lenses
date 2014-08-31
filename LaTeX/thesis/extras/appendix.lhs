\chapter{Combinatorial Library: Examples for Lenses}\label{a:combExamples}

\section*{Temperature examples}

\begin{spec}
data Temp = Temp Float

centigrade :: Lens Temp Float
centigrade = isoLens inn out
 where
  inn :: Float -> Temp
  inn celsius = Temp (cToF celsius)
  out :: Temp -> Float
  out (Temp temp) = fToC temp

cToF :: Float -> Float
cToF c = c *. 1.8 +. 32

fToC :: Float -> Float
fToC f = (f -. 32) *. (5/.9)
\end{spec}

\section*{Time examples}
\begin{spec}
data Time = Time Int Int

time :: Lens Time Int
time = isoLens innT (\(Time hour min) -> hour * 60 + min)

mins :: Lens Time Int
mins = isoLens innT (\(Time _ min) -> min)

innT :: Int -> Time
innT m = Time (m `quot` 60) (m `mod` 60)
\end{spec}

\section*{Person examples}
\begin{spec}
data Person = Person Name City
type First = String
type Address = String

person :: Lens Person (Name,City)
person = isoLens inn out
 where
  inn (n,c)        = Person n c
  out (Person n c) = (n,c)

nameOrAddress :: Lens Person String
nameOrAddress = nameLens ? addressLens

nameLens :: Lens Person First
nameLens = person <.> keepSnd

addressLens :: Lens Person Address
addressLens = person <.> keepFst
\end{spec}
