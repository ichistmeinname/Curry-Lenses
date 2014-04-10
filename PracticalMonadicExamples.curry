import Monadic
import Float ((*.), (/.), (+.), (-.))

type Temp = { fahrenheit :: Float }

centigrade :: Lens Temp Float
centigrade = isoLens inn out
 where
  inn :: Float -> Temp
  inn celsius = { fahrenheit := cToF celsius }
  out :: Temp -> Float
  out temp = fToC (temp :> fahrenheit)

cToF :: Float -> Float
cToF c = c *. 1.8 +. 32
fToC :: Float -> Float
fToC f = (f -. 32) *. (5/.9)

centigradeGet :: Float -> Float
centigradeGet temp = get' centigrade { fahrenheit := temp }

centigradePut :: Float -> Float -> Temp
centigradePut temp celsius = put' centigrade (Just { fahrenheit := temp }) celsius
centigradePut _    celsius = put' centigrade Nothing celsius

type Time = { hour :: Int, min :: Int }

inTime :: Lens Time Int
inTime = isoLens innT (\t -> t :> hour * 60 + t :> min)

mins :: Lens Time Int
mins = isoLens innT (\t -> t :> min)

innT :: Int -> Time
innT m = { hour := m `quot` 60, min := m `mod` 60 }

minsGet :: Int -> Int -> Int
minsGet h m = get' mins { hour := h, min := m }
minsGet h m = get' inTime { hour := h, min := m}

minsPut :: Int -> Int -> Int -> Time
minsPut h m newM = put' mins (Just { hour := h, min := m }) newM
minsPut h m newM = put' inTime (Just { hour := h, min := m }) newM
minsPut h m newM = put' mins Nothing newM
minsPut h m newM = put' inTime Nothing newM