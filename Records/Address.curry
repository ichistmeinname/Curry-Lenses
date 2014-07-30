module Address where

-- import Lens

-- data P = Person

-- data Address = Address42 Person String
-- type Address = { person :: Person, street :: String }
type Person = { first, last :: String }

sandra = { first := "Sandra", last := "Dylus" }
-- myAddress = { person := sandra, street := "Olshausenstrasse" }

-- street :: Lens Address String
-- street = (\(Address42 _ s) -> s,\(Address42 p _) n -> Address42 p n)