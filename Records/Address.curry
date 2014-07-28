module Address where

type Address = { person :: Person, street :: String }
type Person = { first, last :: String }

sandra = { first := "Sandra", last := "Dylus" }
myAddress = { person := sandra, street := "Olshausenstrasse" }