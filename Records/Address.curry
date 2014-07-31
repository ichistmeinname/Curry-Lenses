module Address where

type Address = { person :: Person, street :: String }
type Person = { first, last :: String }

bobDylan = { first := "Bob", last := "Dylan" }
aPerson = { first := "Sandra", last := "Dylus" }

anAddress = { person := aPerson, street := "Olshausenstrasse" }

data Test = Test123 Int String

test :: Test
test = Test123 123 "Test"