module ThingInt where

import ThingClass

newtype AThing = Thing { getId :: Int }
              deriving (Read, Show, Eq)

instance Thing AThing where
  isEmpty     = (<0) . getId
  emptyThing  = Thing (-1)
  fromAntId a = Thing a
  getAntId t  = if getId t < 0
                then Nothing
                else Just (getId t)
