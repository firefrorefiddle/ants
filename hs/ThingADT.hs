module ThingADT where

import ThingClass

newtype Ant = Ant { getAntId :: AntId }
            deriving (Read, Show, Eq)

data AThing = Empty | Occupied Ant
            deriving (Read, Show, Eq)

instance Thing AThing where
  isEmpty     = (== Empty)
  emptyThing  = Empty
  fromAntId a = Occupied (Ant a)
  getAntId t  = case t of
    Empty -> Nothing
    Occupied (Ant a) -> Just a
