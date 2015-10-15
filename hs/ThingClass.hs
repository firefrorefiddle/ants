module ThingClass where

type AntId = Int

class Thing t where
  emptyThing :: t
  isEmpty    :: t -> Bool
  fromAntId  :: AntId -> t
  getAntId   :: t -> Maybe AntId
