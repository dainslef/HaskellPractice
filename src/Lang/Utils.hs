module Lang.Utils
  ( module Debug.Trace
  , module Lang.Utils
  )
where

import           Debug.Trace

traceSelf :: Show a => String -> a -> a
traceSelf text v = trace (text ++ show v) v
