module Lang.Utils
  ( module Debug.Trace, -- re-export the content in Debug.Trace
    module Lang.Utils,
  )
where

import Debug.Trace

traceSelf :: Show a => String -> a -> a
traceSelf text v = trace (text ++ show v) v
