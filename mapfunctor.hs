module Main (main) where

import Data.Map (Map, fromList, toList)

class Functorx f where
  xmap :: (a -> b) -> f a -> f b

instance (Ord k) => Functorx (Map k) where
  xmap f m = fromList $ map (\ (k, v) -> (k, f v)) $ toList m

main = print $ show $ xmap (*2) $ fromList [("a", 3), ("b", 6)]
