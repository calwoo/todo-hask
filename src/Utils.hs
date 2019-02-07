module Utils (subsetOf) where

subsetOf :: (Eq a, Foldable t) => [a] -> t a -> Bool
xs `subsetOf` ys = null $ filter (not . (`elem` ys)) xs