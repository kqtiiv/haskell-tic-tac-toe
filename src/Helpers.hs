module Helpers where

-- Some useful functions from, or based on, the unassessed problem sheets...

-- | Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x) | p x = m
filterMaybe p _                = Nothing

-- | Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace _ p []        = []
replace 0 p (c : cs)  = p : cs
replace n p (c : cs)  = c : replace (n - 1) p cs

