--implement these functions
maybeJoin :: Maybe (Maybe a) -> Maybe a
maybeJoin (Just (Just x)) = Just x
maybeJoin (Just Nothing)  = Nothing
maybeJoin Nothing         = Nothing

listJoin :: [[a]] -> [a]
listJoin = concat

eitherJoin :: Either r (Either r a) -> Either r a
eitherJoin (Right (Right x)) = Right x
eitherJoin (Right (Left x))  = Left x
eitherJoin (Left x)          = Left x


arrowJoin :: (r -> r -> a) -> r -> a
arrowJoin f x = f x x

pairJoin :: Monoid r => (r, (r, a)) -> (r, a)
pairJoin (x, (y, a)) = (x <> y, a)
-- what do we need to know about r?
--      r is a Monoid, which means:
--          r has an identity element (mempty)
--          r has an associative binary operation (<>)
--          It must satisfy the monoid laws  

-- Prove either --
-- prove that the three monad laws hold for the Either monad
-- hint: prove by cases (Left vs Right)
-- instance Monad (Either e) where
--     	Right m >>= k = k m
--     	Left e  >>= _ = Left e

-- 1. left identity return a >>= k = k a 
-- return a >>= k 
--      = (Right a) >>=  k
--      = k a
-- Holds

-- 2. right identity m >>= return = m
-- Right a >>= return
--      = return a 
--      = Right a
-- Left e >>= return
--      = Left e
-- Holds

-- 3. associativity m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- (Right a >>= k) >>= h
--      = Right a >>= (\x -> k x >>= h)
--      = (\x -> k x >>= h) a
--      = k a >>= h
--      = (return a >>= k) >>= h
--      = (Right a >>= k) >>= h
-- (Left e >>= k) >>= h
--      = Left e >>= (\x -> k x >>= h)
--      = Left e
--      = Left e >>= h
--      = (Left e >>= k) >>= h
-- Holds

-- satisfies the monad laws.

-- Prove list --
-- prove that the three monad laws hold for the list monad
-- instance Monad []  where
-- 	xs >>= f = [y | x <- xs, y <- f x]

-- 1. left identity return a >>= k = k a 
-- return a >>= k
--      = [a] >>= k
--      = [y | x <- [a], y <- k x]
--      = [y | y <- k a]
--      = k a
-- Holds

-- 2. right identity m >>= return = m
-- xs >>= return
--      = [y | x <- xs, y <- return x]
--      = [y | x <- xs, y <- [x]]
--      = [x | x <- xs]
--      = xs
-- Holds

-- 3. associativity m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- LHS: m >>= (\x -> k x >>= h)
-- m >>= (\x -> k x >>= h)
--      = m >>= (\x -> [z | y <- k x, z <- h y])
--      = [z | x <- m, y <- k x, z <- h y]
-- RHS: (m >>= k) >>= h
-- m >>= k = [y | x <- m, y <- k x]
--      = [m >>= k] >>= h
--      = [z | y <- [y | x <- m, y <- k x], z <- h y]
--      = [z | x <- m, y <- k x, z <- h y]
-- Holds

-- satisfies the monad laws.

-- Prove arrow --
-- prove that the three monad laws hold for the arrow monad
-- hint: unlock each side of the equality with a value of type r, and check that both sides are indeed equal
-- instance Monad ((->) r) where
-- 	f >>= k = \ r -> k (f r) r

-- 1. left identity return a >>= k = k a 
-- return a >>= k
--      = (\_ -> a) >>= k
--      = \r -> k ((\_ -> a) r) r
--      = \r -> k a r
--      = k a
-- Holds

-- 2. right identity m >>= return = m
-- m >>= return
--      = \r -> return (m r) r
--      = \r -> (\_ -> m r) r
--      = \r -> m r
--      = m
-- Holds

-- 3. associativity m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- LHS: m >>= (\x -> k x >>= h)
-- m >>= (\x -> k x >>= h)
--      = m >>= (\x -> \r -> h (k x r) r)
--      = \r -> (\x -> \r' -> h (k x r') r') (m r) r
--      = \r -> h (k (m r) r) r
-- RHS: (m >>= k) >>= h
-- ((m >>= k) >>= h)
--      = (\r -> k (m r) r) >>= h
--      = \r -> h ((\r' -> k (m r') r') r) r
--      = \r -> h (k (m r) r) r
-- Holds

-- satisfies the monad laws.


-- Prove Pair (Extra) --
-- 1. left identity return a >>= k = k a 
-- return a >>= k
--      = (mempty, a) >>= k
--      = let (v, b) = k a in (mempty, b)
--      = (mempty, b) 
--      = k a
-- Holds

-- 2. right identity m >>= return = m
-- (u, a) >>= return
--      = let (v, b) = return a in (u, b)
--      = let (mempty, b) = (mempty, a) in (u, b)
--      = (u, a)
-- Holds

-- 3. associativity m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- LHS: m >>= (\x -> k x >>= h)
-- m >>= (\x -> k x >>= h)
--     = (u, a) >>= (\x -> k x >>= h)
--     = let (v, b) = k a >>= h in (u, b)
-- RHS: (m >>= k) >>= h
--(m >>= k) >>= h
--     = let (v', c) = k a in (u, c) >>= h
--     = let (w, d) = h c in (u, d)
-- Holds

-- satisfies the monad laws.