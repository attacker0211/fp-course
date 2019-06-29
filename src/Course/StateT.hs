{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
-- f (b, s)
instance Functor f => Functor (StateT s f) where
  (<$>) ::
    (a -> b)
    -> StateT s f a
    -> StateT s f b
  (<$>) g sa = StateT $ \s -> do
    (<$>) (\(a, s1) -> (g a, s1)) (runStateT sa s)

    -- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where
  pure ::
    a
    -> StateT s f a
  pure a = StateT (\s -> pure (a, s))
  (<*>) ::
   StateT s f (a -> b)
    -> StateT s f a
    -> StateT s f b
  (<*>) (StateT stab) (StateT sta) = StateT $ \s -> do
    (f, s1) <- stab s
    (a, s2) <- sta s1
    pure (f a, s2) 

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  (=<<) ::
    (a -> StateT s f b)
    -> StateT s f a
    -> StateT s f b
    -- (s -> f (a, s) -> StateT s f a)
  (=<<) g (StateT sta) = StateT $ \x -> do
    (a, s1) <- sta x
    runStateT (g a) s1

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a =
  StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne  ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' f = StateT $ (\x -> ExactlyOne (f x))

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' (StateT t) = runExactlyOne . t

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT (StateT sfa) = ((<$>) snd) . sfa
-- s -> (a, s)
-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' sa = snd . (runState' sa)

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT (StateT sfa) = ((<$>) fst) . sfa

-- | Run the `State'` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' sa = fst . (runState' sa)

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Applicative f =>
  StateT s f s
getT = StateT (\s -> pure (s, s))

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
-- s f a -> s -> f (a, s)
putT ::
  Applicative f =>
  s
  -> StateT s f ()
putT x = StateT (\_ -> pure((), x))

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' li =  listh . S.toList $ exec' (run li) S.empty
  where
    run (x:.xs) = getT >>= (\s -> putT (S.insert x s) *> run xs)
    run Nil = pure ()

-- distinctF ::
--   (Ord a, Num a) =>
--   List a
--   -> List a
-- distinctF list = listh . S.toList <$> foldLeft foldFn (pure S.empty) list
--   where
--     foldFn (Full s) el | el >= 100 = Empty
--                        | otherwise = Full (S.insert elem s)
--     foldFn s _ = s

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
-- distinctF :: (Ord a, Num a) => List a -> StateT (S.set a) Optional ()
distinctF list = listh . S.toList <$> execT (run list) S.empty
  where
    run (x:.xs) | x > 100 = StateT (const Empty)
                | otherwise = do
                    set <- getT
                    putT (S.insert x set) *> run xs
                    -- getT >>= (\s -> putT (S.insert x s) *> run xs)
    run Nil = pure ()

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  (<$>) g (OptionalT x) =  OptionalT ((<$>) g <$> x) -- f (OptionalT a)
  -- OptionalT (f (Optional b))
-- (a -> b) -> OptionalT f a -> OptionalT f b
-- (<$>):: functor f => (a -> b) -> f a -> f b
-- | Implement the `Applicative` instance for `OptionalT f` given a Monad f.
--
-- /Tip:/ Use `onFull` to help implement (<*>).
--
-- >>> runOptionalT $ OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- []
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
-- []
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty,Empty]
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Full 2,Full 3,Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Monad f => Applicative (OptionalT f) where
  pure = OptionalT . pure . pure
  --  OptionalT :: f (OptionalT a) -> OptionalT f a
  -- Optional a -> OptionalT f a
  -- pure :: a -> f a
  -- a -> OptionalT f a
  -- Optional a -> f (Optional a) -> OptionalT f a
  -- a -> Optional a -> f (Optional a) -> OptionalT f a
  (<*>) (OptionalT fab) (OptionalT a) = OptionalT $ fab >>= \x -> do run x a 
    where
      run Empty p = pure Empty
      run (Full k) p = ((k <$>) <$> p)

  -- (<*>) (OptionalT fab) (OptionalT fa) = OptionalT $ do
  -- fnOp <- fab
  -- optional (\f -> (f <$>) <$> a) (pure Empty) fnOp
  
  -- Type: OptionalT f (a -> b) -> OptionalT f a -> OptionalT f b
  -- OptionalT ( f (OptionalT b) )
  -- OptionalT :: f (OptionalT a) -> OptionalT f a
  -- runOptionalT :: OptionalT f a -> f (Optional a)

-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  (=<<) f (OptionalT s) = OptionalT $ s >>= \x -> do run x
    where
      run (Empty) = pure Empty
      run (Full k) = runOptionalT (f k)
  -- (=<<) f (OptionalT fa) = OptionalT $ do
  --   a <- fa
  --   Optional (runOptionalT . f) (pure Empty) a
      --(a -> OptionalT f b) -> OptionalT f a -> OptionalT f b

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) f (Logger l a) = Logger l (f a)
  -- (a -> b) -> (Logger l a) -> (Logger l b)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure = Logger Nil 
  -- a -> Logger l a
  (<*>) (Logger l1 f) (Logger l2 a) = Logger (l1 ++ l2) (f a)
  -- Logger l (a -> b) -> Logger l a -> Logger l b

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  (=<<) f (Logger l a) = run (f a) (Logger l a) where 
    run (Logger l1 x) (Logger l2 _) = Logger (l2 ++ l1) x
  --- (a -> Logger l b) -> Logger l a -> Logger l b

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 l = Logger (listh [l])

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG li =  (<$>) (listh . S.toList) <$> runOptionalT (execT (run li) S.empty)
  where
    insertElem x = do
      set <- getT
      putT (S.insert x set)
    run (x:.xs) | x > 100 = StateT (\_ -> OptionalT (log1 ("aborting > 100: " ++ show' x) Empty))
                | otherwise = do
                    if x `mod` 2 == 0 then StateT (\s -> OptionalT (log1 ("even number: " ++ show' x) (pure ((), s))))
                    else pure ()
                    insertElem x
                    run xs
    run _ = pure ()

onFull ::
  Applicative f =>
  (t -> f (Optional a))
  -> Optional t
  -> f (Optional a)
onFull g o =
  case o of
    Empty ->
      pure Empty
    Full a ->
      g a
