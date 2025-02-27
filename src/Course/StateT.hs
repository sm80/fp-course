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

-- | A `StateT` is a function from a state value `s` to a functor k of (a produced value `a`, and a resulting state `s`).
newtype StateT s k a =
  StateT {
    runStateT ::
      s
      -> k (a, s)
  }

-- | Implement the `Functor` instance for @StateT s k@ given a @Functor k@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor k => Functor (StateT s k) where
  (<$>) ::
    (a -> b)
    -> StateT s k a
    -> StateT s k b
  -- (<$>) =
  --   error "todo: Course.StateT (<$>)#instance (StateT s k)"
  -- fab <$> ska = StateT {
  --   runStateT = \s ->
  --     let
  --       kas = runStateT ska s
  --       f = \(a, s) -> (fab a, s)
  --     in f <$> kas
  -- }
  fab <$> ska = StateT {
    runStateT = \s ->
      let f (a, s) = (fab a, s)
      in f <$> runStateT ska s
  }

-- | Implement the `Applicative` instance for @StateT s k@ given a @Monad k@.
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
-- >>> runStateT (StateT (\s -> Full ((+2), s ++ (1:.Nil))) <*> (StateT (\s -> Full (2, s ++ (2:.Nil))))) (0:.Nil)
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s ++ (1:.Nil)) :. ((+3), s ++ (1:.Nil)) :. Nil) <*> (StateT (\s -> (2, s ++ (2:.Nil)) :. Nil))) (0:.Nil)
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad k => Applicative (StateT s k) where
  pure ::
    a
    -> StateT s k a
  -- pure =
  --   error "todo: Course.StateT pure#instance (StateT s k)"
  pure a = StateT {
    runStateT = \s -> pure (a, s)
  }
  (<*>) ::
    StateT s k (a -> b)
    -> StateT s k a
    -> StateT s k b
  -- (<*>) =
  --   error "todo: Course.StateT (<*>)#instance (StateT s k)"
  skfab <*> ska = StateT {
    runStateT = \s -> do
      (fab, s') <- runStateT skfab s
      (a, s'') <- runStateT ska s'
      pure (fab a, s'')
  }

-- | Implement the `Monad` instance for @StateT s k@ given a @Monad k@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad k => Monad (StateT s k) where
  (=<<) ::
    (a -> StateT s k b)
    -> StateT s k a
    -> StateT s k b
  -- (=<<) =
  --   error "todo: Course.StateT (=<<)#instance (StateT s k)"
  faskb =<< ska = StateT {
    runStateT = \s -> do
      (a, s') <- runStateT ska s
      let skb = faskb a
      runStateT skb s'
  }

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a =
  StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
-- state' =
--   error "todo: Course.StateT#state'"
-- state' fsas = StateT {
--   runStateT = \s -> ExactlyOne (fsas s)
-- }
state' fsas = StateT {
  runStateT = ExactlyOne . fsas
}

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
-- runState' =
--   error "todo: Course.StateT#runState'"
runState' sa s = runExactlyOne $ runStateT sa s

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
--
-- >>> execT (StateT $ \s -> Full ((), s + 1)) 2
-- Full 3
execT ::
  Functor k =>
  StateT s k a
  -> s
  -> k s
-- execT =
--   error "todo: Course.StateT#execT"
-- execT ska s =
--   let
--     f (_, s') = s'
--     kas = runStateT ska s
--   in f <$> kas
execT ska s = snd <$> runStateT ska s

-- | Run the `State'` seeded with `s` and retrieve the resulting state.
--
-- >>> exec' (state' $ \s -> ((), s + 1)) 2
-- 3
exec' ::
  State' s a
  -> s
  -> s
-- exec' =
--   error "todo: Course.StateT#exec'"
exec' sa s = snd $ runState' sa s

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
--
-- >>> evalT (StateT $ \s -> Full (even s, s + 1)) 2
-- Full True
evalT ::
  Functor k =>
  StateT s k a
  -> s
  -> k a
-- evalT =
--   error "todo: Course.StateT#evalT"
-- evalT ska s =
--   let
--     f (a, _) = a
--     kas = runStateT ska s
--   in f <$> kas
evalT ska s = fst <$> runStateT ska s

-- | Run the `State'` seeded with `s` and retrieve the resulting value.
--
-- >>> eval' (state' $ \s -> (even s, s + 1)) 5
-- False
eval' ::
  State' s a
  -> s
  -> a
-- eval' =
--   error "todo: Course.StateT#eval'"
eval' sa s = fst $ runState' sa s

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Applicative k =>
  StateT s k s
-- getT =
--   error "todo: Course.StateT#getT"
getT = StateT {
  runStateT = \s -> pure (s, s)
}

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Applicative k =>
  s
  -> StateT s k ()
-- putT =
--   error "todo: Course.StateT#putT"
-- putT s = StateT {
--   runStateT = \_ -> pure ((), s)
-- }
putT s = StateT {
  runStateT = const $ pure ((), s)
}

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  Ord a =>
  List a
  -> List a
-- distinct' =
--   error "todo: Course.StateT#distinct'"
distinct' la = eval' (filtering f la) S.empty
  where
    f a = do
      s <- getT
      if S.member a s
      then pure False
      else do
        putT (S.insert a s)
        pure True

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
-- distinctF =
--   error "todo: Course.StateT#distinctF"
distinctF la = evalT (filtering f la) S.empty
  where
    f a = do
      if a > 100
      then StateT (const Empty)
      else do
        s <- getT
        if S.member a s
        then pure False
        else do
          putT $ S.insert a s
          pure True

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT k a =
  OptionalT {
    runOptionalT ::
      k (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT k` given a Functor k.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor k => Functor (OptionalT k) where
  (<$>) ::
    (a -> b)
    -> OptionalT k a
    -> OptionalT k b
  -- (<$>) =
  --   error "todo: Course.StateT (<$>)#instance (OptionalT k)"
  -- fab <$> oka = OptionalT {
  --   runOptionalT =
  --     let koa = runOptionalT oka
  --     in (fab <$>) <$> koa
  -- }
  fab <$> oka = OptionalT {
    runOptionalT = (fab <$>) <$> runOptionalT oka
  }

-- | Implement the `Applicative` instance for `OptionalT k` given a Monad k.
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
instance Monad k => Applicative (OptionalT k) where
  pure ::
    a
    -> OptionalT k a
  -- pure =
  --   error "todo: Course.StateT pure#instance (OptionalT k)"
  pure a = OptionalT {
    runOptionalT = pure (pure a)
  }

  (<*>) ::
    OptionalT k (a -> b)
    -> OptionalT k a
    -> OptionalT k b
  -- (<*>) =
  --   error "todo: Course.StateT (<*>)#instance (OptionalT k)"
  -- onFull :: (t -> k (Optional a)) -> Optional t -> k (Optional a)
  -- onFull :: ((a -> b) -> k (Optional b)) -> Optional (a -> b) -> k (Optional b)
  okfab <*> oka = OptionalT {
    runOptionalT = do
      ofab <- runOptionalT okfab
      let ffabkob = \fab -> do
          oa <- runOptionalT oka
          pure $ fab <$> oa
      onFull ffabkob ofab
  }

-- | Implement the `Monad` instance for `OptionalT k` given a Monad k.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad k => Monad (OptionalT k) where
  (=<<) ::
    (a -> OptionalT k b)
    -> OptionalT k a
    -> OptionalT k b
  -- (=<<) =
  --   error "todo: Course.StateT (=<<)#instance (OptionalT k)"
  -- onFull :: (t -> k (Optional a)) -> Optional t -> k (Optional a)
  -- onFull :: (a -> k (Optional b)) -> Optional a -> k (Optional b)
  -- faokb =<< oka = OptionalT {
  --   runOptionalT = do
  --     oa <- runOptionalT oka
  --     let fakob = runOptionalT . faokb
  --     onFull fakob oa
  -- }
  faokb =<< oka = OptionalT {
    runOptionalT = do
      oa <- runOptionalT oka
      onFull (runOptionalT . faokb) oa
  }

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) ::
    (a -> b)
    -> Logger l a
    -> Logger l b
  -- (<$>) =
  --   error "todo: Course.StateT (<$>)#instance (Logger l)"
  fab <$> (Logger ll a) = Logger ll (fab a)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure ::
    a
    -> Logger l a
  -- pure =
  --   error "todo: Course.StateT pure#instance (Logger l)"
  -- pure a = Logger Nil a
  pure = Logger Nil

  (<*>) ::
    Logger l (a -> b)
    -> Logger l a
    -> Logger l b
  -- (<*>) =
  --   error "todo: Course.StateT (<*>)#instance (Logger l)"
  (Logger ll fab) <*> (Logger ll' a) = Logger (ll ++ ll') (fab a)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  (=<<) ::
    (a -> Logger l b)
    -> Logger l a
    -> Logger l b
  -- (=<<) =
  --   error "todo: Course.StateT (=<<)#instance (Logger l)"
  falgb =<< (Logger ll a) =
    let (Logger ll' b) = falgb a
    in Logger (ll ++ ll') b

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
-- log1 =
--   error "todo: Course.StateT#log1"
-- log1 l a = Logger (l :. Nil) a
log1 = Logger . (:. Nil)

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
-- f :: a -> StateT (Set a) (OptionalT (Logger Chars)) Bool
-- log1 "even number" :: a -> Logger Chars a
-- pure True :: StateT s k Bool
-- distinctG la = runOptionalT $ evalT (filtering f la) S.empty
--   where
--     f a = do
--       s <- getT
--       let isLarge = a > 100
--           isNew = not $ S.member a s
--           isEven = a `mod` 2 == 0
--           logMsg | isLarge = "aborting > 100: " ++ show' a
--                  | isEven = "even number: " ++ show' a
--                  | otherwise = Nil
--           logFn = if isLarge || isEven then log1 logMsg else pure
--           result s = if isLarge then Empty else Full (isNew, s)
--       putT $ S.insert a s
--       StateT {
--         runStateT = \s -> OptionalT {
--           runOptionalT = logFn (result s)
--         }
--       }
distinctG la = runOptionalT $ evalT (filtering f la) S.empty
  where
    f a = do
      s <- getT
      let isLarge = a > 100
          isNew = not $ S.member a s
          isEven = a `mod` 2 == 0
          logMsg | isLarge = "aborting > 100: " ++ show' a
                 | isEven = "even number: " ++ show' a
                 | otherwise = Nil
          logFn = if isLarge || isEven then log1 logMsg else pure
          result s = if isLarge then Empty else Full (isNew, s)
      putT $ S.insert a s
      StateT $ OptionalT . logFn . result

onFull ::
  Applicative k =>
  (t -> k (Optional a))
  -> Optional t
  -> k (Optional a)
onFull g o =
  case o of
    Empty ->
      pure Empty
    Full a ->
      g a
