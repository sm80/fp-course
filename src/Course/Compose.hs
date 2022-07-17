{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.Contravariant

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a)) deriving (Show, Eq)

-- Implement a Functor instance for Compose
instance (Functor g, Functor h) =>
    Functor (Compose g h) where
  -- (<$>) =
  --   error "todo: Course.Compose (<$>)#instance (Compose f g)"
  -- (<$>) :: (a -> b) -> Compose g h a -> Compose g h b
  fab <$> Compose gha = Compose ((fab <$>) <$> gha)

instance (Applicative g, Applicative h) =>
  Applicative (Compose g h) where
-- Implement the pure function for an Applicative instance for Compose
  -- pure =
  --   error "todo: Course.Compose pure#instance (Compose f g)"
  -- pure :: a -> Compose g h a
  -- pure a = Compose (pure (pure a))
  pure = Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
  -- (<*>) =
  --   error "todo: Course.Compose (<*>)#instance (Compose f g)"
  -- (<*>) :: Compose g h (a -> b) -> Compose g h a -> Compose g h b
  -- hfab <*> ha = hb
  -- (<*>) hfab ha = hb
  -- lift2 <*> ghfab gha = ghb
  Compose ghfab <*> Compose gha = Compose (lift2 (<*>) ghfab gha)

instance (Monad g, Monad h) =>
  Monad (Compose g h) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "todo: Course.Compose (=<<)#instance (Compose f g)"
  -- (=<<) :: (a -> Compose g h b) -> Compose g h a -> Compose g h b
  -- this one is not possible because all the others are possible

-- Note that the inner h is Contravariant but the outer g is
-- Functor. We would not be able to write an instance if both were
-- Contravariant; why not?
instance (Functor g, Contravariant h) =>
  Contravariant (Compose g h) where
-- Implement the (>$<) function for a Contravariant instance for Compose
  -- (>$<) =
  --   error "todo: Course.Compose (>$<)#instance (Compose f g)"
  -- (>$<) :: (b -> a) -> Compose g h a -> Compose g h b
  -- (<$>) :: (x -> y) -> g x -> g y
  -- (<$>) :: (h a -> h b) -> g (h a) -> g (h b)
  -- (>$<) :: (y -> x) -> h x -> h y
  -- (>$<) :: (b -> a) -> h a -> h b
  -- fba >$< Compose gha =
  --   let fhahb = (fba >$<)
  --   in Compose $ fhahb <$> gha
  fba >$< Compose gha = Compose $ (fba >$<) <$> gha
