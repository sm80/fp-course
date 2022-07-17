{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.List
import Course.ExactlyOne
import Course.Optional
import Course.Compose

-- | All instances of the `Traversable` type-class must satisfy three laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse ::
    Applicative k =>
    (a -> k b)
    -> t a
    -> k (t b)

instance Traversable List where
  traverse ::
    Applicative k =>
    (a -> k b)
    -> List a
    -> k (List b)
  traverse f =
    foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse ::
    Applicative k =>
    (a -> k b)
    -> ExactlyOne a
    -> k (ExactlyOne b)
  -- traverse =
  --   error "todo: Course.Traversable traverse#instance ExactlyOne"
  -- traverse fakb (ExactlyOne a) =
  --   let kb = fakb a
  --   in ExactlyOne <$> kb
  traverse fakb (ExactlyOne a) = ExactlyOne <$> fakb a

instance Traversable Optional where
  traverse ::
    Applicative k =>
    (a -> k b)
    -> Optional a
    -> k (Optional b)
  -- traverse =
  --   error "todo: Course.Traversable traverse#instance Optional"
  traverse _ Empty = pure Empty
  traverse fakb (Full a) = Full <$> fakb a

-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA ::
  (Applicative k, Traversable t) =>
  t (k a)
  -> k (t a)
-- sequenceA =
--   error "todo: Course.Traversable#sequenceA"
-- traverse :: (a -> k b) -> t a -> k (t b)
-- traverse :: (k a -> k a) -> t (k a) -> k (t a)
sequenceA = traverse id

instance (Traversable g, Traversable h) =>
  Traversable (Compose g h) where
-- Implement the traverse function for a Traversable instance for Compose
  -- traverse =
  --   error "todo: Course.Traversable traverse#instance (Compose f g)"
  -- traverse :: (a -> k b) -> Compose g h a -> k (Compose g h b)
  -- traverse :: (x -> k y) -> g x -> k (g y)
  -- traverse :: (h a -> k h b) -> g h a -> k (g h b)
  -- traverse :: (a -> k b) -> h a -> k (h b)
  -- traverse fakb (Compose gha) =
  --   let fhakhb = traverse fakb
  --   in Compose <$> traverse fhakhb gha
  traverse fakb (Compose gha) = Compose <$> traverse (traverse fakb) gha

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a) (g a) deriving (Show, Eq)

instance (Functor g, Functor h) =>
  Functor (Product g h) where
-- Implement the (<$>) function for a Functor instance for Product
  -- (<$>) =
  --   error "todo: Course.Traversable (<$>)#instance (Product f g)"
  -- (<$>) :: (a -> b) -> Product g h a -> Product g h b
  fab <$> Product ga ha = Product (fab <$> ga) (fab <$> ha)

instance (Traversable g, Traversable h) =>
  Traversable (Product g h) where
-- Implement the traverse function for a Traversable instance for Product
  -- traverse =
  --   error "todo: Course.Traversable traverse#instance (Product f g)"
  -- traverse :: (a -> k b) -> Product g h a -> k (Product g h b)
  -- traverse :: (a -> k b) -> g a -> k (g b)
  -- traverse :: (a -> k b) -> h a -> k (h b)
  -- traverse fakb (Product ga ha) =
  --   let
  --     kgb = traverse fakb ga
  --     khb = traverse fakb ha
  --   in lift2 Product kgb khb
  traverse fakb (Product ga ha) = lift2 Product (traverse fakb ga) (traverse fakb ha)

-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a =
  InL (f a)
  | InR (g a) deriving (Show, Eq)

instance (Functor g, Functor h) =>
  Functor (Coproduct g h) where
-- Implement the (<$>) function for a Functor instance for Coproduct
  -- (<$>) =
  --   error "todo: Course.Traversable (<$>)#instance (Coproduct f g)"
  -- (<$>) :: (a -> b) -> Coproduct g h a -> Coproduct g h b
  fab <$> InL ga = InL (fab <$> ga)
  fab <$> InR ha = InR (fab <$> ha)

instance (Traversable g, Traversable h) =>
  Traversable (Coproduct g h) where
-- Implement the traverse function for a Traversable instance for Coproduct
  -- traverse =
  --   error "todo: Course.Traversable traverse#instance (Coproduct f g)"
  -- traverse :: (a -> k b) -> Coproduct g h a -> k (Coproduct g h b)
  -- traverse :: (a -> k b) -> g a -> k (g b)
  -- traverse :: (a -> k b) -> h a -> k (h b)
  traverse fakb (InL ga) = InL <$> traverse fakb ga
  traverse fakb (InR ha) = InR <$> traverse fakb ha
