{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ListZipper where

import Course.Core
import Course.List
import Course.Optional
import Course.Functor
import Course.Applicative
import Course.Extend
import Course.Comonad
import Course.Traversable
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Maybe(maybe)
-- >>> import Course.Core
-- >>> import qualified Prelude as P
-- >>> let optional e _ Empty = e; optional _ f (Full a) = f a
-- >>> instance Arbitrary a => Arbitrary (Optional a) where arbitrary = P.fmap (maybe Empty Full) arbitrary
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap (P.foldr (:.) Nil :: ([a] -> List a)) arbitrary
-- >>> instance Arbitrary a => Arbitrary (ListZipper a) where arbitrary = do l <- arbitrary; x <- arbitrary; r <- arbitrary; P.return (ListZipper l x r)

-- A `ListZipper` is a focussed position, with a list of values to the left and to the right.
--
-- For example, taking the list [0,1,2,3,4,5,6], the moving focus to the third position, the zipper looks like:
-- ListZipper [2,1,0] 3 [4,5,6]
--
-- Supposing then we move left on this zipper:
-- ListZipper [1,0] 2 [3,4,5,6]
--
-- then suppose we add 17 to the focus of this zipper:
-- ListZipper [1,0] 19 [3,4,5,6]
data ListZipper a =
  ListZipper (List a) a (List a)
  deriving Eq

lefts ::
  ListZipper a
  -> List a
lefts (ListZipper l _ _) =
  l

rights ::
  ListZipper a
  -> List a
rights (ListZipper _ _ r) =
  r

-- A `MaybeListZipper` is a data structure that allows us to "fail" zipper operations.
-- e.g. Moving left when there are no values to the left.
newtype MaybeListZipper a =
  MLZ (Optional (ListZipper a))
  deriving Eq

isZ ::
  ListZipper a
  -> MaybeListZipper a
isZ = MLZ . Full

isNotZ ::
  MaybeListZipper a
isNotZ = MLZ Empty

-- | Implement the `Functor` instance for `ListZipper`.
--
-- >>> (+1) <$> (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2] >5< [6,7,8]
instance Functor ListZipper where
  -- (<$>) =
  --   error "todo: Course.ListZipper (<$>)#instance ListZipper"
  -- (<$>) :: (a -> b) -> ListZipper a -> ListZipper b
  fab <$> ListZipper la a la' = ListZipper (map fab la) (fab a) (map fab la')

-- | Implement the `Functor` instance for `MaybeListZipper`.
--
-- >>> (+1) <$> (MLZ (Full (zipper [3,2,1] 4 [5,6,7])))
-- [4,3,2] >5< [6,7,8]
instance Functor MaybeListZipper where
  -- (<$>) =
  --   error "todo: Course.ListZipper (<$>)#instance MaybeListZipper"
  -- (<$>) :: (a -> b) -> MaybeListZipper a -> MaybeListZipper b
  fab <$> MLZ oza = MLZ $ (fab <$>) <$> oza

-- | Convert the given zipper back to a list.
--
-- >>> toList <$> toOptional (fromList Nil)
-- Empty
--
-- >>> toList (ListZipper Nil 1 (2:.3:.4:.Nil))
-- [1,2,3,4]
--
-- >>> toList (ListZipper (3:.2:.1:.Nil) 4 (5:.6:.7:.Nil))
-- [1,2,3,4,5,6,7]
toList ::
  ListZipper a
  -> List a
-- toList =
--   error "todo: Course.ListZipper#toList"
toList (ListZipper la a la') = reverse la ++ (a :. la')

-- | Convert the given (maybe) zipper back to a list.
toListZ ::
  MaybeListZipper a
  -> List a
toListZ (MLZ Empty) =
  Nil
toListZ (MLZ (Full z)) =
  toList z

-- | Create a `MaybeListZipper` positioning the focus at the head.
--
-- ->>> fromList (1 :. 2 :. 3 :. Nil)
-- [] >1< [2,3]
--
-- >>> fromList Nil
-- ><
--
-- prop> \xs -> xs == toListZ (fromList xs)
fromList ::
  List a
  -> MaybeListZipper a
-- fromList =
--   error "todo: Course.ListZipper#fromList"
fromList Nil = MLZ Empty
fromList (a :. la) = MLZ $ Full $ ListZipper Nil a la

-- | Retrieve the `ListZipper` from the `MaybeListZipper` if there is one.
--
-- prop> \xs -> isEmpty xs == (toOptional (fromList xs) == Empty)
--
-- prop> \z -> toOptional (fromOptional z) == z
toOptional ::
  MaybeListZipper a
  -> Optional (ListZipper a)
-- toOptional =
--   error "todo: Course.ListZipper#toOptional"
toOptional (MLZ oza) = oza

zipper ::
  [a]
  -> a
  -> [a]
  -> ListZipper a
zipper l x r =
  ListZipper (listh l) x (listh r)

fromOptional ::
  Optional (ListZipper a)
  -> MaybeListZipper a
fromOptional =
  MLZ

asZipper ::
  (ListZipper a -> ListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
asZipper f =
  asMaybeZipper (isZ . f)

(>$>)::
  (ListZipper a -> ListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
(>$>) =
  asZipper

asMaybeZipper ::
  (ListZipper a -> MaybeListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
asMaybeZipper _ (MLZ Empty) =
  isNotZ
asMaybeZipper f (MLZ (Full z)) =
  f z

(-<<) ::
  (ListZipper a -> MaybeListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
(-<<) =
  asMaybeZipper

-- | Update the focus of the zipper with the given function on the current focus.
--
-- >>> withFocus (+1) (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> withFocus (+1) (zipper [1,0] 2 [3,4])
-- [1,0] >3< [3,4]
withFocus ::
  (a -> a)
  -> ListZipper a
  -> ListZipper a
-- withFocus =
--   error "todo: Course.ListZipper#withFocus"
withFocus faa (ListZipper la a la') = ListZipper la (faa a) la'

-- | Set the focus of the zipper to the given value.
-- /Tip:/ Use `withFocus`.
--
-- >>> setFocus 1 (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> setFocus 1 (zipper [1,0] 2 [3,4])
-- [1,0] >1< [3,4]
setFocus ::
  a
  -> ListZipper a
  -> ListZipper a
-- setFocus =
--   error "todo: Course.ListZipper#setFocus"
-- setFocus a = withFocus (const a)
setFocus = withFocus . const

-- A flipped infix alias for `setFocus`. This allows:
--
-- z .= "abc" -- sets the focus on the zipper z to the value "abc".
(.=) ::
  ListZipper a
  -> a
  -> ListZipper a
(.=) =
  flip setFocus

-- | Returns whether there are values to the left of focus.
--
-- >>> hasLeft (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasLeft (zipper [] 0 [1,2])
-- False
hasLeft ::
  ListZipper a
  -> Bool
-- hasLeft =
--   error "todo: Course.ListZipper#hasLeft"
-- hasLeft za = not $ isEmpty $ lefts za
hasLeft = not . isEmpty . lefts

-- | Returns whether there are values to the right of focus.
--
-- >>> hasRight (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasRight (zipper [1,0] 2 [])
-- False
hasRight ::
  ListZipper a
  -> Bool
-- hasRight =
--   error "todo: Course.ListZipper#hasRight"
-- hasRight za = not $ isEmpty $ rights za
hasRight = not . isEmpty . rights

-- | Seek to the left for a location matching a predicate, excluding the
-- focus.
--
-- /Tip:/ Use `break`
--
-- prop> \xs p -> findLeft (const p) -<< fromList xs == isNotZ
--
-- >>> findLeft (== 1) (zipper [2, 1] 3 [4, 5])
-- [] >1< [2,3,4,5]
--
-- >>> findLeft (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findLeft (== 1) (zipper [2, 1] 1 [4, 5])
-- [] >1< [2,1,4,5]
--
-- >>> findLeft (== 1) (zipper [1, 2, 1] 3 [4, 5])
-- [2,1] >1< [3,4,5]
--
-- >>> findLeft (== 1) (zipper [3, 4, 1, 5] 9 [2, 7])
-- [5] >1< [4,3,9,2,7]
findLeft ::
  (a -> Bool)
  -> ListZipper a
  -> MaybeListZipper a
-- findLeft =
--   error "todo: Course.ListZipper#findLeft"
findLeft fab (ListZipper la a' la') =
  case break fab la of
    (_, Nil) -> isNotZ
    (lla, a :. rla) -> isZ $ ListZipper rla a (reverse lla ++ a' :. la')


-- | Seek to the right for a location matching a predicate, excluding the
-- focus.
--
-- /Tip:/ Use `break`
--
-- prop> \xs -> findRight (const False) -<< fromList xs == isNotZ
--
-- >>> findRight (== 5) (zipper [2, 1] 3 [4, 5])
-- [4,3,2,1] >5< []
--
-- >>> findRight (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [4, 5, 1])
-- [5,4,1,2,3] >1< []
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [1, 4, 5, 1])
-- [1,2,3] >1< [4,5,1]
findRight ::
  (a -> Bool)
  -> ListZipper a
  -> MaybeListZipper a
-- findRight =
--   error "todo: Course.ListZipper#findRight"
findRight fab (ListZipper la' a' la) =
  case break fab la of
    (_, Nil) -> isNotZ
    (lla, a :. rla) -> isZ $ ListZipper (reverse lla ++ a' :. la') a rla

-- | Move the zipper left, or if there are no elements to the left, go to the far right.
--
-- >>> moveLeftLoop (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftLoop (zipper [] 1 [2,3,4])
-- [3,2,1] >4< []
moveLeftLoop ::
  ListZipper a
  -> ListZipper a
-- moveLeftLoop =
--   error "todo: Course.ListZipper#moveLeftLoop"
moveLeftLoop (ListZipper la a' la') =
  case la of
    Nil -> let (a :. tla) = reverse (a' :. la') in ListZipper tla a Nil
    a :. tla -> ListZipper tla a (a' :. la')

-- | Move the zipper right, or if there are no elements to the right, go to the far left.
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [])
-- [] >1< [2,3,4]
moveRightLoop ::
  ListZipper a
  -> ListZipper a
-- moveRightLoop =
--   error "todo: Course.ListZipper#moveRightLoop"
moveRightLoop (ListZipper la' a' la) =
  case la of
    Nil -> let (a :. tla) = reverse (a' :. la') in ListZipper Nil a tla
    a :. tla -> ListZipper (a' :. la') a tla

-- | Move the zipper one position to the left.
--
-- >>> moveLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeft (zipper [] 1 [2,3,4])
-- ><
moveLeft ::
  ListZipper a
  -> MaybeListZipper a
-- moveLeft =
--   error "todo: Course.ListZipper#moveLeft"
moveLeft (ListZipper la a' la') =
  case la of
    Nil -> isNotZ
    a :. tla -> isZ $ ListZipper tla a (a' :. la')

-- | Move the zipper one position to the right.
--
-- >>> moveRight (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRight (zipper [3,2,1] 4 [])
-- ><
moveRight ::
  ListZipper a
  -> MaybeListZipper a
-- moveRight =
--   error "todo: Course.ListZipper#moveRight"
moveRight (ListZipper la' a' la) =
  case la of
    Nil -> isNotZ
    a :. tla -> isZ $ ListZipper (a' :. la') a tla

-- | Swap the current focus with the value to the left of focus.
--
-- >>> swapLeft (zipper [3,2,1] 4 [5,6,7])
-- [4,2,1] >3< [5,6,7]
--
-- >>> swapLeft (zipper [] 1 [2,3,4])
-- ><
swapLeft ::
  ListZipper a
  -> MaybeListZipper a
-- swapLeft =
--   error "todo: Course.ListZipper#swapLeft"
swapLeft (ListZipper la a' la') =
  case la of
    Nil -> isNotZ
    a :. tla -> isZ $ ListZipper (a' :. tla) a la'

-- | Swap the current focus with the value to the right of focus.
--
-- >>> swapRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [4,6,7]
--
-- >>> swapRight (zipper [3,2,1] 4 [])
-- ><
swapRight ::
  ListZipper a
  -> MaybeListZipper a
-- swapRight =
--   error "todo: Course.ListZipper#swapRight"
swapRight (ListZipper la' a' la) =
  case la of
    Nil -> isNotZ
    a :. tla -> isZ $ ListZipper la' a (a' :. tla)

-- | Drop all values to the left of the focus.
--
-- >>> dropLefts (zipper [3,2,1] 4 [5,6,7])
-- [] >4< [5,6,7]
--
-- >>> dropLefts (zipper [] 1 [2,3,4])
-- [] >1< [2,3,4]
--
-- prop> \l x r -> dropLefts (zipper l x r) == zipper [] x r
dropLefts ::
  ListZipper a
  -> ListZipper a
-- dropLefts =
--   error "todo: Course.ListZipper#dropLefts"
dropLefts (ListZipper _ a la) = ListZipper Nil a la

-- | Drop all values to the right of the focus.
--
-- >>> dropRights (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >4< []
--
-- >>> dropRights (zipper [3,2,1] 4 [])
-- [3,2,1] >4< []
--
-- prop> \l x r -> dropRights (zipper l x r) == zipper l x []
dropRights ::
  ListZipper a
  -> ListZipper a
-- dropRights =
--   error "todo: Course.ListZipper#dropRights"
dropRights (ListZipper la a _) = ListZipper la a Nil

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
--
-- >>> moveLeftN 2 (zipper [2,1,0] 3 [4,5,6])
-- [0] >1< [2,3,4,5,6]
--
-- >>> moveLeftN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [3,2,1,0] >4< [5,6]
moveLeftN ::
  Int
  -> ListZipper a
  -> MaybeListZipper a
-- moveLeftN =
--   error "todo: Course.ListZipper#moveLeftN"
moveLeftN n za@(ListZipper la a' la')
  | n < 0 = moveRightN (-n) za
  | n == 0 = isZ za
  | n > length la = isNotZ
  | otherwise =
    let lla = take (n - 1) la
        (a :. rla) = drop (n - 1) la
    in isZ $ ListZipper rla a (reverse lla ++ a' :. la')

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
--
-- >>> moveRightN 1 (zipper [2,1,0] 3 [4,5,6])
-- [3,2,1,0] >4< [5,6]
--
-- >>> moveRightN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [1,0] >2< [3,4,5,6]
moveRightN ::
  Int
  -> ListZipper a
  -> MaybeListZipper a
-- moveRightN =
--   error "todo: Course.ListZipper#moveRightN"
moveRightN n za@(ListZipper la' a' la)
  | n < 0 = moveLeftN (-n) za
  | n == 0 = isZ za
  | n > length la = isNotZ
  | otherwise =
    let lla = take (n - 1) la
        (a :. rla) = drop (n - 1) la
    in isZ $ ListZipper (reverse lla ++ a' :. la') a rla

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveLeftN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [5,4,3,2,1] >6< [7]
--
-- >>> moveLeftN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7,8,9])
-- Left 3
--
-- >>> moveLeftN' (-4) (zipper [5,4,3,2,1] 6 [7,8,9])
-- Left 3
--
-- >>> rights <$> moveLeftN' 1 (zipper [3,2,error "moveLeftN' not sufficiently lazy"] 4 [5,6,7])
-- Right [4,5,6,7]
--
moveLeftN' ::
  Int
  -> ListZipper a
  -> Either Int (ListZipper a)
-- moveLeftN' =
--   error "todo: Course.ListZipper#moveLeftN'"
moveLeftN' n za
  | n < 0 = moveRightN' (-n) za
  | otherwise =
    case moveLeftN n za of
      MLZ Empty -> Left $ length $ lefts za
      MLZ (Full za') -> Right za'

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveRightN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveRightN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [4,3,2,1] >5< [6,7]
--
-- >>> moveRightN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveRightN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [1] >2< [3,4,5,6,7]
--
-- >>> moveRightN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
moveRightN' ::
  Int
  -> ListZipper a
  -> Either Int (ListZipper a)
-- moveRightN' =
--   error "todo: Course.ListZipper#moveRightN'"
moveRightN' n za
  | n < 0 = moveLeftN' (-n) za
  | otherwise =
    case moveRightN n za of
      MLZ Empty -> Left $ length $ rights za
      MLZ (Full za') -> Right za'

-- | Move the focus to the given absolute position in the zipper. Traverse the zipper only to the extent required.
--
-- >>> nth 1 (zipper [3,2,1] 4 [5,6,7])
-- [1] >2< [3,4,5,6,7]
--
-- >>> nth 5 (zipper [3,2,1] 4 [5,6,7])
-- [5,4,3,2,1] >6< [7]
--
-- >>> nth 8 (zipper [3,2,1] 4 [5,6,7])
-- ><
nth ::
  Int
  -> ListZipper a
  -> MaybeListZipper a
-- nth =
--   error "todo: Course.ListZipper#nth"
-- nth n za =
--   let l = length (lefts za)
--   in moveRightN (n - l) za
nth n za = moveRightN (n - index za) za

-- | Return the absolute position of the current focus in the zipper.
--
-- >>> index (zipper [3,2,1] 4 [5,6,7])
-- 3
--
-- prop> \i z -> optional True (\z' -> index z' == i) (toOptional (nth i z))
index ::
  ListZipper a
  -> Int
-- index =
--   error "todo: Course.ListZipper#index"
-- index za = length (lefts za)
index = length . lefts

-- | Move the focus to the end of the zipper.
--
-- >>> end (zipper [3,2,1] 4 [5,6,7])
-- [6,5,4,3,2,1] >7< []
--
-- prop> \lz -> toList lz == toList (end lz)
--
-- prop> \lz -> rights (end lz) == Nil
end ::
  ListZipper a
  -> ListZipper a
-- end =
--   error "todo: Course.ListZipper#end"
end za@(ListZipper la' a' la)
  | isEmpty la = za
  | otherwise =
    let (a :. rla) = reverse la
    in ListZipper (rla ++ a' :. la') a Nil

-- | Move the focus to the start of the zipper.
--
-- >>> start (zipper [3,2,1] 4 [5,6,7])
-- [] >1< [2,3,4,5,6,7]
--
-- prop> \lz -> toList lz == toList (start lz)
--
-- prop> \lz -> lefts (start lz) == Nil
start ::
  ListZipper a
  -> ListZipper a
-- start =
--   error "todo: Course.ListZipper#start"
start za@(ListZipper la a' la')
  | isEmpty la = za
  | otherwise =
    let (a :. rla) = reverse la
    in ListZipper Nil a (rla ++ a' :. la')

-- | Delete the current focus and pull the left values to take the empty position.
--
-- >>> deletePullLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [5,6,7]
--
-- >>> deletePullLeft (zipper [] 1 [2,3,4])
-- ><
deletePullLeft ::
  ListZipper a
  -> MaybeListZipper a
-- deletePullLeft =
--   error "todo: Course.ListZipper#deletePullLeft"
deletePullLeft (ListZipper la a' la')
  | isEmpty la = isNotZ
  | otherwise =
    let (a :. rla) = la
    in isZ $ ListZipper rla a la'

-- | Delete the current focus and pull the right values to take the empty position.
--
-- >>> deletePullRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [6,7]
--
-- >>> deletePullRight (zipper [3,2,1] 4 [])
-- ><
deletePullRight ::
  ListZipper a
  -> MaybeListZipper a
-- deletePullRight =
--   error "todo: Course.ListZipper#deletePullRight"
deletePullRight (ListZipper la' a' la)
  | isEmpty la = isNotZ
  | otherwise =
    let (a :. rla) = la
    in isZ $ ListZipper la' a rla

-- | Insert at the current focus and push the left values to make way for the new position.
--
-- >>> insertPushLeft 15 (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >15< [5,6,7]
--
-- >>> insertPushLeft 15 (zipper [] 1 [2,3,4])
-- [1] >15< [2,3,4]
--
-- prop> \i z -> optional False (==z) (toOptional (deletePullLeft (insertPushLeft i z)))
insertPushLeft ::
  a
  -> ListZipper a
  -> ListZipper a
-- insertPushLeft =
--   error "todo: Course.ListZipper#insertPushLeft"
insertPushLeft a (ListZipper la' a' la) =
  ListZipper (a' :. la') a la

-- | Insert at the current focus and push the right values to make way for the new position.
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >15< [4,5,6,7]
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [])
-- [3,2,1] >15< [4]
--
-- prop> \i z -> optional False (==z) (toOptional (deletePullRight (insertPushRight i z)))
insertPushRight ::
  a
  -> ListZipper a
  -> ListZipper a
-- insertPushRight =
--   error "todo: Course.ListZipper#insertPushRight"
insertPushRight a (ListZipper la a' la') =
  ListZipper la a (a' :. la')

-- | Implement the `Applicative` instance for `ListZipper`.
-- `pure` produces an infinite list zipper (to both left and right).
-- (<*>) zips functions with values by function application.
--
-- prop> \n -> all . (==) <*> take n . lefts . pure
--
-- prop> \n -> all . (==) <*> take n . rights . pure
--
-- >>> zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)] <*> zipper [3,2,1] 4 [5,6,7]
-- [5,12] >8< [15,24,12]
instance Applicative ListZipper where
-- /Tip:/ Use @List#repeat@.
  -- pure =
  --   error "todo: Course.ListZipper pure#instance ListZipper"
  -- pure :: a -> ListZipper a
  pure a = ListZipper (repeat a) a (repeat a)
-- /Tip:/ Use `zipWith`
  -- (<*>) =
  --   error "todo: Course.ListZipper (<*>)#instance ListZipper"
  -- (<*>) :: ListZipper (a -> b) -> ListZipper a -> ListZipper b
  ListZipper lfab fab lfab' <*> ListZipper la a la' =
    let ap = \f x -> f x
    in ListZipper (zipWith ap lfab la) (fab a) (zipWith ap lfab' la')

-- | Implement the `Applicative` instance for `MaybeListZipper`.
--
-- /Tip:/ Use @pure@ for `ListZipper`.
-- /Tip:/ Use `<*>` for `ListZipper`.
--
-- prop> \z n -> let is (MLZ (Full z)) = z in all . (==) <*> take n . lefts . is . pure
--
-- prop> \z n -> let is (MLZ (Full z)) = z in all . (==) <*> take n . rights . is . pure
--
-- >>> isZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> isZ (zipper [3,2,1] 4 [5,6,7])
-- [5,12] >8< [15,24,12]
--
-- >>> isNotZ <*> isZ (zipper [3,2,1] 4 [5,6,7])
-- ><
--
-- >>> isZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> isNotZ
-- ><
--
-- >>> isNotZ <*> isNotZ
-- ><
instance Applicative MaybeListZipper where
  -- pure =
  --   error "todo: Course.ListZipper pure#instance MaybeListZipper"
  -- pure :: a -> MaybeListZipper a
  pure = isZ . pure
  -- (<*>) =
  --   error "todo: Course.ListZipper (<*>)#instance MaybeListZipper"
  -- (<*>) :: MaybeListZipper (a -> b) -> MaybeListZipper a -> MaybeListZipper b
  MLZ (Full zfab) <*> MLZ (Full za) = MLZ $ Full (zfab <*> za)
  _ <*> _ = MLZ Empty

-- | Implement the `Extend` instance for `ListZipper`.
-- This implementation "visits" every possible zipper value derivable from a given zipper (i.e. all zippers to the left and right).
--
-- /Tip:/ Use @List#unfoldr@.
--
-- >>> id <<= (zipper [2,1] 3 [4,5])
-- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
instance Extend ListZipper where
  -- (<<=) =
  --   error "todo: Course.ListZipper (<<=)#instance ListZipper"
  -- (<<=) :: (ListZipper a -> b) -> ListZipper a -> ListZipper b
  fzab <<= za =
    let lb = unfoldr (f moveLeft) za
        lb' = unfoldr (f moveRight) za
        b = fzab za
        f moveFunc za' = case moveFunc za' of
          MLZ Empty -> Empty
          MLZ (Full za'') -> Full (fzab za'', za'')
    in ListZipper lb b lb'

-- | Implement the `Extend` instance for `MaybeListZipper`.
-- This instance will use the `Extend` instance for `ListZipper`.
--
--
-- id <<= isNotZ
-- ><
--
-- >>> id <<= (isZ (zipper [2,1] 3 [4,5]))
-- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
instance Extend MaybeListZipper where
  -- (<<=) =
  --   error "todo: Course.ListZipper (<<=)#instance MaybeListZipper"
  -- (<<=) :: (MaybeListZipper a -> b) -> MaybeListZipper a -> MaybeListZipper b
  _ <<= MLZ Empty = isNotZ
  fmzab <<= MLZ (Full za) =
    let fzab = fmzab . isZ
    in isZ (fzab <<= za)

-- | Implement the `Comonad` instance for `ListZipper`.
-- This implementation returns the current focus of the zipper.
--
-- >>> copure (zipper [2,1] 3 [4,5])
-- 3
instance Comonad ListZipper where
  -- copure =
  --   error "todo: Course.ListZipper copure#instance ListZipper"
  -- copure :: ListZipper a -> a
  copure (ListZipper _ a _) = a

-- | Implement the `Traversable` instance for `ListZipper`.
-- This implementation traverses a zipper from left to right while running
-- some `Applicative` effect through the zipper.
-- An effectful zipper is returned.
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7])
-- Full [1,2,3] >4< [5,6,7]
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Empty, Full 6, Full 7])
-- Empty
--
-- >>> traverse id (zipper [error "traversing left values in wrong order", Empty] (error "traversing focus before left values") [Full 5, Full 6, Full 7])
-- Empty
instance Traversable ListZipper where
  -- traverse =
  --   error "todo: Course.ListZipper traverse#instance ListZipper"
  -- traverse :: (a -> k b) -> ListZipper a -> k (ListZipper b)
  traverse fakb za@(ListZipper lla a rla) =
    let lklb = traverse fakb (reverse lla)
        kb = fakb a
        rklb = traverse fakb rla
        f llb' b' rlb' = ListZipper (reverse llb') b' rlb'
    in lift3 f lklb kb rklb

-- | Implement the `Traversable` instance for `MaybeListZipper`.
--
-- /Tip:/ Use `traverse` for `ListZipper`.
--
-- >>> traverse id isNotZ
-- ><
--
-- >>> traverse id (isZ (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7]))
-- Full [1,2,3] >4< [5,6,7]
instance Traversable MaybeListZipper where
  -- traverse =
  --   error "todo: Course.ListZipper traverse#instance MaybeListZipper"
  -- traverse :: (a -> k b) -> MaybeListZipper a -> k (MaybeListZipper b)
  traverse _ (MLZ Empty) = pure isNotZ
  traverse fakb (MLZ (Full za)) = isZ <$> traverse fakb za

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Show a => Show (ListZipper a) where
  show (ListZipper l x r) =
    stringconcat [show l, " >", show x, "< ", show r]

instance Show a => Show (MaybeListZipper a) where
  show (MLZ (Full z)) = show z
  show (MLZ Empty) = "><"
