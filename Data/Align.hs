-----------------------------------------------------------------------------
-- | Module     :  Data.Align
--
--   'These'-based zipping and unzipping of structures with
--   non-uniform shapes.
module Data.Align (Align(..), Unalign(..)) where

import Data.These
import Data.Functor.Identity (Identity(..))
import Control.Applicative (ZipList(..), (<$>))
import Data.Tree (Tree(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

oops :: String -> a
oops = error . ("Data.Align: internal error: " ++)

-- --------------------------------------------------------------------------
-- | Zipping for structures with non-uniform shapes.
--
--   Complete definition: Either @align@ or @alignWith@.
--
--   Laws:
--
--   1. @align@ = @alignWith id@
--
--   2. @alignWith f a b@ = @fmap f (align a b)@
--
--   3. @join align@ = @fmap (join Both)@
class (Functor f) => Align f where
    align :: f a -> f b -> f (These a b)
    align = alignWith id

    alignWith :: (These a b -> c) -> f a -> f b -> f c
    alignWith f a b = f <$> align a b

instance Align Identity where
    align (Identity a) (Identity b) = Identity (These a b)

instance Align Maybe where
    align Nothing Nothing = Nothing
    align (Just a) Nothing = Just (This a)
    align Nothing (Just b) = Just (That b)
    align (Just a) (Just b) = Just (These a b)

instance Align [] where
    align xs [] = This <$> xs
    align [] ys = That <$> ys
    align (x:xs) (y:ys) = These x y : align xs ys

instance Align ZipList where
    align (ZipList xs) (ZipList ys) = ZipList (align xs ys)

instance Align Tree where
    align (Node a xs) (Node b ys) = Node (These a b) (map merge (align xs ys))
      where merge = these (fmap This) (fmap That) align

-- could probably be more efficient...
instance Align Seq where
    align xs ys =
        case Seq.viewl xs of
            Seq.EmptyL   -> That <$> ys
            x Seq.:< xs' ->
                case Seq.viewl ys of
                    Seq.EmptyL   -> This <$> xs
                    y Seq.:< ys' -> These x y Seq.<| align xs' ys'

instance (Ord k) => Align (Map k) where
    align m n = Map.unionWith merge (Map.map This m) (Map.map That n)
      where merge (This a) (That b) = These a b
            merge _ _ = oops "Align Map: merge"

instance Align IntMap where
    align m n = IntMap.unionWith merge (IntMap.map This m) (IntMap.map That n)
      where merge (This a) (That b) = These a b
            merge _ _ = oops "Align IntMap: merge"

-- --------------------------------------------------------------------------
-- | Unzipping for structures of non-uniform shapes.
--
--   Complete definition: nothing; a default definition is provided,
--   but it may not have the desired definition for all functors. See
--   the source for more information.
class (Align f) => Unalign f where
    -- This needs laws. Specifically, some notion of not duplicating
    -- the effects is required, and a way to express its relationship
    -- with @align@.
    unalign :: f (These a b) -> (f (Maybe a), f (Maybe b))
    unalign x = (fmap left x, fmap right x)
      where left  = these Just (const Nothing) (\a _ -> Just a)
            right = these (const Nothing) Just (\_ b -> Just b)

instance Unalign Identity

instance Unalign Maybe

instance Unalign [] where
    unalign = foldr (these a b ab) ([],[]) 
      where a  l   ~(ls,rs) = (Just l :ls, Nothing:rs)
            b    r ~(ls,rs) = (Nothing:ls, Just r :rs)
            ab l r ~(ls,rs) = (Just l :ls, Just r :rs)

instance Unalign ZipList where
    unalign (ZipList xs) = (ZipList ys, ZipList zs)
      where (ys, zs) = unalign xs

-- TODO: More obvious instances...
