module Data.Intern (
      Interned
    , InternSet
    , empty
    , singleton
    , insert
    , fromList
    , toList
    , delete
    , member
    , notMember
    ) where


import qualified Data.Map as M


type Cache a = M.Map a a
type Interned a = a


newtype InternSet a = InternSet { toCache :: Cache a }


empty :: InternSet a
empty = InternSet M.empty


singleton :: a -> InternSet a
singleton x = InternSet $ M.singleton x x


instance (Show a) => Show (InternSet a) where
    show = ("InternSet " ++) . show . M.keys . toCache


insert :: (Ord a) => a -> InternSet a -> (Interned a, InternSet a)
insert x iset = case M.lookup x $ toCache iset of
    Just y -> (y, iset)
    Nothing -> (x, InternSet $ M.insert x x $ toCache iset)


fromList :: (Ord a) => [a] -> InternSet a -> ([a], InternSet a)
fromList [] iset = ([], iset)
fromList (x:xs) iset = case insert x iset of
    (y, iset') -> let
        (ys, iset'') = fromList xs iset'
        in (y : ys, iset'')


toList :: InternSet a -> [a]
toList = M.keys . toCache


delete :: (Ord a) => a -> InternSet a -> InternSet a
delete x = InternSet . M.delete x . toCache


member :: (Ord a) => a -> InternSet a -> Bool
member x = M.member x . toCache


notMember :: (Ord a) => a -> InternSet a -> Bool
notMember x = not . member x




