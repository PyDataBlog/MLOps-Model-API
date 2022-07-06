{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -w #-}

module Prelude (print, Base.Int) where

import                  Fay.Types (Fay)
import                  Fay.FFI
import qualified "base" Prelude as Base
import "base" Prelude (Bool(True,False)
                      ,(||),(&&),seq,Eq,(==),(/=))

--------------------------------------------------------------------------------
-- Fixities

infixr 9  .

infixl 1  >>, >>=
infixr 0  $

--------------------------------------------------------------------------------
-- Aliases of base

type String = Base.String
type Double = Base.Double
type Char = Base.Char

--------------------------------------------------------------------------------
-- Standard data types

-- | Maybe type.
data Maybe a = Just a | Nothing
instance Base.Read a => Base.Read (Maybe a)
instance Base.Show a => Base.Show (Maybe a)

--------------------------------------------------------------------------------
-- Monads

-- | Monomorphic bind for Fay.
(>>=) :: Fay a -> (a -> Fay b) -> Fay b
(>>=) = ffi "Fay$$bind(%1)(%2)"

-- | Monomorphic then for Fay.
(>>) :: Ptr (Fay a) -> Ptr (Fay b) -> Ptr (Fay b)
(>>) = ffi "Fay$$then(%1)(%2)"

-- | Monomorphic return for Fay.
return :: a -> Fay a
return = ffi "Fay$$return(%1)"

--------------------------------------------------------------------------------
-- Show

-- | Uses JSON.stringify.
show :: Automatic a -> String
show = ffi "JSON.stringify(%1)"

--------------------------------------------------------------------------------
-- Functions

(.) :: (t1 -> t) -> (t2 -> t1) -> t2 -> t
(f . g) x = f (g x)

($) :: (t1 -> t) -> t1 -> t
f $ x = f x

--------------------------------------------------------------------------------
-- IO

print :: Automatic a -> Fay ()
print = ffi "(function(x) { if (console && console.log) console.log(x) })(%1)"

putStrLn :: String -> Fay ()
putStrLn = ffi "(function(x) { if (console && console.log) console.log(x) })(%1)"
