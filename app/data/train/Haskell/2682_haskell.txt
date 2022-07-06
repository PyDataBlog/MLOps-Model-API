{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
import Foreign
import Foreign.C.Types
import System.IO.Unsafe

data CFoo

foreign import ccall unsafe "foo_c.h foo_ctor"
  c_foo_ctor :: CInt -> IO (Ptr CFoo)

data Foo = Foo !(Ptr CFoo)
  deriving (Eq, Ord, Show)

newFoo :: Int -> Either String Foo
newFoo x = unsafePerformIO $ do
  foo_ptr <- c_foo_ctor (fromIntegral x)
  if foo_ptr == nullPtr
    then
      return (Left "Failed to construct Foo")
    else
      return (Right (Foo foo_ptr))

foreign import ccall unsafe "foo_c.h foo_get"
  c_foo_get :: Ptr CFoo -> IO CInt

getFoo :: Foo -> Int
getFoo (Foo foo_ptr) = unsafePerformIO $ do
  c <- c_foo_get foo_ptr
  return $ fromIntegral c

foreign import ccall unsafe "foo_c.h foo_dtor"
  c_foo_dtor :: Ptr CFoo -> IO ()

delFoo :: Foo -> IO ()
delFoo (Foo foo_ptr) = c_foo_dtor foo_ptr

main :: IO ()
main = do
  let r = newFoo 10
  case r of
    Left e -> print e
    Right f -> 
      do
        print (getFoo f)
        delFoo f
        print "foo deleted"

