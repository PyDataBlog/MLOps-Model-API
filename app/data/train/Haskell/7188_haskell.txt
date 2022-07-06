{-# LANGUAGE CPP #-}

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
module Access.System.FilePath
    ( module Access.System.FilePath.Windows
    ) where

import Access.System.FilePath.Windows

#else
module Access.System.FilePath
    ( module Access.System.FilePath.Posix
    ) where

import Access.System.FilePath.Posix
#endif
