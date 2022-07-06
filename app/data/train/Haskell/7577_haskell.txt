{-# LANGUAGE ForeignFunctionInterface #-}
-------------------------------------------------------------------------------
-- |
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : BSD3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module Sound.Csound.Debugger (
    csoundDebuggerInit,
    csoundDebuggerClean,
    --csoundSetInstrumentBreakpoint,
    --csoundRemoveInstrumentBreakpoint,
    csoundClearBreakpoints,
    --csoundSetBreakpointCallback,
    csoundDebuggerContinue,
    csoundDebugStop,
    --csoundDebugGetInstrument
) where

import Control.Monad.IO.Class
import Foreign
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "csdebug.h csoundDebuggerInit" csoundDebuggerInit' :: Ptr () -> IO ()
foreign import ccall "csdebug.h csoundDebuggerClean" csoundDebuggerClean' :: Ptr () -> IO ()
--foreign import ccall "csdebug.h csoundSetInstrumentBreakpoint" csoundSetInstrumentBreakpoint'
--foreign import ccall "csdebug.h csoundRemoveInstrumentBreakpoint" csoundRemoveInstrumentBreakpoint'
foreign import ccall "csdebug.h csoundClearBreakpoints" csoundClearBreakpoints' :: Ptr () -> IO ()
--foreign import ccall "csdebug.h csoundSetBreakpointCallback" csoundSetBreakpointCallback'
foreign import ccall "csdebug.h csoundDebugContinue" csoundDebugContinue' :: Ptr () -> IO ()
foreign import ccall "csdebug.h csoundDebugStop" csoundDebugStop' :: Ptr () -> IO ()
--foreign import ccall "csdebug.h csoundDebugGetInstrument" csoundDebugGetInstrument'

csoundDebuggerInit :: MonadIO m => Ptr () -> m ()
csoundDebuggerInit csnd = liftIO (csoundDebuggerInit' csnd)

csoundDebuggerClean :: MonadIO m => Ptr () -> m ()
csoundDebuggerClean csnd = liftIO (csoundDebuggerClean' csnd)

--csoundSetInstrumentBreakpoint 
--csoundSetInstrumentBreakpoint 

--csoundRemoveInstrumentBreakpoint
--csoundRemoveInstrumentBreakpoint

csoundClearBreakpoints :: MonadIO m => Ptr () -> m ()
csoundClearBreakpoints csnd = liftIO (csoundClearBreakpoints' csnd)

--csoundSetBreakpointCallback
--csoundSetBreakpointCallback

csoundDebuggerContinue :: MonadIO m => Ptr () -> m ()
csoundDebuggerContinue csnd = liftIO (csoundDebuggerContinue' csnd)

csoundDebugStop :: MonadIO m => Ptr () -> m ()
csoundDebugStop csnd = liftIO (csoundDebugStop' csnd)

--csoundDebugGetInstrument
--csoundDebugGetInstrument
