import Control.Monad.Reader.Class (ask)
import XMonad.Main (xmonad)
import XMonad.Core
       ( X
       , spawn
       , ManageHook
       , layoutHook
       , startupHook
       , manageHook
       , logHook
       , modMask
       , terminal
       , borderWidth
       , normalBorderColor
       , focusedBorderColor
       , workspaces
       )

import XMonad.ManageHook
       ( composeAll
       , (<||>), (=?), (-->), (<+>)
       , className, doF, stringProperty )
import XMonad.Config ( defaultConfig )
import XMonad.Hooks.DynamicLog
       ( dynamicLogWithPP
       , ppOutput
       , ppTitle
       , ppLayout
       , ppVisible
       , ppCurrent
       , xmobarPP
       , xmobarColor
       , shorten
       )
import XMonad.Hooks.ManageDocks ( avoidStruts, docks, manageDocks)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.StackSet (sink)
import Graphics.X11.Types
import Data.Bits ( (.|.) )
import System.IO (hPutStrLn)

----------------------- Colors ----------------------
magenta = "#FF14E5"
blue1   = "#29acff"
green1  = "#60ff45"
white1  = "#FFFFFF"
gray1   = "#3D3D3D"
gray2   = "#808080"
gray3   = "#CCCCCC"
yellow1 = "#FFF500"
yellow2 = "#ffff66"

myManageHook :: ManageHook
myManageHook = composeAll [ (role =? "gimp-toolbox" <||> role =? "gimp-image-window") -->(unfloat)
                          , className =? "MPlayer" --> (unfloat) ]
                   where unfloat = ask >>= doF . sink
                         role = stringProperty "WM_WINDOW_ROLE"

myLayoutHook = smartSpacing 5 $ avoidStruts  $ layoutHook defaultConfig

myKeys = [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
         , ((mod4Mask .|. shiftMask, xK_o), spawn "urxvt -e ranger")
         , ((mod4Mask .|. shiftMask, xK_f), spawn "firefox")
         , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
         , ((0, xK_Print), spawn "scrot")
         ]

myLogHook h = dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn h
                                        , ppTitle = xmobarColor blue1 "" . shorten 70
                                        , ppCurrent = xmobarColor blue1 ""
                                        , ppVisible = xmobarColor white1 ""
                                      --, ppHiddenNoWindows = xmobarColor magenta ""
                                        , ppLayout = xmobarColor gray2 ""
                                        }

------------------------  Bottom bar stuff  ----------------------
myStartupHook :: X ()
myStartupHook = do spawn xmobarBottom

bin_xmobar = "/run/current-system/sw/bin/xmobar"
rc_xmobarTop = "/home/bernie/.xmobarrc.hs"
rc_xmobarBottom = "/home/bernie/.xmobarrc_bottom.hs"

xmobarTop = bin_xmobar ++ " " ++ rc_xmobarTop
xmobarBottom = bin_xmobar ++ " " ++ rc_xmobarBottom
------------------------------------------------------------------


main :: IO ()
main = do
    xmproc <- spawnPipe xmobarTop
    xmonad $ docks defaultConfig
        { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = myLayoutHook
        , logHook = myLogHook xmproc
--        , startupHook = myStartupHook
        , modMask = mod4Mask
        , terminal = "urxvt"
        , borderWidth = 1
        , normalBorderColor = gray1
        , focusedBorderColor = blue1
        , workspaces = ["sh1","sh2","sh3","gimp4","pdf5","com6","mpc7","web8","web9"]
        } `additionalKeys` myKeys


