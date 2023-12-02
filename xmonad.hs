import XMonad
import XMonad.Actions.CycleWS 
import XMonad.Actions.DwmPromote
import XMonad.Hooks.ManageHelpers    -- dock/tray mgmt
import XMonad.Hooks.DynamicLog
import Data.List
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Hooks.ManageDocks 
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Layout.Gaps
import XMonad.Hooks.EwmhDesktops
import qualified XMonad.StackSet as W   -- manageHook rules
import System.IO

main = do 
        status <- spawnPipe myDzenStatus
        info <- spawnPipe myDzenConky
        --xmonad $ ewmh $docks defaultConfig
        xmonad $ ewmh $docks def
            { borderWidth        = 2
            , terminal           = "st -e dvtm"
            , normalBorderColor  = "black"
            , focusedBorderColor = "#20a6f5"
            , workspaces = ["1","2","3","4","5","6","7","8"]
            , manageHook = manageDocks <+> myManageHook
            --, layoutHook = spacing 3 $ avoidStruts $ layoutHook defaultConfig
            , layoutHook = spacing 3 $ avoidStruts $ layoutHook def
            , modMask = mod4Mask     -- Rebind Mod to the Windows key
            , startupHook = myStartupHook 
            , logHook    = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn status }
            } `additionalKeysP` myKeys


myDzenStatus = "dzen2 -w '531' -ta 'l'" ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -x '320' -ta 'r'" ++ myDzenStyle
myDzenStyle  = " -h '17' -bg '#000' -fn 'ubuntu:regular:size=10'"

myDzenPP  = dzenPP
    { ppCurrent = dzenColor "#acb5ec" "" . wrap " " " "
    , ppHidden  = dzenColor "#664" "" . wrap " " " "
    , ppLayout  = dzenColor "#aaaaaa" "" . wrap "^ca(1,xdotool key super+space)· " " ·^ca()"
    , ppTitle   = dzenColor "#a7a7a7" "" 
                    . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                           "                          ^ca()^ca()" . shorten 40 . dzenEscape
    }

myManageHook = composeAll
    [
     className =? "MPlayer"        --> doFloat
    , className =? "Thunderbird"    --> doF (W.shift "3")
    , className =? "XCalc"          --> doFloat
    , className =? "Galculator"     --> doFloat
    , className =? "Thunar"         --> doFloat 
    , fmap ("Discord" `isInfixOf`) title --> doF (W.shift "9")
    , fmap ("Chromium" `isInfixOf`) title --> (doShift "3")
    ]

myStartupHook :: X ()
myStartupHook = do
  spawn "killall stalonetray ; stalonetray -c ~/.xmonad/stalonetrayrc &"
  spawn "xscreensaver-command -exit;  xscreensaver -no-splash & "


myKeys = [ ("M-b"        , sendMessage ToggleStruts              ) -- toggle the status bar gap
         , ("M-<Return>" , dwmpromote                            ) -- swap the focused window and the master window
         , ("M-<Tab>"    , toggleWS                              ) -- toggle last workspace (super-tab)
         , ("M-<Right>"  , nextWS                                ) -- go to next workspace
         , ("M-<Left>"   , prevWS                                ) -- go to prev workspace
         , ("M-S-<Right>", shiftToNext                           ) -- move client to next workspace
         , ("M-S-<Left>" , shiftToPrev                           ) -- move client to prev workspace
         , ("M-h"        , sendMessage Shrink                    ) -- Shrink the master area (with the reflectHoriz the master area is on the right side)
         , ("M-l"        , sendMessage Expand                    ) -- Expand the master area (the keybindings swapped cause I use reflectHoriz)
         , ("M-p"        , spawn "dmenu_run -nb black -sb black -nf white -sf lightblue -fn 'ubuntu:size=10'") -- app launcher
         , ("M-r"        , spawn "xmonad --restart"              ) -- restart xmonad w/o recompiling
         , ("M-S-x"        , spawn "sh ~/nist778/scripts/get_wallpaper.sh"              ) -- restart xmonad w/o recompiling
         , ("M-e"        , spawn "pcmanfm"                      ) -- launch file manager
         , ("M-S-l"        , spawn "xscreensaver-command -lock"         ) -- launch file manager
         , ("M-S-<Up>"     , spawn "amixer -q set -q PCM 3%+") -- Volume control Up
         , ("M-S-<Down>"   , spawn "amixer -q set -q PCM 3%-") -- Down
         ]
