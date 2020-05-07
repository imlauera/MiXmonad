import XMonad
import XMonad.Actions.CycleWS 
import XMonad.Actions.DwmPromote
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Hooks.ManageDocks 
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO

main = do 
        status <- spawnPipe myDzenStatus
        info <- spawnPipe myDzenConky
        xmonad $ docks defaultConfig
            { borderWidth        = 2
            , terminal           = "lxterminal"
            , normalBorderColor  = "#444"
            , focusedBorderColor = "red"
            , workspaces = ["1","2","3","4","5","6"]
            , manageHook = manageHook defaultConfig
            , layoutHook =  avoidStruts $ layoutHook defaultConfig
            , modMask = mod4Mask     -- Rebind Mod to the Windows key
            , startupHook = myStartupHook
            , logHook    = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn status }
            } `additionalKeysP` myKeys

myDzenStatus = "dzen2 -w '531' -ta 'l'" ++ myDzenStyle
myDzenConky  = "conky -c ~/nist778/ActualXmonad/conkyrc | dzen2 -x '320' -ta 'r'" ++ myDzenStyle
myDzenStyle  = " -h '15' -bg '#000' -fn 'ubuntu:regular:size=8'"

myDzenPP  = dzenPP
    { ppCurrent = dzenColor "white" "" . wrap " " " "
    , ppHidden  = dzenColor "#999" "" . wrap " " " "
    , ppLayout  = dzenColor "#aaaaaa" "" . wrap "^ca(1,xdotool key super+space)· " " ·^ca()"
    , ppTitle   = dzenColor "#a7a7a7" "" 
                    . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                           "                          ^ca()^ca()" . shorten 40 . dzenEscape
    }

myStartupHook :: X ()
myStartupHook = do
  spawn "killall stalonetray ; stalonetray -c ~/nist778/ActualXmonad/stalonetrayrc &"
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
         , ("M-p"        , spawn "dmenu_run -nb black -sb white -nf white -sf red -fn 'Ubuntu'") -- app launcher
         , ("M-r"        , spawn "xmonad --restart"              ) -- restart xmonad w/o recompiling
         , ("M-S-w"      , spawn "chromium --incognito"          ) -- launch private browser
         , ("M-e"        , spawn "thunar"                      ) -- launch file manager
         , ("M-S-l"        , spawn "xscreensaver-command -lock"         ) -- launch file manager
         , ("M-<Up>"     , spawn "amixer -q set -q Master 3%+") -- Volume control Up
         , ("M-<Down>"   , spawn "amixer -q set -q Master 3%-") -- Down
         ]
