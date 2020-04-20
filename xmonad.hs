import XMonad
import XMonad.Actions.CycleWindows -- classic alt-tab
import XMonad.Actions.CycleWS      -- cycle thru WS', toggle last WS
import XMonad.Actions.DwmPromote   -- swap master like dwm
import XMonad.Hooks.DynamicLog     -- statusbar 
import XMonad.Hooks.EwmhDesktops   -- fullscreenEventHook fixes chrome fullscreen
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import XMonad.Hooks.ManageHelpers    -- dock/tray mgmt
import XMonad.Hooks.UrgencyHook    -- window alert bells 
import XMonad.Layout.Named         -- custom layout names
import XMonad.Layout.NoBorders     -- smart borders on solo clients
import XMonad.Layout.Spacing
import XMonad.Util.Run(spawnPipe)  -- spawnPipe and hPutStrLn
import XMonad.Layout.AutoMaster
import XMonad.Util.Themes
import XMonad.Layout.Spiral
import XMonad.Layout.PerWorkspace
import XMonad.Hooks.DynamicProperty
import XMonad.Layout.TwoPane
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import XMonad.Layout.Reflect
import XMonad.Layout.Accordion
import XMonad.Layout.Combo
import XMonad.Layout.Magnifier
import XMonad.Layout.GridVariants
import XMonad.Layout.CenteredMaster
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import System.IO                   -- hPutStrLn scope
import XMonad.Layout.SimplestFloat
import XMonad.Hooks.Place
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W   -- manageHook rules

myStartupHook :: X ()
myStartupHook = do
  spawn "killall stalonetray ; stalonetray -c ~/.xmonad/stalonetrayrc &"
  spawn "feh --bg-fil /home/nist778/Imágenes/pantsu/wide/tits.jpg &"
  spawn "xcompmgr &" -- Podés usar compton es más pesado
  spawn "nm-applet &"
  spawn "xscreensaver-command -exit;  xscreensaver -no-splash & "
  spawn "blueman-applet &"


main = do
        status <- spawnPipe myDzenStatus    -- xmonad status on the left
        conky <- spawnPipe myDzenConky    -- xmonad status on the left
        xmonad $ ewmh $ docks defaultConfig 
            { modMask            = mod4Mask
            , terminal           = "lxterminal"
            , borderWidth        = 2
            , normalBorderColor  = "#222"
            , focusedBorderColor = "#ff0000"
            , handleEventHook    = fullscreenEventHook <+>
                                   handleEventHook def
            , workspaces = ["1","2","3","4","5"]
            , layoutHook = smartSpacing 2 $ myLayoutHook
            --, manageHook = manageDocks   <+> doCenterFloat <+> myManageHook
            , manageHook = manageDocks   <+> myManageHook
            , startupHook = myStartupHook <+> setFullscreenSupported
            , logHook    = myLogHook status
            } 
            `additionalKeysP` myKeys

-- dynamicPropertyChange "Spotify Free" (className =? "Spotify" --> doShift "2")

-- Layouts
-- the default layout is fullscreen with smartborders applied to all
-- myLayoutHook = simplestFloat
add_layout x = smartBorders $ avoidStruts $ x
myLayoutHook = add_layout $ tall3 ||| tall1 ||| simplestFloat 
  where -- tall2 = reflectHoriz $ TwoPane (3/100) (1/2);
        tall3 = reflectHoriz $ ResizableTall 1 (3/100) (1/2) [];
        tall1 = Full;
        -- tall5 = ThreeColMid 1 (3/100) (1/2)



-- Window management
--
myManageHook = composeAll
    [
     className =? "MPlayer"        --> doFloat
    , className =? "Vlc"            --> doFloat
    , className =? "Gnuplot_qt"     --> doCenterFloat
    , className =? "Gnuplot"        --> doCenterFloat
    , className =? "Gnuplot_x11"    --> doCenterFloat
    , className =? "Android-Studio" --> doF(W.shift "3")
    , className =? "Thunderbird"    --> doF (W.shift "4")
    , className =? "XCalc"          --> doFloat
    , className =? "XCalc"          --> doFloat
    , className =? "Minecraft"      --> doFloat
    , className =? "Thunar"         --> doFloat 
    , className =? "Gimp"           --> doF (W.shift "3") 
    ]


-- hack to let firefox fullscreen
setFullscreenSupported :: X ()
setFullscreenSupported = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom ["_NET_WM_STATE_HIDDEN"
                         ,"_NET_WM_STATE_FULLSCREEN" -- XXX Copy-pasted to add this line
                         ,"_NET_NUMBER_OF_DESKTOPS"
                         ,"_NET_CLIENT_LIST"
                         ,"_NET_CLIENT_LIST_STACKING"
                         ,"_NET_CURRENT_DESKTOP"
                         ,"_NET_DESKTOP_NAMES"
                         ,"_NET_ACTIVE_WINDOW"
                         ,"_NET_WM_DESKTOP"
                         ,"_NET_WM_STRUT"
                         ]
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)



-- Statusbar 
--
myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myDzenStatus = "dzen2 -w '531' -ta 'l'" ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -x '320' -ta 'r'" ++ myDzenStyle
myDzenStyle  = " -h '20' -bg '#000' -fn 'ubuntu:regular:size=12'"

myDzenPP  = dzenPP
    { ppCurrent = dzenColor "#dcfefd" "" . wrap "" "*"  
    , ppHidden  = dzenColor "#fff" "" . wrap " " " "
    , ppHiddenNoWindows = dzenColor "#a6a6a6" "" . wrap " " " "
    , ppUrgent  = dzenColor "#ff0000" "" . wrap " " " "
    , ppSep     = "     "
    , ppLayout  = dzenColor "#aaaaaa" "" . wrap "^ca(1,xdotool key super+space)· " " ·^ca()"
    , ppTitle   = dzenColor "#ffffff" "" 
                    . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                           "                          ^ca()^ca()" . shorten 40 . dzenEscape
    }

-- Key bindings
--
myKeys = [ ("M-b"        , sendMessage ToggleStruts              ) -- toggle the status bar gap
         , ("M1-<Tab>"   , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab ) -- classic alt-tab behaviour
         , ("M-<Return>" , dwmpromote                            ) -- swap the focused window and the master window
         , ("M-<Tab>"    , toggleWS                              ) -- toggle last workspace (super-tab)
         , ("M-<Right>"  , nextWS                                ) -- go to next workspace
         , ("M-<Left>"   , prevWS                                ) -- go to prev workspace
         , ("M-S-<Right>", shiftToNext                           ) -- move client to next workspace
         , ("M-S-<Left>" , shiftToPrev                           ) -- move client to prev workspace
         , ("M-h"        , sendMessage Expand                    ) -- Expand the master area (with the reflectHoriz the master area is on the right side)
         , ("M-l"        , sendMessage Shrink                    ) -- Shrink the master area (the keybindings swapped cause I use reflectHoriz)
         , ("M-S-a"      , sendMessage MirrorShrink              )
         , ("M-S-z"      , sendMessage MirrorExpand               )
         , ("M-p"        , spawn "dmenu_run -nb black -sb 'purple'"                         ) -- app launcher
         , ("M-r"        , spawn "xmonad --restart"              ) -- restart xmonad w/o recompiling
         , ("M-S-w"      , spawn "chromium --incognito"          ) -- launch private browser
         , ("M-e"        , spawn "thunar"                      ) -- launch file manager
         , ("M-S-l"        , spawn "xscreensaver-command -lock"         ) -- launch file manager
         , ("C-M1-l"     , spawn "gnome-screensaver-command --lock"              ) -- lock screen
         , ("M-s"        , spawn "urxvtcd -e bash -c 'screen -dRR -S $HOSTNAME'" ) 
         -- launch screen session
         , ("M-<Up>"     , spawn "amixer -q set -q Master 1%+") -- Volume control Up
         , ("M-<Down>"   , spawn "amixer -q set -q Master 1%-") -- Down
         , ("C-M1-<Delete>" , spawn "sudo shutdown -r now"       ) -- reboot
         , ("C-M1-<Insert>" , spawn "sudo shutdown -h now"       ) -- poweroff
         ]

