import XMonad.Layout.SimplestFloat
import XMonad

myTerminal		= "terminator"
myModMask			= mod4Mask -- Win key or Super_L
myBorderWidth = 5
myLayout = simplestFloat

main = do
	xmonad $ defaultConfig
		{ terminal		= myTerminal
		, modMask			= myModMask
		, borderWidth = myBorderWidth
		, layoutHook  = myLayout
		}

