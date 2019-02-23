import System.IO
import Data.Default
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Util.EZConfig -- additionalKeys

myLayout = spacing 5 $ gaps[(U, 24 + 5), (D, 5), (L, 5), (R, 5)] $ ThreeColMid 1 (3/100) (1/2) |||  Full ||| Tall 1 (3/100) (1/2)

myManageHookFloat = composeAll [
      className =? "Gimp" --> doFloat,
      title     =? "絵文字の選択" --> doFloat,
      title     =? "Authy" --> doFloat
  ]

myModMask = mod4Mask
altMask   = mod1Mask

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ def
        { manageHook = manageDocks <+> myManageHookFloat <+> manageHook def
        , layoutHook = avoidStruts $ myLayout
        , logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , handleEventHook = fadeWindowsEventHook
        , terminal    = "terminator"
        , modMask     = myModMask
        , borderWidth = 2
        , startupHook = setWMName "LG3D"
        }
        `additionalKeys`
        [
          ((altMask, xK_space), spawn "rofi -combi-modi -font \"hack 10\" -show")
        , ((0, 0x1008ff13), spawn "amixer sset Master 5%+ && notify-send -t 3 '♪▲' \"$(amixer sget Master | grep '%' | sed 's/^ *//g')\"")
        , ((0, 0x1008ff11), spawn "amixer sset Master 5%- && notify-send -t 3 '♪▼' \"$(amixer sget Master | grep '%' | sed 's/^ *//g')\"")
        , ((0, 0x1008ff12), spawn "amixer sset Master toggle && notify-send -t 3 '♪' \"$(amixer sget Master | grep '%' | sed 's/^ *//g')\"")
        ]
