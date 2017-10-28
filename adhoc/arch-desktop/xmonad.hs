import System.IO
import Data.Default
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.SetWMName
import XMonad.Layout
import XMonad.Util.EZConfig -- additionalKeys

myLayout = Full ||| Tall 1 (3/100) (1/2)

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ def
        { manageHook = manageDocks <+> manageHook def
        , layoutHook = avoidStruts $ myLayout
        , logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , terminal    = "terminator"
        , modMask     = mod4Mask
        , borderWidth = 1
        , startupHook = setWMName "LG3D"
        }
         `additionalKeys`
        [ ((0, 0x1008ff13), spawn "amixer sset Master 5%+ && notify-send -t 3 '♪▲' \"$(amixer sget Master | grep '%' | sed 's/^ *//g')\"")
        , ((0, 0x1008ff11), spawn "amixer sset Master 5%- && notify-send -t 3 '♪▼' \"$(amixer sget Master | grep '%' | sed 's/^ *//g')\"")
        , ((0, 0x1008ff12), spawn "amixer sset Master toggle && notify-send -t 3 '♪' \"$(amixer sget Master | grep '%' | sed 's/^ *//g')\"")
        ]
