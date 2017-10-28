import System.IO
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
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
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
        [ ((0, 0x1008ff13), spawn "amixer sset Master 10%+ && notify-send (amixer sget Master | grep '%') ")
        , ((0, 0x1008ff11), spawn "amixer sset Master 10%- && notify-send (amixer sget Master | grep '%') ")
        ]

