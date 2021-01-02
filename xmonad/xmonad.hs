module Main
  ( main
  ) where

import qualified XMonad.StackSet as W

import Data.Default       (def)
import Data.List          (intercalate)
import System.Environment (getEnv)

import XMonad                      (Resize (Expand, Shrink), XConfig (..),
                                    className, mod4Mask, sendMessage, spawn,
                                    title, windows, withFocused, xmonad, (<+>),
                                    (=?))
import XMonad.Actions.WindowGo     (raise, runOrRaise)
import XMonad.Hooks.EwmhDesktops   (ewmh)
import XMonad.Hooks.ManageDocks    (docks, avoidStruts)
import XMonad.Hooks.ManageHelpers  (doCenterFloat)
import XMonad.Layout.NoBorders     (smartBorders)
import XMonad.Util.EZConfig        (additionalKeysP)
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), namedScratchpadAction,
                                    namedScratchpadManageHook)

main :: IO ()
main = do
  -- FIXME put this in generated
  normalColor <- getEnv "ZENBURN_BG__M__05"
  focusedColor <- getEnv "ZENBURN_FG__M__1"
  xmonad $ docks . ewmh $
    def { modMask = mod4Mask
        , normalBorderColor = normalColor
        , focusedBorderColor = focusedColor
        , manageHook = namedScratchpadManageHook scratchpads <+> manageHook def
        , layoutHook = avoidStruts $ smartBorders (layoutHook def)
        } `additionalKeysP` [ ("M-s", saneNSAction scratchpads nsScratch)
                            , ("M-u", withFocused (windows . W.sink))
                            , ("M-t", runOrRaise "xt" (title =? "tmux:default"))
                            , ("M-e", runOrRaise "emacs-attach" (className =? "Emacs"))
                            , ("M-i", runOrRaise "firefox" (className =? "Firefox"))
                            , ("M-a", runOrRaise "slack" (className =? "Slack"))
                            , ("M-p", spawn "rofi -show run")
                            , ("M-n", runOrRaise "nautilus" (className =? "Nautilus"))
                            , ("M-q", spawn "polybar-msg cmd quit; polybar --config=$HOME_D/xmonad/polybar.ini --reload top & disown; xmonad --recompile && xmonad --restart")
                            , ("M-S-l", spawn "xset s activate")
                            , ("M-S-x", spawn "xkill")
                            , ("M-<Right>", sendMessage Expand)
                            , ("M-<Left>", sendMessage Shrink)
                            , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
                            , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
                            , ("S-<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
                            , ("S-<XF86MonBrightnessUp>", spawn "xbacklight -inc 5")
                            , ("C-<XF86MonBrightnessDown>", spawn "xbacklight -dec 1")
                            , ("C-<XF86MonBrightnessUp>", spawn "xbacklight -inc 1")
                            , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 10%+")
                            , ("<XF86AudioLowerVolume>", spawn "amixer set Master 10%-")
                            , ("S-<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
                            , ("S-<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
                            , ("C-<XF86AudioRaiseVolume>", spawn "amixer set Master 1%+")
                            , ("C-<XF86AudioLowerVolume>", spawn "amixer set Master 1%-")
                            , ("M-<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
                            , ("M-<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
                            , ("<XF86AudioMute>", spawn "amixer set Master toggle")
                            ]
  where sessionName = "scratch"
        xtermTitle = "NS:tmux:" ++ sessionName
        nsScratch = "NS:" ++ sessionName
        scratchpads = [ NS nsScratch (intercalate " " ["xt", sessionName, xtermTitle])
                           (title =? xtermTitle)
                           doCenterFloat
                      ]
        -- FIXME add a way to reselect the scratchpad even if it is on the current workspace
        saneNSAction = namedScratchpadAction
