module Main
  ( main,
  )
where

import Data.List (intercalate)
import System.Environment (getEnv)
import XMonad
  ( Resize (Expand, Shrink),
    XConfig (..),
    className,
    def,
    mod4Mask,
    sendMessage,
    spawn,
    title,
    windows,
    withFocused,
    xmonad,
    (<+>),
    (=?),
  )
import XMonad.Actions.WindowGo (raise, runOrRaise)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.ManageHook (doIgnore, (-->))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    namedScratchpadAction,
    namedScratchpadManageHook,
  )

data Terminal = Alacritty | XTerm

main :: IO ()
main = do
  -- FIXME put this in generated
  normalColor <- getEnv "ZENBURN_BG__M__05"
  focusedColor <- getEnv "ZENBURN_FG__M__1"
  xmonad $
    docks . ewmh $
      def
        { modMask = mod4Mask,
          normalBorderColor = normalColor,
          focusedBorderColor = focusedColor,
          terminal = terminalName,
          manageHook =
            mconcat
              [ className =? "Logic" --> doIgnore, -- https://support.saleae.com/faq/technical-faq/xmonad-on-linux-causes-issues
                namedScratchpadManageHook scratchpads,
                manageHook def
              ],
          layoutHook = avoidStruts $ smartBorders (layoutHook def)
        }
        `additionalKeysP` [ ("M-s", saneNSAction scratchpads nsScratch),
                            ("M-u", withFocused (windows . W.sink)),
                            ("M-t", runOrRaise terminalFinsLauncher (title =? "tmux:default")),
                            ("M-e", runOrRaise "emacs-attach" (className =? "Emacs")),
                            ("M-i", runOrRaise "firefox" (className =? "firefox")),
                            ("M-a", runOrRaise "slack" (className =? "Slack")),
                            ("M-p", spawn "rofi -show run"),
                            ("M-n", runOrRaise "nautilus" (className =? "Nautilus")),
                            ("M-q", spawn "polybar-msg cmd quit; polybar --config=$HOME_D/xmonad/polybar.ini --reload top & disown; xmonad --recompile && xmonad --restart"),
                            ("M-S-l", spawn "xset s activate"),
                            ("M-S-x", spawn "xkill"),
                            ("M-<Right>", sendMessage Expand),
                            ("M-<Left>", sendMessage Shrink),
                            ("<XF86MonBrightnessDown>", spawn "light -U 5"),
                            ("<XF86MonBrightnessUp>", spawn "light -A 5"),
                            ("S-<XF86MonBrightnessDown>", spawn "light -U 2"),
                            ("S-<XF86MonBrightnessUp>", spawn "light -A 2"),
                            ("C-<XF86MonBrightnessDown>", spawn "light -U 1"),
                            ("C-<XF86MonBrightnessUp>", spawn "light -A 1"),
                            ("<XF86AudioRaiseVolume>", spawn "amixer set Master 10%+"),
                            ("<XF86AudioLowerVolume>", spawn "amixer set Master 10%-"),
                            ("S-<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+"),
                            ("S-<XF86AudioLowerVolume>", spawn "amixer set Master 5%-"),
                            ("C-<XF86AudioRaiseVolume>", spawn "amixer set Master 1%+"),
                            ("C-<XF86AudioLowerVolume>", spawn "amixer set Master 1%-"),
                            ("M-<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%"),
                            ("M-<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%"),
                            ("<XF86AudioMute>", spawn "amixer set Master toggle")
                          ]
  where
    terminal = Alacritty
    terminalName = case terminal of
      Alacritty -> "alacritty"
      XTerm -> "xterm"
    terminalFinsLauncher = case terminal of
      Alacritty -> "at"
      XTerm -> "xt"
    sessionName = "scratch"
    termTitle = "NS:tmux:" ++ sessionName
    nsScratch = "NS:" ++ sessionName
    scratchpads =
      [ NS
          nsScratch
          (intercalate " " [terminalFinsLauncher, sessionName, termTitle])
          (title =? termTitle)
          doCenterFloat
      ]
    -- FIXME add a way to reselect the scratchpad even if it is on the current workspace
    saneNSAction = namedScratchpadAction
