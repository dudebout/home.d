module Main
  ( main
  ) where

import System.Environment (getEnv)

import XMonad                      (XConfig (..), className, defaultConfig,
                                    mod4Mask, spawn, title, xmonad, (<+>), (=?))
import XMonad.Actions.WindowGo     (raise, runOrRaise)
import XMonad.Hooks.EwmhDesktops   (ewmh)
import XMonad.Hooks.ManageHelpers  (doCenterFloat)
import XMonad.Util.EZConfig        (additionalKeysP)
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), namedScratchpadAction,
                                    namedScratchpadManageHook)

main :: IO ()
main = do
  normalColor <- getEnv "ZENBURN_BG__M__05"
  focusedColor <- getEnv "ZENBURN_FG__M__1"
  xmonad $ ewmh $ defaultConfig { modMask = mod4Mask
                                , focusFollowsMouse = False
                                , normalBorderColor = normalColor
                                , focusedBorderColor = focusedColor
                                , manageHook = namedScratchpadManageHook scratchpads <+> manageHook defaultConfig
                                } `additionalKeysP` [ ("M-s", saneNSAction scratchpads nsTmux)
                                                    , ("M-e", runOrRaise "e" (title =? "emacs_X_frame"))
                                                    , ("M-i", runOrRaise "firefox" (className =? "Firefox"))
                                                    ]
  where nsTmux = "NS:tmux"
        scratch = "scratch"
        scratchpads = [ NS nsTmux
                           ("xterm -T " ++ nsTmux ++ " -e zsh -c 'tmux attach -t " ++ scratch ++ " || tmux new-session -s " ++ scratch ++ "'")
                           (title =? nsTmux)
                           doCenterFloat
                      ]
        -- FIXME add a way to reselect the scratchpad even if it is on the current workspace
        saneNSAction = namedScratchpadAction
