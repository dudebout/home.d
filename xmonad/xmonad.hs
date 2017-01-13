module Main
  ( main
  ) where

import qualified XMonad.StackSet             as W

import           Data.List                   (intercalate)
import           System.Environment          (getEnv)

import           XMonad                      (XConfig (..), className,
                                              defaultConfig, mod4Mask, spawn,
                                              title, windows, withFocused,
                                              xmonad, (<+>), (=?))
import           XMonad.Actions.WindowGo     (raise, runOrRaise)
import           XMonad.Hooks.EwmhDesktops   (ewmh)
import           XMonad.Hooks.ManageHelpers  (doCenterFloat)
import           XMonad.Util.EZConfig        (additionalKeysP)
import           XMonad.Util.NamedScratchpad (NamedScratchpad (NS),
                                              namedScratchpadAction,
                                              namedScratchpadManageHook)

main :: IO ()
main = do
  -- FIXME put this in generated
  normalColor <- getEnv "ZENBURN_BG__M__05"
  focusedColor <- getEnv "ZENBURN_FG__M__1"
  xmonad $ ewmh $ defaultConfig { modMask = mod4Mask
                                , normalBorderColor = normalColor
                                , focusedBorderColor = focusedColor
                                , manageHook = namedScratchpadManageHook scratchpads <+> manageHook defaultConfig
                                } `additionalKeysP` [ ("M-s", saneNSAction scratchpads nsScratch)
                                                    , ("M-u", withFocused (windows . W.sink))
                                                    , ("M-t", runOrRaise "xt" (title =? "tmux:default"))
                                                    , ("M-e", runOrRaise "e" (title =? "emacs_X_frame"))
                                                    , ("M-i", runOrRaise "firefox" (className =? "Firefox"))
                                                    , ("M-a", runOrRaise "slack" (className =? "Slack"))
                                                    , ("M-n", runOrRaise "nautilus" (className =? "Nautilus"))
                                                    , ("M-q", spawn "PATH=$HOME_D_XMONAD_PATH $HOME_D_XMONAD --recompile && $HOME_D_XMONAD --restart")
                                                    -- FIXME put this in profile
                                                    , ("M-l", spawn "sudo systemctl suspend")
                                                    , ("M-S-x", spawn "xkill")
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
