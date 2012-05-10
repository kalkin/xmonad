-- default desktop configuration for Fedora

import System.Posix.Env (getEnv)
import Data.Maybe (maybe)

import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome

main = do
     session <- getEnv "DESKTOP_SESSION"
     xmonad  $ maybe desktopConfig desktop session

desktop "gnome" = gnomeConfig
desktop "xmonad-gnome" = gnomeConfig
desktop _ = desktopConfig
