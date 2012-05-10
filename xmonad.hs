-- Information
-- {{{
--  Last Modified [ 2012-05-10 ]
-- File:    ~/.xmonad/xmonad.hs
-- Author:  kalkin-
-- Purpose  config file for the xmonad window manager
-- Notices: This file is actually kind of a haskell programm. So it's really
--          helpfull if you understand the basics of functional programming and
--          haskell.
--
-- Documentation: http://xmonad.org/documentation.html
--
-- For more documentation about the used Extensions (the import stuff) look at
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Doc-Extending.html
--
-- Powered by: A pack of Lucky Strike Click & Roll¹, boredom and Fedora!
-- }}}
--

import XMonad                               -- (0) core xmonad libraries
import XMonad.Config.Gnome
import XMonad.Actions.Plane
import XMonad.Util.EZConfig

-- Hooks -----------------------------------------------------
import XMonad.Hooks.ManageDocks             -- (2)  automatically avoid covering my status bar with windows

-- Layout -- ----------------------------------------------------
import XMonad.Layout.Grid                   -- (3)  grid layout
import XMonad.Layout.ResizableTile          -- (4)  resize non-master windows too
import XMonad.Layout.ResizableTile          -- (5)  resize non-master windows too

-- Actions ---------------------------------------------------
import XMonad.Actions.FindEmptyWorkspace    -- (6) for finding empty workspace and tagging windows to it 
import XMonad.Actions.CopyWindow            -- (7) for closing a window via delete protocol
{-import XMonad.Actions.SpawnOn               -- (7) start programs on a particular WS-}
{-import XMonad.Actions.TopicSpace            -- (7b) set a "topic" for each workspace-}

import qualified Data.Map as M
main = xmonad $ gnomeConfig
    { terminal = "gnome-terminal -e 'screen -xRR everday'"
    , modMask = mod4Mask -- set the mod key to the windows key
    , layoutHook    = myLayoutHook
    , workspaces    = ["www", "work", "chat", "mail", "5", "6", "7", "stat", "dwnl"] 
    }
    `additionalKeysP` 
        [ ("M-c", kill1)                    -- (7)
        , ("M-n", refresh)                  -- (7)
        , ("M-m", viewEmptyWorkspace)       -- (6)
        , ("M-S-m", tagToEmptyWorkspace)    -- (7)
        , ("M-a", sendMessage MirrorExpand)                       -- (6)
        , ("M-z", sendMessage MirrorShrink)                       -- (6)
        ]
    `additionalKeys`
    -- NOTE: planeKeys requires xmonad-0.9 or greater
    M.toList (planeKeys mod4Mask GConf Finite)



myLayoutHook = avoidStruts(Grid ||| tiled ||| Mirror tiled ||| Full)  -- (2) & (3) & (4)
    where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = ResizableTall 1 delta ratio []
 
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 5/9
 
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100


-- ¹ Smoking is seriosly enjoyed by me and others
