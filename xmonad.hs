-- Information
-- {{{
--  Last Modified [ 2013-03-01 ]
-- File:    ~/.xmonad/xmonad.hs
-- }}}
--

import XMonad                               -- (0) core xmonad libraries
import XMonad.Config.Gnome
import XMonad.Actions.Plane
import XMonad.Util.EZConfig

-- Hooks -----------------------------------------------------
import XMonad.Hooks.ManageDocks             -- (2)  automatically avoid covering my status bar with windows
import XMonad.Hooks.SetWMName

-- Layout -- ----------------------------------------------------
import XMonad.Layout.HintedGrid as HintedGrid                 -- (3)  grid layout
import XMonad.Layout.GridVariants
import XMonad.Layout.ResizableTile          -- (4)  resize non-master windows too
import XMonad.Layout.ResizableTile          -- (5)  resize non-master windows too

-- Actions ---------------------------------------------------
import XMonad.Actions.FindEmptyWorkspace    -- (6) for finding empty workspace and tagging windows to it 
import XMonad.Actions.CopyWindow            -- (7) for closing a window via delete protocol
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders

import XMonad.Hooks.ManageDocks                 -- Manages the harmonic placement of docs and windows
import XMonad.Hooks.DynamicLog                  -- Used for dzen statusbar
import XMonad.Util.Run(spawnPipe, hPutStrLn)    -- Used to spawn dzen
import XMonad.Util.Loggers

import XMonad.Prompt
import XMonad.Prompt.RunOrRaise


import qualified Data.Map as M


main = do 
    xmproc <- spawnPipe "dzen2 -ta lr"
    xmonad $ gnomeConfig
        { terminal = "gnome-terminal -e 'screen -xRR everday'"
        {-, modMask = mod2Mask -- set the mod key to the windows key-}
        , layoutHook    = smartBorders (myLayoutHook)
        , logHook = dynamicLogWithPP $ myPP xmproc
        , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
        {-, workspaces    = ["www", "work", "chat", "mail", "5", "6", "7", "stat", "dwnl"] -}
        }
        `additionalKeysP` 
            [ ("M-c", kill1)                    -- (7)
            , ("M-S-m", tagToEmptyWorkspace)    -- (7)
            , ("M-`", spawn "exe=`gnome-terminal -e /bin/zsh`")
            , ("M-a", sendMessage MirrorExpand)                       -- (6)
            , ("M-m", viewEmptyWorkspace)       -- (6)
            , ("M-n", refresh)                  -- (7)
            , ("M-p", spawn "dmenu_run")
            , ("M-z", sendMessage MirrorShrink)                       -- (5)
            , ("M-f", sendMessage $ IncMasterCols 1)
            , ("M-v", sendMessage $ IncMasterCols (-1))
            , ("M-g", sendMessage $ IncMasterRows 1)
            , ("M-b", sendMessage $ IncMasterRows (-1))
            ]

myManageHook :: [ManageHook]
myManageHook = 
    [ resource  =? "Do"   --> doIgnore ,
      isFullscreen --> doFullFloat
    ]

myLayoutHook = avoidStruts( -- (2)
                            HintedGrid.Grid False -- (3)
                        ||| tiled   -- (4)
                        ||| Mirror tiled 
                        ||| Full 
                        ||| SplitGrid XMonad.Layout.GridVariants.L 2 1 (3/5) (16/9) (5/100)
                        )  
    where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = ResizableTall 1 delta ratio []
 
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 5/9
 
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

startupHook = setWMName "LG3D" -- For Java


-- some magic which does my the logging in the dzen bar.
myPP h = defaultPP 
        { ppCurrent           =   dzenColor "#ebac54" "#1B1D1E" . pad
        , ppLayout            =   dzenColor "#ebac54" "#1B1D1E" .
                (\x -> case x of
                "ResizableTall" -> "^i(/home/kalkin/dzen_bitmaps/tall.xbm)"
                "Mirror ResizableTall" -> "^i(/home/kalkin/dzen_bitmaps/mtall.xbm)"
                "Full" -> "^i(/home/kalkin/dzen_bitmaps/full.xbm)"
                "Grid False" -> "^i(/home/kalkin/dzen_bitmaps/grid.xbm)"
                "SplitGrid" -> "S -> ^i(/home/kalkin/dzen_bitmaps/grid.xbm)"

                )
        , ppSep               =   "  |  "
        , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
        , ppExtras = [ date "%d.%m.%Y %R" ]
        , ppOutput   = hPutStrLn h
        }
