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
import XMonad.Hooks.DynamicLog                  -- Used for dzen statusbar

-- Layout -- ----------------------------------------------------
import XMonad.Layout.HintedGrid as HintedGrid                 -- (3)  grid layout
import XMonad.Layout.GridVariants           -- (4) used for splitgrid
import XMonad.Layout.ResizableTile          -- (5)  resize non-master windows too

-- Actions ---------------------------------------------------
import XMonad.Actions.FindEmptyWorkspace    -- (5) for finding empty workspace and tagging windows to it 
import XMonad.Actions.CopyWindow            -- (6) for closing a window via delete protocol
import XMonad.ManageHook                    -- (7) an EDSL for ManageHook 
import XMonad.Hooks.ManageHelpers           -- (8) some helpers used with ManageHook
import XMonad.Layout.NoBorders              -- (9) used for smarter removing borders i.e mplayer

import XMonad.Util.Run(spawnPipe, hPutStrLn)    -- Used to spawn dzen
import XMonad.Util.Loggers

import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Actions.CopyWindow
import Solarized


myXPConfig = defaultXPConfig

main = do 
    dzenL <- spawnPipe "dzen2 -ta l -xs 1 -bg '#073642' -fg '#839496'"
    dzenR <- spawnPipe "dzen2 -ta l -xs 2 -bg '#073642' -fg '#839496'"
    xmonad $ gnomeConfig 
        { terminal = "urxvt256c"
        , modMask = mod4Mask -- set the mod key to the windows key
        , layoutHook    = myLayoutHook
        , logHook = myLogHook dzenL <+> myLogHook dzenR
        , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
        , startupHook = setWMName "LG3D"
        , normalBorderColor = solarizedBase01
        , focusedBorderColor = solarizedRed
        }
        `additionalKeysP` 
            ([ ("M-c", kill1)                    -- (6)
            , ("M-S-m", tagToEmptyWorkspace)    -- (6)
            , ("M-`", spawn "exe=`urxvt256c -e /bin/zsh`")
            , ("M-a", sendMessage MirrorExpand)                       -- (5)
            , ("M-m", viewEmptyWorkspace)       -- (5)
            , ("M-n", refresh)                  -- (6)
            , ("M-p", spawn "dmenu_run -nb '#073642' -nf '#839496' -sf '#cb4b16'")
            , ("M-z", sendMessage MirrorShrink)                       -- (5)
            , ("M-f", sendMessage $ IncMasterCols 1)
            , ("M-v", sendMessage $ IncMasterCols (-1))
            , ("M-g", sendMessage $ IncMasterRows 1)
            , ("M-b", sendMessage $ IncMasterRows (-1))
            , ("M-<Print>", spawn "gnome-screenshot -i")
            ]
            ++
            [ ("M-" ++ m ++ [k], windows $ f i)
                                            -- this must be changed if workspaces is customized...
                            | (i, k) <- zip (XMonad.workspaces gnomeConfig) (['1' .. '9'] ++ ['0', '-'])
                            , (f, m) <- [ (W.greedyView, "")
                                        , (W.shift, "S-")
                                        , (copy, "C-")
                                        ]
            ])



myLogHook h = dynamicLogWithPP $ myPP h

myManageHook :: [ManageHook] -- (7)
myManageHook = 
    [ resource  =? "Do"   --> doIgnore , -- (8)
      isFullscreen --> doFullFloat -- (8)
    ]

myLayoutHook =  smartBorders (      -- (9)
                    avoidStruts(    -- (2)
                            HintedGrid.Grid False -- (3)
                        ||| tiled
                        ||| Mirror tiled 
                        ||| Full 
                        ||| splitGrid 
                        ))
    where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = ResizableTall 1 (3/100) (5/9) [] -- (5)
     splitGrid = SplitGrid XMonad.Layout.GridVariants.L 2 1 (3/5) (16/9) (5/100) -- (4)

-- some magic which does my the logging in the dzen bar.
myPP h = defaultPP 
        { 
          ppCurrent           =   dzenColor solarizedViolet solarizedBase02  . wrap "<" ">"
        , ppVisible           =   dzenColor solarizedBase0 solarizedBase02 . wrap "<" ">"
        , ppHidden            =   dzenColor solarizedBase00 solarizedBase02 . wrap ("^i(/home/kalkin/dzen_bitmaps/has_win.xbm)") ""
        , ppLayout            =   dzenColor solarizedBase0 solarizedBase02 .
                (\x -> case x of
                "ResizableTall" -> "^i(/home/kalkin/dzen_bitmaps/tall.xbm)"
                "Mirror ResizableTall" -> "^i(/home/kalkin/dzen_bitmaps/mtall.xbm)"
                "Full" -> "^i(/home/kalkin/dzen_bitmaps/full.xbm)"
                "Grid False" -> "^i(/home/kalkin/dzen_bitmaps/grid.xbm)"
                "SplitGrid" -> "S -> ^i(/home/kalkin/dzen_bitmaps/grid.xbm)"

                )
        , ppSep               =  dzenColor solarizedBase01 solarizedBase02 "  |  "
        , ppTitle             =   (" " ++) . dzenColor solarizedViolet solarizedBase02  . dzenEscape
        , ppExtras = [ date "%R %d, %b %Y" ]
        , ppOutput = hPutStrLn h
        }
