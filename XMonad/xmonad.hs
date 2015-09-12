-- Import stuff
import XMonad
import qualified XMonad.StackSet  as W
import qualified Data.Map         as M
import System.Exit
import Data.Monoid
import Control.Applicative ((<$>))
import XMonad.Actions.Submap
import XMonad.Actions.SpawnOn
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Util.Run
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.Prompt    as P
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.RunOrRaise
import qualified Network.MPD      as MPD
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Layout.Grid
import Data.Ratio ((%))


-- The preferred terminal program
myTerminal = "st"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth = 2

-- Set modMask to left-alt
myModMask = mod4Mask

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

------------------------------------------------------------
-- The default number of workspaces (virtual screens) and their names.
myWorkspaces = show <$> [1..10]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#002b36"
myFocusedBorderColor = "#657b83"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((0                 ,  0x1008FF10  ), spawn "sudo systemctl suspend")
    -- Headphones
    , ((0                 ,  0x1008FF11  ), spawn "amixer set -c 3 PCM 5%-")
    , ((0                 ,  0x1008FF12  ), spawn "amixer set -c 3 PCM toggle")
    , ((0                 ,  0x1008FF13  ), spawn "amixer set -c 3 PCM 5%+")
    -- Speakers
    , ((shiftMask         ,  0x1008FF11  ), spawn "amixer set -c 0 PCM 5%-")
    , ((shiftMask         ,  0x1008FF12  ), spawn "amixer set -c 0 PCM toggle")
    , ((shiftMask         ,  0x1008FF13  ), spawn "amixer set -c 0 PCM 5%+")
    -- , ((0                 ,  0x1008FF14  ), spawn "/usr/bin/mpc -q toggle")
    -- , ((0                 ,  0x1008FF16  ), spawn "/usr/bin/mpc -q prev")
    -- , ((0                 ,  0x1008FF17  ), spawn "/usr/bin/mpc -q next")
    , ((modm              ,  xK_BackSpace), focusUrgent)
    , ((modm .|. shiftMask,  xK_BackSpace), clearUrgents)
    , ((modm              ,  xK_Return   ), windows W.swapMaster)
    , ((modm .|. shiftMask,  xK_Return   ), spawnHere (myTerminal ++ " tmux new -A -s Primary \\; set-option -t Primary destroy-unattached off"))
    , ((modm              ,  xK_Tab      ), windows W.focusDown)
    , ((modm              ,  xK_space    ), sendMessage NextLayout)
    , ((modm .|. shiftMask,  xK_space    ), setLayout $ XMonad.layoutHook conf)
    , ((modm .|. shiftMask,  xK_c        ), kill)
    , ((modm              ,  xK_h        ), sendMessage Shrink)
    , ((modm              ,  xK_j        ), windows W.focusDown)
    , ((modm .|. shiftMask,  xK_j        ), windows W.swapDown  )
    , ((modm              ,  xK_k        ), windows W.focusUp  )
    , ((modm .|. shiftMask,  xK_k        ), windows W.swapUp    )
    , ((modm              ,  xK_l        ), sendMessage Expand)
    , ((modm .|. mod1Mask ,  xK_l        ), spawn "/usr/bin/xscreensaver-command -activate")
    , ((modm              ,  xK_m        ), windows W.focusMaster)
    , ((modm .|. mod1Mask ,  xK_m        ), unsafePrompt "/usr/bin/mpc" mpcXPConfig)
    , ((modm              ,  xK_n        ), unsafePrompt (myTerminal ++ " -t") myXPConfig )
    , ((modm              ,  xK_p        ), spawn "~/bin/passmenu" )
    , ((modm              ,  xK_q        ), spawn "xmonad -- recompile; xmonad -- restart")
    , ((modm .|. shiftMask,  xK_q        ), io exitSuccess)
    , ((modm              ,  xK_r        ), runOrRaisePrompt myXPConfig )
    , ((modm              ,  xK_s        ), unsafePrompt myTerminal myXPConfig )
    , ((modm .|. shiftMask,  xK_s        ), sshPrompt myXPConfig )
    , ((modm              ,  xK_t        ), withFocused $ windows . W.sink)
    , ((modm              ,  xK_u        ), bindOn
        [ ("5", spawnHere   "/usr/bin/firefox -P Work -new-window")
        , ("" , spawnHere "/usr/bin/firefox -P default -new-window")
        ])
    , ((modm .|. shiftMask,  xK_v        ), sendMessage (IncMasterN (-1)))
    , ((modm              ,  xK_w        ), windowPromptGoto myXPConfig )
    , ((modm .|. shiftMask,  xK_w        ), sendMessage (IncMasterN 1))
    ]
    ++

    --
    -- mod-[F1..F10], Switch to workspace N
    -- mod-shift-[F1..F10], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1..xK_F10]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
    ++

    --
    -- mod-{',','.'}, Switch to physical/Xinerama screens 1 or 2
    -- mod-shift-{',','.'}, Move client to screen 1 or 2
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_comma, xK_period] [1,0]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

-- Non-numeric num pad keys, sorted by number
numPadKeys = [ xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 7, 8, 9
             , xK_KP_Insert]                            -- 0


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

------------------------------------------------------------------------
-- Layouts:
--
myLayoutHook  = avoidStruts $
                smartBorders $
                onWorkspace "10" imLayout
                standardLayouts
    where
        standardLayouts = noBorders Full ||| tiled |||  reflectTiled ||| Mirror tiled ||| Grid
        tiled           = ResizableTall 1 (2/100) (1/2) []
        reflectTiled    = reflectHoriz tiled
        imLayout        = withIM (1%8) psiRoster $ reflectHoriz $
                          withIM (1%9) skypeRoster chatLayouts
        chatLayouts     = tabbed shrinkText tabConfig ||| Grid ||| tiled
        psiRoster       = ClassName "psi"    `And` Resource "main"
        skypeRoster     = ClassName "Skype"  `And` Not (Role  "ConversationsWindow")

------------------------------------------------------------------------
-- Window rules:
--
myManageHook = composeAll . concat $
    [ [resource     =? r        --> doIgnore            |   r    <- myIgnores]
    , [className    =? c        --> doCenterFloat       |   c    <- myFloats ]
    , [className    =? c        --> doShift "8"         |   c    <- myVideo  ]
    , [className    =? chat     --> doShift "10"        |   chat <- myChat   ]
    , [isFullscreen             --> myDoFullFloat                           ]
    ]
    where
        name      = stringProperty "WM_NAME"
        -- classnames
        myFloats  = ["Xmessage"]
        myChat    = ["Tkabber","Chat", "Skype", "psi"]
        myVideo   = ["MPlayer", "xv", "Vlc"]
        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","trayer","panel"]
        -- a trick for fullscreen but stil allow focusing of other WSs
        myDoFullFloat :: ManageHook
        myDoFullFloat = doF W.focusDown <+> doFullFloat

------------------------------------------------------------------------
-- Startup hook
--
myStartupHook =
    spawn "tmux new-session -d -s Global weechat \\; set-option -t Global destroy-unattached off"

------------------------------------------------------------------------
-- Set up status bar
--
-- Command to launch the bar.
myBar = "dzen2 " ++ myDzenBaseFmt ++ " -w '1728' -ta 'left'"

-- Dzen2 Format
myDzenBaseFmt = "-x '0' -y '0' -h '16' -xs 1 -fn '-*-terminus-medium-r-*-*-13-*-*-*-*-*-*-*' -bg '#002b36' -fg '#657b83'"

myPP = dzenPP
    { ppCurrent         = dzenColor "#859900" "" . wrap "<" "> "
    , ppVisible         = dzenColor "#2AA198" "" . wrap "[" "] "
    , ppHidden          = dzenColor "#93A1A1" "" . wrap "" " "
    , ppHiddenNoWindows = dzenColor "#586E75" "" . wrap "" " "
    , ppUrgent          = dzenColor "#B58900" "#DC322F" . xmobarStrip
    , ppSep             = dzenColor "#EEE8D5" "" "| "
    , ppLayout          = dzenColor "#839496" "" . wrap "" " "
    , ppTitle           = dzenColor "#839496" ""
    }

-----------------------------------------------------------------------
-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme
    { activeBorderColor   = "#657b83"
    , activeTextColor     = "#859900"
    , activeColor         = "#002b36"
    , inactiveBorderColor = "#002b36"
    , inactiveTextColor   = "#586E75"
    , inactiveColor       = "#002b36"
    }

-----------------------------------------------------------------------
-- Prompt Config
myXPConfig = defaultXPConfig
    { P.font            = "xft:Terminus:pixelsize=14:autohint=true"
    , bgColor           = "#002B36"
    , fgColor           = "#657B83"
    , borderColor       = "#657b83"
    , promptBorderWidth = 1
    }
mpcXPConfig = myXPConfig

------------------------------------------------------------------------
-- Urgency Hook
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- W.findTag w <$> gets windowset
        safeSpawn "notify-send" [show name, "workspace " ++ idx]


------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey (withUrgencyHook LibNotifyUrgencyHook defaults)
defaults = defaultConfig
    { terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , clickJustFocuses   = myClickJustFocuses
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , layoutHook         = myLayoutHook
    , manageHook         = manageSpawn <+> myManageHook
    , startupHook        = myStartupHook
    }
-- vim: set makeprg=xmonad\ --recompile
