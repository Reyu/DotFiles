-- Import stuff
import XMonad
import qualified XMonad.StackSet               as W
import qualified Data.Map                      as M
import System.Exit
import Data.Monoid

-- Utils
import XMonad.Util.Run
import XMonad.Util.NamedWindows (getName)

-- Prompts
import qualified XMonad.Prompt                 as P
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.RunOrRaise
-- import XMonad.Prompt.MPD
-- import qualified Network.MPD                   as MPD

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

-- Layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Layout.Grid

-- Data.Ratio for IM Layout
import Data.Ratio ((%))

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "st"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
myModMask :: KeyMask
myModMask = mod1Mask

------------------------------------------------------------
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
myWorkspaces :: [String]
myWorkspaces = ["1","2","3","4","5","6","7","8","9","10"]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor :: String
myNormalBorderColor  = "#002b36"
myFocusedBorderColor :: String
myFocusedBorderColor = "#657b83"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- Yeganesh as run dialog (L_Alt+r and Win+r)
    {-, ((modm,               xK_r     ), spawn "exe=`yeganesh -x -p exec` && eval \"exec $exe\"")-}

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,              xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm .|. shiftMask, xK_w     ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm .|. shiftMask, xK_v     ), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- , ((modm,               xK_b     ), sendMessage ToggleStruts)

    -- Focus Urgent Window
    , ((modm,               xK_BackSpace), focusUrgent)
    , ((modm .|. shiftMask, xK_BackSpace), clearUrgents)

    -- Type RSA Token
    , ((modm .|. shiftMask, xK_t     ), spawn "/home/reyu/bin/stoken-type")

    -- Launch Firefox
    -- TODO: Set -new-instance to be different key binding
    , ((modm,               xK_u     ), spawn "/usr/bin/firefox -P default -new-instance")
    , ((modm .|. shiftMask, xK_u     ), spawn "/usr/bin/firefox -P Work -new-instance")

    -- Launch Mutt
    , ((modm,               xK_e     ), spawn (myTerminal ++ " -e mutt"))

    -- XMonad Prompt's
    , ((modm,               xK_r     ), runOrRaisePrompt myXPConfig )
    , ((modm,               xK_w     ), windowPromptGoto myXPConfig )
    , ((modm,               xK_n     ), unsafePrompt (myTerminal ++ " -t") myXPConfig )
    , ((modm,               xK_s     ), unsafePrompt (myTerminal ++ " -e") myXPConfig )
    , ((modm .|. shiftMask, xK_s     ), sshPrompt myXPConfig )

    -- MPD Control
    -- , ((modm .|. shiftMask, xK_a     ), addMatching MPD.withMPD myXPConfig [MPD.Artist, MPD.Album] >> return ())
    , ((modm .|. mod4Mask,  xK_m     ), unsafePrompt "/usr/bin/mpc" mpcXPConfig)
    -- , ((modm .|. mod4Mask,  xK_space ), spawn "/usr/bin/mpc toggle")
    -- , ((modm .|. mod4Mask,  xK_plus  ), spawn "/usr/bin/mpc volume +5")
    -- , ((modm .|. mod4Mask,  xK_minus ), spawn "/usr/bin/mpc volume -5")

    , ((modm .|. shiftMask, xK_r      ), unsafePrompt "/home/reyu/.local/ZFunctions/roku" myXPConfig)

    -- XF86 Keys
    , ((0                ,  0x1008FF10), spawn "sudo systemctl suspend")
    , ((0                ,  0x1008FF11), spawn "/usr/bin/pulseaudio-ctl down")
    , ((0                ,  0x1008FF12), spawn "/usr/bin/pulseaudio-ctl mute")
    , ((0                ,  0x1008FF13), spawn "/usr/bin/pulseaudio-ctl up")
    , ((0                ,  0x1008FF14), spawn "/usr/bin/mpc toggle")
    , ((0                ,  0x1008FF16), spawn "/usr/bin/mpc prev")
    , ((0                ,  0x1008FF17), spawn "/usr/bin/mpc next")
    , ((modm             ,  0x1008FF14), spawn "${HOME}/.local/ZFunctions/roku play")
    , ((modm             ,  0x1008FF16), spawn "${HOME}/.local/ZFunctions/roku prev")
    , ((modm             ,  0x1008FF17), spawn "${HOME}/.local/ZFunctions/roku next")

    -- Lock Screen
    , ((modm .|. mod4Mask,  xK_l     ), spawn "/usr/bin/xscreensaver-command -activate")

    -- Quit xmonad
    , ((modm .|. mod4Mask,  xK_q     ), io exitSuccess)

    -- Restart xmonad
    , ((modm,               xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[F1..F10], Switch to workspace N
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
    -- mod-{F11,F12}, Switch to physical/Xinerama screens 1 or 2
    -- mod-shift-{F11,F12}, Move client to screen 1 or 2
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_comma, xK_period] [1,0]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

-- Non-numeric num pad keys, sorted by number
numPadKeys :: [KeySym]
numPadKeys = [ xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 7, 8, 9
             , xK_KP_Insert]                            -- 0


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

-- LayoutHook
myLayoutHook  = avoidStruts $
                smartBorders $
                onWorkspace "9" imLayout
                standardLayouts
    where
        standardLayouts = noBorders Full ||| tiled |||  reflectTiled ||| Mirror tiled ||| Grid

        -- Layouts
        tiled         = ResizableTall 1 (2/100) (1/2) []
        reflectTiled  = reflectHoriz tiled

        -- Im Layout
        imLayout = withIM (1%8) psiRoster $ reflectHoriz $
                   withIM (1%9) skypeRoster chatLayouts

        psiRoster    = ClassName "psi"    `And` Resource "main"
        skypeRoster  = ClassName "Skype"  `And` Not (Role  "ConversationsWindow")
        chatLayouts  = tabbed shrinkText tabConfig ||| Grid ||| tiled

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll . concat $
    [ [resource     =? r        --> doIgnore            |   r    <- myIgnores]
    , [className    =? c        --> doCenterFloat       |   c    <- myFloats ]
    , [name         =? n        --> doCenterFloat       |   n    <- myNames  ]
    , [className    =? c        --> doShift "8"         |   c    <- myVideo  ]
    , [className    =? chat     --> doShift "9"         |   chat <- myChat   ]
    , [isFullscreen             --> myDoFullFloat                           ]
    ]
    where
        -- role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"

        -- classnames
        myFloats  = ["Xmessage"]
        myChat    = ["Tkabber","Chat", "Skype", "psi"]
        myVideo   = ["MPlayer", "xv", "Vlc"]

        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","trayer","panel"]

        -- names
        myNames   = []

        -- a trick for fullscreen but stil allow focusing of other WSs
        myDoFullFloat :: ManageHook
        myDoFullFloat = doF W.focusDown <+> doFullFloat

------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
myEventHook :: Event -> X All
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
--
-- myLogHook h = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn h }
myLogHook :: X ()
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook = setWMName "LG3D"

------------------------------------------------------------------------
-- Set up status bar

-- Command to launch the bar.
myBar :: String
myBar = "dzen2 " ++ myDzenBaseFmt ++ " -w '950' -ta 'left'"

-- Dzen2 Format
myDzenBaseFmt :: String
myDzenBaseFmt = "-x '0' -y '0' -h '16' -xs 1 -fn '-*-terminus-medium-r-*-*-13-*-*-*-*-*-*-*' -bg '#002b36' -fg '#657b83'"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP :: PP
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

-- Key binding to toggle the gap for the bar.
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-----------------------------------------------------------------------
-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig :: Theme
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
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
    { P.font            = "xft:Terminus:pixelsize=14:autohint=true"
    , bgColor           = "#002B36"
    , fgColor           = "#657B83"
    , borderColor       = "#657b83"
    , promptBorderWidth = 1
    }
mpcXPConfig :: XPConfig
mpcXPConfig = myXPConfig

------------------------------------------------------------------------
-- Urgency Hook
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" [show name, "workspace " ++ idx]


------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
main :: IO ()
main = xmonad =<< statusBar myBar myPP toggleStrutsKey (withUrgencyHook LibNotifyUrgencyHook defaults)

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
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
        , manageHook         = myManageHook
        , handleEventHook    = myEventHook
        , logHook            = myLogHook
        , startupHook        = myStartupHook
        }
