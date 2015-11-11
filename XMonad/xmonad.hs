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
import XMonad.Actions.PhysicalScreens
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
import System.Posix.Unistd
import XMonad.Util.EZConfig

data Host = Desktop | Laptop Bool -- ^ Does the laptop have a Windows key?
    deriving (Eq, Show, Read)

getHost :: IO Host
getHost = do
    hostName <- nodeName `fmap` getSystemID
    return $ case hostName of
        "renard" -> Desktop
        "vulpie" -> Desktop
        "crevan" -> Laptop True
        _        -> Desktop

main = do
    host <- getHost
    logPipe <- spawnPipe myBar
    xmonad $ myConfig host logPipe

myConfig host logPipe = defaultConfig
    { terminal           = myTerminal
    , focusFollowsMouse  = False
    , borderWidth        = 2
    , modMask            = if host == Laptop False
                              then modMask defaultConfig
                              else mod4Mask
    , workspaces         = myWorkspaces
    , normalBorderColor  = solarizedBackground
    , focusedBorderColor = solarizedForeground
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook
                           <+> manageSpawn
                           <+> manageDocks
                           <+> manageHook defaultConfig
    , logHook            = myLoghook logPipe
                           <+> logHook defaultConfig
    , startupHook        = myStartupHook host logPipe
                           <+> startupHook defaultConfig
    } `additionalKeysP` myKeys host logPipe

solarizedForeground = "#002b36"
solarizedBackground = "#657b83"
myTerminal = "st"

-- Key binding to toggle the gap for the bar.
-- toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

------------------------------------------------------------
-- The default number of workspaces (virtual screens) and their names.
myWorkspaces = show <$> [1..10]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = solarizedBackground
myFocusedBorderColor = solarizedForeground

speakersAlsaName = "pci-0000_00_1b.0.analog-surround-51"
headphonesAlsaName = "usb-Logitech_Logitech_G930_Headset-00.iec958-stereo"

adjustVolume device value = spawn $ "pactl -- set-sink-volume alsa_output." ++ device ++ " " ++ value
adjustMute device value = spawn $ "pactl -- set-sink-mute alsa_output." ++ device ++ " " ++ value

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
myKeys host logPipe = myKeymap host (myConfig host logPipe)
myKeymap host conf =
    [
    -- Volume: Headphones
      ("<XF86AudioLowerVolume>", adjustVolume headphonesAlsaName "5%-")
    , ("<XF86AudioMute>", adjustMute headphonesAlsaName "toggle")
    , ("<XF86AudioRaiseVolume>", adjustVolume headphonesAlsaName "5%+")
    -- Volume: Speakers
    , ("S-<XF86AudioLowerVolume>", adjustVolume speakersAlsaName "5%-")
    , ("S-<XF86AudioMute>", adjustMute speakersAlsaName "toggle")
    , ("S-<XF86AudioRaiseVolume>", adjustVolume speakersAlsaName "5%+")
    -- General
    , ("M-<Backspace>", focusUrgent)
    , ("M-S-<Backspace>", clearUrgents)
    , ("M-<Return>", windows W.swapMaster)
    , ("M-S-<Return>", spawnHere (myTerminal ++ " -e tmux"))
    , ("M-<Space>", sendMessage NextLayout)
    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)
    , ("M-M1-l", spawn "/usr/bin/xscreensaver-command -activate")
    , ("M-m", windows W.focusMaster)
    , ("M-n", unsafePrompt (myTerminal ++ " -t") myXPConfig )
    , ("M-p", spawn "~/bin/passmenu" )
    , ("M-q", spawn "xmonad -- recompile; xmonad -- restart")
    , ("M-S-q", io exitSuccess)
    , ("M-r", runOrRaisePrompt myXPConfig )
    , ("M-s", unsafePrompt myTerminal myXPConfig )
    , ("M-S-s", sshPrompt myXPConfig )
    , ("M-t", withFocused $ windows . W.sink)
    , ("M-u", spawnHere "/usr/bin/firefox -P default -new-window")
    , ("M-S-v", sendMessage (IncMasterN (-1)))
    , ("M-w", windowPromptGoto myXPConfig )
    , ("M-S-w", sendMessage (IncMasterN 1))
    ]
    ++
    [(m ++ "M-" ++ k, windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ["<F" ++ show i ++ ">" | i <- [1..]]
        , (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]
    ]
    ++
    [(m ++ "M-" ++ k, f s)
        | (k, s) <- zip [",","."] [0..]
        , (f, m) <- [(viewScreen, ""), (sendToScreen, "S-")]
    ]

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
myLayoutHook = avoidStrutsOn [U] $
               smartBorders $
               onWorkspace "10" imLayout
               standardLayouts
    where
        standardLayouts = noBorders Full ||| tiled |||  reflectTiled ||| Mirror tiled ||| Grid
        tiled           = ResizableTall 1 (2/100) (1/2) []
        reflectTiled    = reflectHoriz tiled
        imLayout        = withIM (1%9) skypeRoster chatLayouts
        chatLayouts     = tabbed shrinkText tabConfig ||| Grid ||| tiled
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
myStartupHook host logPipe = do
    checkKeymap (myConfig host logPipe) (myKeys host logPipe)
    spawn "tmux new-session -d -s Global weechat \\; set-option -t Global destroy-unattached off"

------------------------------------------------------------------------
-- Set up status bar
--
-- Command to launch the bar.
myBar = "dzen2" ++ concatMap (" " ++)
    [ "-x '0'"
    , "-y '0'"
    , "-h '16'"
    , "-xs 1"
    , "-fn '-*-terminus-medium-r-*-*-13-*-*-*-*-*-*-*'"
    , "-bg '#002b36'"
    , "-fg '#657b83'"
    , "-ta 'left'"
    , "-e 'onstart=lower'"
    ]

myLoghook h = dynamicLogWithPP $ defaultPP 
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
