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
import XMonad.Actions.TopicSpace
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.DynamicWorkspaceGroups
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Util.Run
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Loggers
import qualified XMonad.Prompt    as P
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
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
import XMonad.Util.WorkspaceCompare

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
    , workspaces         = myTopicNames host
    , normalBorderColor  = solarized "background"
    , focusedBorderColor = solarized "emphasis"
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook
                           <+> manageSpawn
                           <+> manageDocks
                           <+> manageHook defaultConfig
    , logHook            = myLoghook logPipe host
                           <+> logHook defaultConfig
    , startupHook        = myStartupHook host logPipe
                           <+> startupHook defaultConfig
    } `additionalKeysP` myKeys host logPipe

------------------------------------------------------------------------
-- Usefull common vars
myTerminal = "st"
myShell = "zsh"
speakersAlsaName = "pci-0000_00_1b.0.analog-surround-51"
headphonesAlsaName = "usb-Logitech_Logitech_G930_Headset-00.iec958-stereo"

------------------------------------------------------------------------
-- Helper functions
adjustVolume device value =
    spawn $ "pactl -- set-sink-volume alsa_output." ++ device ++ " " ++ value
adjustMute device value =
    spawn $ "pactl -- set-sink-mute alsa_output." ++ device ++ " " ++ value

spawnShell :: Host -> Maybe String -> X ()
spawnShell host name =
    currentTopicDir (myTopicConfig host) >>= spawnShellIn name

spawnShellIn :: Maybe String -> Dir -> X ()
spawnShellIn Nothing dir =
    spawn $ myTerminal ++ " -e tmux new -Ac '" ++ dir ++ "'"
spawnShellIn (Just name) dir =
    spawn $ myTerminal ++ " -e tmux new -As '" ++ name ++ "' -c '" ++ dir ++ "'"

goto :: Host -> Topic -> X ()
goto host topic = switchTopic (myTopicConfig host) topic

promptedGoto :: Host -> X ()
promptedGoto = workspacePrompt myXPConfig . goto

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

-- Solarized colors
solarized :: String -> String
solarized "base03"       = "#002b36"
solarized "base02"       = "#073642"
solarized "base01"       = "#586e75"
solarized "base00"       = "#657b83"
solarized "base0"        = "#839496"
solarized "base1"        = "#93a1a1"
solarized "base2"        = "#eee8d5"
solarized "base3"        = "#fdf6e3"
solarized "yellow"       = "#b58900"
solarized "orange"       = "#cb4b16"
solarized "red"          = "#dc322f"
solarized "magenta"      = "#d33682"
solarized "violet"       = "#6c71c4"
solarized "blue"         = "#268bd2"
solarized "cyan"         = "#2aa198"
solarized "green"        = "#859900"
solarized "text"         = solarized "base0"
solarized "secondary"    = solarized "base01"
solarized "background"   = solarized "base03"
solarized "bghighlights" = solarized "base02"
solarized "emphasis"     = solarized "base1"
solarized _              = solarized "text" --Use foreground color as default

------------------------------------------------------------------------
-- Topic Spaces
data TopicItem = TI { topicName :: Topic   -- (22b)
                    , topicDir  :: Dir
                    , topicAction :: X ()
                    }
 
-- define some custom topics for use with the TopicSpace module.
myTopics :: Host -> [TopicItem]
myTopics host =
    [ TI "web" "" (spawn "firefox")
    , ti "chat" "" 
    , ti "xmonad" ".config/XMonad"
    ]
    where
        ti t d = TI t d (spawnShell host (Just t))

myTopicNames :: Host -> [Topic]
myTopicNames = map topicName . myTopics

myTopicConfig :: Host -> TopicConfig
myTopicConfig host = defaultTopicConfig
    { topicDirs = M.fromList $ map (\(TI n d _) -> (n,d)) myTopics'
    , defaultTopicAction = const (return ())
    , defaultTopic = "web"
    , maxTopicHistory = 10
    , topicActions = M.fromList $ map (\(TI n _ a) -> (n,a)) myTopics'
    }
    where myTopics' = myTopics host

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
    -- , ("M-p", spawn "~/bin/passmenu" )
    , ("M-q", spawn "xmonad --recompile; xmonad --restart")
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
    ++ -- Window Movement
    [ ("M-a", currentTopicAction (myTopicConfig host))
    , ("M-g", promptedGoto host)
    , ("S-M-g", promptedShift)
    ]
    ++
    [(m ++ "M-" ++ k, f s)
        | (k, s) <- zip [",","."] [0..]
        , (f, m) <- [(viewScreen, ""), (sendToScreen, "S-")]
    ]

------------------------------------------------------------------------
-- Layouts:
myLayoutHook = avoidStrutsOn [U] $
               smartBorders $
               onWorkspace "Chat" imLayout
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
myManageHook = composeAll . concat $
    [ [resource     =? r        --> doIgnore            |   r    <- myIgnores]
    , [className    =? c        --> doCenterFloat       |   c    <- myFloats ]
    , [className    =? chat     --> doShift "Chat"      |   chat <- myChat   ]
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
myStartupHook host logPipe = do
    checkKeymap (myConfig host logPipe) (myKeys host logPipe)
    spawn "tmux new-session -d -s Global weechat \\; set-option -t Global destroy-unattached off"

------------------------------------------------------------------------
-- Set up status bar
myBar = "dzen2" ++ concatMap (" " ++)
    [ "-x '0'"
    , "-y '0'"
    , "-h '16'"
    , "-xs 1"
    , "-fn '-*-terminus-medium-r-*-*-13-*-*-*-*-*-*-*'"
    , "-bg '" ++ solarized "background" ++ "'"
    , "-fg '" ++ solarized "text" ++ "'"
    , "-ta 'center'"
    , "-e 'onstart=lower'"
    ]

myLoghook logPipe host = dynamicLogWithPP $ defaultPP 
    { ppCurrent = dzenColor (solarized "green") ""
    , ppVisible = dzenColor (solarized "cyan") ""
    , ppHidden  = dzenColor (solarized "text") ""
    , ppUrgent  = dzenColor (solarized "yellow") (solarized "red")
    , ppLayout  = dzenColor (solarized "text") ""
    , ppTitle   = shorten 100
    , ppSort    = getSortByXineramaRule
    , ppExtras  = [ date "%a %b %d  %I:%M %p"
                  , loadAvg
                  , maildirNew "~/.maildir"
                  ] ++
                  (case host of Laptop _ -> [battery]
                                Desktop  -> [])
    , ppOrder   = \(ws:l:t:exs) -> [t,l,ws]++exs
    , ppOutput  = hPutStrLn logPipe
    }

-----------------------------------------------------------------------
-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme
    { activeBorderColor   = solarized "emphasis"
    , activeTextColor     = solarized "green"
    , activeColor         = solarized "bghighlights"
    , inactiveBorderColor = solarized "background"
    , inactiveTextColor   = solarized "text"
    , inactiveColor       = solarized "background"
    }

-----------------------------------------------------------------------
-- Prompt Config
myXPConfig = defaultXPConfig
    { P.font            = "xft:Terminus:pixelsize=14:autohint=true"
    , bgColor           = solarized "background"
    , fgColor           = solarized "text"
    , borderColor       = solarized "emphasis"
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
