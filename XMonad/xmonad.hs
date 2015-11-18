-- Import stuff
import XMonad
import qualified XMonad.StackSet  as W
import qualified Data.Map         as M
import System.Exit
import Data.Monoid
import Control.Applicative ((<$>))
import XMonad.Actions.SpawnOn
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.TopicSpace
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.DynamicWorkspaceGroups
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Actions.CycleWS
import XMonad.Actions.WithAll
import XMonad.Util.Run
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Workspace
import XMonad.Prompt.RunOrRaise
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import qualified XMonad.Layout.Magnifier as Mag
import Data.Ratio ((%))
import System.Posix.Unistd
import XMonad.Util.EZConfig
import XMonad.Util.WorkspaceCompare

type HasWinKey = Bool
type IsRetina = Bool
data Host = Desktop | Laptop HasWinKey IsRetina
    deriving (Eq, Show, Read)

getHost :: IO Host
getHost = do
    hostName <- nodeName `fmap` getSystemID
    return $ case hostName of
        "renard" -> Desktop
        "vulpie" -> Desktop
        "crevan" -> Laptop True True
        _        -> Desktop

main = do
    host <- getHost
    logPipe <- spawnPipe (myBar host)
    checkTopicConfig (myTopicNames host) (myTopicConfig host)
    xmonad $ myConfig host logPipe

myConfig host logPipe = defaultConfig
    { terminal           = myTerminal
    , focusFollowsMouse  = False
    , modMask            = case host of
                               Laptop False _ -> modMask defaultConfig
                               otherwise      -> mod4Mask
    , workspaces         = myTopicNames host
    , normalBorderColor  = solarized "secondary"
    , focusedBorderColor = solarized "emphasis"
    , borderWidth        = 2
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook
                           <+> manageSpawn
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
goto host = switchTopic (myTopicConfig host)

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
    , ti "irc" "" 
    , TI "skype" "" (spawn "skype")
    , ti "xmonad" ".config/XMonad"
    , TI "games" "" (spawn "steam")
    , ti "kernel" "/usr/src/linux"
    , TI "piops" "Projects/PIOPS" (
        spawn "firefox -new-window http://tkts.sys.comcast.net/secure/Dashboard.jspa"
        >> spawnShell host (Just "Projects/PIOPS"))
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
-- Scratchpads

-- XXX offset scratchpad windows by a bit --- each one different?
mySPFloat = customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)
mySPLargeFloat = customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4)

scratchpads =
    [ NS "htop" (myTerminal ++ " -e htop") (title =? "htop") mySPFloat
    , NS "ghci" (myTerminal ++ " -e ghci") (title =? "ghci") mySPFloat
    , NS "mail" (myTerminal ++ " -e mutt") (title =? "mutt") mySPLargeFloat
    ]

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
    , ("M-q", spawn "xmonad --recompile; xmonad --restart")
    , ("M-S-q", io exitSuccess)
    , ("M-t", withFocused $ windows . W.sink)
    , ("M-u", spawnHere "/usr/bin/firefox -P default -new-window")
    , ("<Print>", spawn "scrot")
    , ("C-<Print>", spawn "sleep 0.2; scrot -s")
    -- Scratchpads
    , ("M-s t", namedScratchpadAction scratchpads "htop")
    , ("M-s g", namedScratchpadAction scratchpads "ghci")
    , ("M-s m", namedScratchpadAction scratchpads "mail")
    -- Various Prompts
    , ("M-p p", spawn "~/bin/passmenu" )
    , ("M-p r", runOrRaisePrompt myXPConfig)
    , ("M-p e", spawn "exe=`echo | yeganesh -x` && eval \"exec $exe\"") 
    , ("M-p s", sshPrompt myXPConfig )
    , ("M-p m", manPrompt myXPConfig)
    , ("M-p n", appendFilePrompt myXPConfig "Notes")
    , ("M-p C-n", spawn "echo '' >> Notes && date >> Notes" >>
        appendFilePrompt myXPConfig "Notes")
    -- Dynamic Workspaces
    , ("M-w n", addWorkspacePrompt myXPConfig)
    , ("M-w S-n", renameWorkspace myXPConfig)
    , ("M-w C-c", removeWorkspace)
    , ("M-w C-k", killAll >> removeWorkspace) 
    -- Workspace Groups
    , ("M-y n", promptWSGroupAdd myXPConfig "Name this group: ")
    , ("M-y g", promptWSGroupView myXPConfig "Go to group: " >> viewScreen 1)
    , ("M-y d", promptWSGroupForget myXPConfig "Forget group: ")
    -- Topic actions
    , ("M-a", currentTopicAction (myTopicConfig host))
    -- Window Movement
    , ("M-g", promptedGoto host)
    , ("M-S-g", promptedShift)
    , ("M-z", toggleWS)
    , ("M-;", nextScreen)
    , ("M-S-;", swapNextScreen)
    -- toggles: fullscreen, flip x, flip y, mirror, no borders
    , ("M-C-<Space>", sendMessage $ Toggle NBFULL)
    , ("M-C-x", sendMessage $ Toggle REFLECTX)
    , ("M-C-y", sendMessage $ Toggle REFLECTY)
    , ("M-C-m", sendMessage $ Toggle MIRROR)
    , ("M-C-b", sendMessage $ Toggle NOBORDERS)
    ]
    ++ -- Move or shift windows between screens
    [(m ++ "M-" ++ k, f s)
        | (k, s) <- zip [",","."] [0..]
        , (f, m) <- [(viewScreen, ""), (sendToScreen, "S-")]
    ]

------------------------------------------------------------------------
-- Layouts:
myLayoutHook =
    avoidStrutsOn [U] $
    mkToggle1 NBFULL $
    mkToggle1 REFLECTX $
    mkToggle1 REFLECTY $
    mkToggle1 NOBORDERS $
    mkToggle1 MIRROR $
    smartBorders $
    onWorkspaces ["web","irc"] (Full ||| tiled) $
    onWorkspace "skype" (withIM (1%9) skypeRoster skypeLayout) $
    tiled ||| Mag.magnifier Grid ||| TwoPane (2/100) (1/2) ||| Full
    where
        tiled       = ResizableTall 1 (2/100) (1/2) []
        skypeLayout = tabbed shrinkText tabConfig ||| Mag.magnifier Grid ||| tiled
        skypeRoster = Title "reyuzenfold - Skype™"
            -- The title is the ONLY property that changes between windows... WTF

------------------------------------------------------------------------
-- Window rules:
myManageHook = composeAll $
    [resource =? r --> doIgnore | r <- myIgnores ]
    ++
    [ className =? "Xmessage" --> doCenterFloat
    , className =? "Skype" --> doShift "skype"
    , isFullscreen --> myDoFullFloat
    , manageDocks
    , namedScratchpadManageHook scratchpads
    ]
    where
        name = stringProperty "WM_NAME"
        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","trayer","panel"]
        -- a trick for fullscreen but stil allow focusing of other WSs
        myDoFullFloat :: ManageHook
        myDoFullFloat = doF W.focusDown <+> doFullFloat

------------------------------------------------------------------------
-- Startup hook
myStartupHook host logPipe = 
    checkKeymap (myConfig host logPipe) (myKeys host logPipe)

------------------------------------------------------------------------
-- Set up status bar
myBar host = "dzen2" ++ concatMap (" " ++)
    [ "-x '0'"
    , "-y '0'"
    , "-h '16'"
    , "-xs 1"
    , case host of
          Laptop _ True -> "-fn '-*-terminus-medium-r-*-*-20-*-*-*-*-*-*-*'"
          otherwise     -> "-fn '-*-terminus-medium-r-*-*-13-*-*-*-*-*-*-*'"
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
    , ppSort    = fmap (namedScratchpadFilterOutWorkspace.) DO.getSortByOrder
    , ppExtras  = [ date "%a %b %d  %I:%M %p"
                  , loadAvg
                  , dzenColorL (solarized "green") "" $
                      wrapL "Inbox: " "" $
                      maildirNew ".maildir/Inbox"
                  , dzenColorL (solarized "red") "" $
                      wrapL "CNOC: " "" $
                      maildirNew ".maildir/CNOC"
                  ] ++
                  (case host of Laptop _ _ -> [battery]
                                Desktop    -> [])
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
    { font            = "xft:Terminus:pixelsize=16:autohint=true"
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
