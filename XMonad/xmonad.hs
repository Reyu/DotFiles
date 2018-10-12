-- Import stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import XMonad.Actions.SpawnOn
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.TopicSpace
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.DynamicWorkspaceGroups
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Actions.CycleWS
import XMonad.Actions.WithAll
import XMonad.Actions.FloatKeys
import XMonad.Config.Desktop
import XMonad.Util.Run
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh
import XMonad.Prompt.Shell
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
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import qualified XMonad.Layout.Magnifier as Mag
import qualified XMonad.Layout.GridVariants as G
import Data.Ratio ((%))
import System.Posix.Unistd
import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops

type HasWinKey = Bool

type IsRetina = Bool

type StatusBarDisplay = Int

data AudioSystem
  = Alsa
  | Pulse
  deriving (Eq)

data Host
  = Desktop StatusBarDisplay
            AudioSystem
  | Laptop HasWinKey
           IsRetina
           AudioSystem
  deriving (Eq)

getHost :: IO Host
getHost = do
  hostName <- nodeName `fmap` getSystemID
  return $
    case hostName of
      "renard" -> Desktop 3 Pulse
      "vulpie" -> Desktop 3 Pulse
      "crevan" -> Laptop True True Pulse
      _ -> Desktop (-1) Pulse

getAudioSystem :: Host -> AudioSystem
getAudioSystem host =
  case host of
    Desktop _ as -> as
    Laptop _ _ as -> as

main :: IO ()
main = do
  host <- getHost
  logPipe <- spawnPipe (myBar host False)
  _ <- spawnPipe (conkyCommand ++ myBar host True)
  checkTopicConfig (myTopicNames host) (myTopicConfig host)
  xmonad $ ewmh $ myConfig host logPipe
  where
    conkyCommand = "pkill conky;conky -c ~/.xmonad/conky_statusbar|"

myConfig host logPipe =
        desktopConfig
        { terminal = myTerminal
        , focusFollowsMouse = False
        , modMask =
            case host of
                Laptop False _ _ -> modMask desktopConfig
                _ -> mod4Mask
        , workspaces = myTopicNames host
        , normalBorderColor = solarized "secondary"
        , focusedBorderColor = solarized "emphasis"
        , borderWidth = 2
        , layoutHook = myLayoutHook
        , manageHook = myManageHook host <+> manageSpawn <+> manageHook desktopConfig
        , logHook = myLoghook logPipe host <+> logHook desktopConfig
        , startupHook = myStartupHook host logPipe <+> startupHook desktopConfig
        , handleEventHook = docksEventHook <+> handleEventHook desktopConfig
        } `additionalKeysP`
        myKeys host logPipe

------------------------------------------------------------------------
-- Usefull common vars
myTerminal
  :: String
myTerminal = "st"

------------------------------------------------------------------------
-- Helper functions
spawnShell
  :: Host -> Maybe String -> X ()
spawnShell host name' =
  currentTopicDir (myTopicConfig host) >>= spawnShellIn name'

spawnShellIn :: Maybe String -> Dir -> X ()
spawnShellIn Nothing dir =
  spawn $ myTerminal ++ " -e tmux new -Ac '" ++ dir ++ "'"
spawnShellIn (Just name') dir =
  spawn $ myTerminal ++ " -e tmux new -As '" ++ name' ++ "' -c '" ++ dir ++ "'"

goto :: Host -> Topic -> X ()
goto host = switchTopic (myTopicConfig host)

promptedGoto :: Host -> X ()
promptedGoto = workspacePrompt myXPConfig . goto

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

-- Solarized colors
solarized
  :: String -> String
solarized "base03" = "#002b36"
solarized "base02" = "#073642"
solarized "base01" = "#586e75"
solarized "base00" = "#657b83"
solarized "base0" = "#839496"
solarized "base1" = "#93a1a1"
solarized "base2" = "#eee8d5"
solarized "base3" = "#fdf6e3"
solarized "yellow" = "#b58900"
solarized "orange" = "#cb4b16"
solarized "red" = "#dc322f"
solarized "magenta" = "#d33682"
solarized "violet" = "#6c71c4"
solarized "blue" = "#268bd2"
solarized "cyan" = "#2aa198"
solarized "green" = "#859900"
solarized "text" = solarized "base0"
solarized "secondary" = solarized "base01"
solarized "background" = solarized "base03"
solarized "bghighlights" = solarized "base02"
solarized "emphasis" = solarized "base1"
solarized _ = solarized "text" --Use foreground color as default

------------------------------------------------------------------------
-- Topic Spaces
data TopicItem = TI
  { topicName :: Topic   -- (22b)
  , topicDir :: Dir
  , topicAction :: X ()
  }

-- define some custom topics for use with the TopicSpace module.
myTopics
  :: Host -> [TopicItem]
myTopics host =
  [ TI "web" "." (spawn "firefox")
  , TI
      "chat"
      "."
      -- (spawnShell host (Just "irc") >>
      (spawn "telegram-desktop" >>
       spawn "discord")
  , ti "work" "Projects"
  , TI "games" "." (spawn "steam")
  , TI "stream" "." (spawn "obs")
  , TI "virt" "." (spawn "virt-manager")
  , TI "video" "." (spawn "firefox")
  ]
  where
    ti t d = TI t d (spawnShell host (Just t))

myTopicNames :: Host -> [Topic]
myTopicNames = map topicName . myTopics

myTopicConfig :: Host -> TopicConfig
myTopicConfig host =
  def
  { topicDirs = M.fromList $ map (\(TI n d _) -> (n, d)) myTopics'
  , defaultTopicAction = const (return ())
  , defaultTopic = "web"
  , maxTopicHistory = 10
  , topicActions = M.fromList $ map (\(TI n _ a) -> (n, a)) myTopics'
  }
  where
    myTopics' = myTopics host

------------------------------------------------------------------------
-- Scratchpads
scratchpads
  :: Host -> [NamedScratchpad]
scratchpads host =
  [ ns "htop" "htop" mySPFloat
  , ns "ghci" "ghci" mySPFloat
  , ns "mail" "mutt" mySPLargeFloat
  , ns "emacs" "emacs" mySPLargeFloat
  , NS "brain" "emacs --eval '(org-brain-visualize \"Brain\")' --eval '(delete-other-windows)'"
    (title =? "*org-brain*") mySPFloat
  , case getAudioSystem host of
      Alsa -> ns "volume" "alsamixer" mySPLargeFloat
      Pulse ->
        NS "volume" "pavucontrol" (className =? "Pavucontrol") mySPLargeFloat
  , ns "calendar" "ikhal" mySPLargeFloat]
  where
    ns n p = NS n (termTmuxStart p) (className =? p)
    termTmuxStart n =
      myTerminal ++ " -c " ++ n ++ " -e tmux new -s " ++ n ++ " " ++ n
    mySPFloat = customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)
    mySPLargeFloat =
      customFloating $ W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
myKeys host logPipe = myKeymap host (myConfig host logPipe)

myKeymap host conf =
  [
    ("M-C-S-q", spawn "sudo poweroff")
    -- Media Keys
  , ("<XF86AudioPlay>", spawn "mpc toggle")
  , ("<XF86AudioNext>", spawn "mpc next")
  , ("<XF86AudioPrev>", spawn "mpc prev")
  , ("<XF86Sleep>", spawn "xset dpms force off")
  , ("M-S-s", spawn "sleep 1;xset dpms force off")
  ,
    -- General
    ("M-<Backspace>", focusUrgent)
  , ("M-S-<Backspace>", clearUrgents)
  , ("M-<Return>", windows W.swapMaster)
  , ("M-S-<Return>", spawnHere (myTerminal ++ " -e tmux"))
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-M1-l", spawn "i3lock -d -i ~/Pictures/LockScreen.png")
  , ("M-m", windows W.focusMaster)
  , ("M-q", spawn "xmonad --recompile; xmonad --restart")
  , ("M-S-q", io exitSuccess)
  , ("M-t", withFocused $ windows . W.sink)
  , ("M-u", spawnHere "firefox -new-window")
  , ("<Print>", spawn "scrot")
  , ("C-<Print>", spawn "sleep 0.2; scrot -s")
  , ("M-b", sendMessage ToggleStruts)
  , ("M-f", newCodeWS)
  ,
    -- Topic actions
    ("M-a", currentTopicAction (myTopicConfig host))
  ,
    -- Window Movement
    ("M-g", promptedGoto host)
  , ("M-S-g", promptedShift)
  , ("M-z", toggleWS)]
  ++ -- Volume
  case getAudioSystem host of
    Alsa ->
      [ ("<XF86AudioLowerVolume>", spawn "amixer sset Master 1dB-")
      , ("<XF86AudioMute>", spawn "amixer sset Master toggle")
      , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 1dB+")]
    Pulse ->
      [ ( "<XF86AudioLowerVolume>"
        , spawn "~/bin/ponymix decrease 5")
      , ("<XF86AudioMute>", spawn "~/bin/ponymix toggle")
      , ( "<XF86AudioRaiseVolume>"
        , spawn "~/bin/ponymix increase 5")]
  -- I have the rest in list-comprehension groups, because they make
  -- it easier, for me personally, to read.
  ++ -- Various Prompts
  [ ("M-p " ++ k, f)
  | (k,f) <-
     [ ("p", spawn "~/.xmonad/bin/passmenu")
     , ("r", runOrRaisePrompt myXPConfig)
     , ("e", spawn "exe=`echo | yeganesh -x` && eval \"exec $exe\"")
     , ("s", sshPrompt myXPConfig)
     , ("m", prompt "mpc " myXPConfig)
     , ("S-m", manPrompt myXPConfig)
     , ("t", prompt (myTerminal ++ " -e tmux new -s ") myXPConfig)] ]
  ++ -- Workspace Groups
  [ ("M-y " ++ k, f)
  | (k,f) <-
     [ ("n", promptWSGroupAdd myXPConfig "Name this group: ")
     , ("g", promptWSGroupView myXPConfig "Go to group: " >> viewScreen 1)
     , ("d", promptWSGroupForget myXPConfig "Forget group: ")] ]
  ++ -- Dynamic Workspaces
  [ ("M-w " ++ k, f)
  | (k,f) <-
     [ ("n", addWorkspacePrompt myXPConfig)
     , ("S-n", renameWorkspace myXPConfig)
     , ("r", renameWorkspace myXPConfig)
     , ("C-c", removeWorkspace)
     , ("C-k", killAll >> removeWorkspace)] ]
  ++ -- Scratchpads
  [ ("M-s " ++ k, namedScratchpadAction (scratchpads host) sp)
  | (k,sp) <-
     [ ("t", "htop")
     , ("g", "ghci")
     , ("m", "mail")
     , ("e", "emacs")
     , ("v", "volume")
     , ("b", "brain")
     , ("c", "calendar")] ]
  ++ -- toggles: fullscreen, flip x, flip y, mirror, no borders
  [ ("M-C-" ++ k, sendMessage f)
  | (k,f) <-
     [ ("<Space>", Toggle NBFULL)
     , ("x", Toggle REFLECTX)
     , ("y", Toggle REFLECTY)
     , ("m", Toggle MIRROR)
     , ("b", Toggle NOBORDERS)] ]
  ++ -- GridVariants keys
  [ ("M-C-g " ++ k, sendMessage f)
  | (k,f) <-
     [ ("i", G.IncMasterRows 1)
     , ("d", G.IncMasterRows (-1))
     , ("M-i", G.IncMasterCols 1)
     , ("M-d", G.IncMasterCols (-1))] ]
  ++ -- Float Window Movement
  [ ("M-M1-" ++ dir, withFocused (keysMoveWindow (dx, dy)))
  | (dir,dx,dy) <- [("h", -20, 0), ("n", 20, 0), ("c", 0, -20), ("t", 0, 20)] ]
  ++ -- Float Window Resize
  [ ("M-C-" ++ dir, withFocused (keysResizeWindow (dx, dy) (1, 1)))
  | (dir,dx,dy) <- [("n", -20, 0), ("h", 20, 0), ("t", 0, -20), ("c", 0, 20)] ]
  ++ -- Move focus, or move windows, between screens
  [ (m ++ "M-" ++ k, f s)
  | (k,s) <- zip [";", ",", "."] [0 ..]
  , (f,m) <- [(viewScreen, ""), (sendToScreen, "S-")] ]

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
  onWorkspace "web" (noBorders Full ||| tiled) $
  onWorkspace "chat" chatLayout $
  onWorkspace "games" (noBorders Full) $
  onWorkspaces
    [ "code" ++ show i
    | i <- [0 .. 10] ]
    (noBorders Full ||| TwoPane (2 / 100) (1 / 2)) $
  noBorders Full ||| tiled ||| Mag.magnifier Grid ||| TwoPane (2 / 100) (1 / 2)
  where
    tiled = ResizableTall 1 (2 / 100) (1 / 2) []
    chatLayout =
      noBorders $ magnify splitGrid ||| tallGrid ||| tabbed shrinkText tabConfig
    tallGrid = G.TallGrid 2 1 (1 / 2) (16 / 10) 1
    splitGrid = G.SplitGrid G.R 1 1 (1 / 2) (16 / 10) 1
    magnify = Mag.magnifiercz (20 % 10)

------------------------------------------------------------------------
-- Window rules:
myManageHook
  :: Host -> Query (Endo WindowSet)
myManageHook host =
  composeAll $
  [ resource =? r --> doIgnore
  | r <- myIgnores ] ++
  [ className =? "Xmessage" --> doCenterFloat
  , className =? "Skype" --> doShift "skype"
  , className =? "Steam" --> doShift "games"
  , isFullscreen --> myDoFullFloat
  , manageDocks
  , namedScratchpadManageHook (scratchpads host)]
  where
    -- resources
    myIgnores = ["desktop", "desktop_window", "notify-osd", "trayer", "panel"]
    myDoFullFloat = doF W.focusDown <+> doFullFloat

------------------------------------------------------------------------
-- Startup hook
myStartupHook host logPipe =
  checkKeymap (myConfig host logPipe) (myKeys host logPipe)

------------------------------------------------------------------------
-- Set up status bar
myBar
  :: Host -> Bool -> String
myBar host isSecondary =
  "dzen2" ++
  concatMap
    (" " ++)
    [ "-dock"
    , "-x '0'"
    , "-y '0'"
    , "-h '16'"
    , if isSecondary
        then case host of
               Desktop s _ ->
                 if s /= (-1)
                   then "-xs " ++ show s
                   else ""
               _ -> ""
        else "-xs 1"
    , case host    -- Change font size on Large Resolution/Retina display
            of
        Laptop _ True _ ->
             "-fn '-*-Source Code Pro-medium-r-*-*-20-*-*-*-*-*-*-*'"
        _ -> "-fn '-*-Source Code Pro-medium-r-*-*-10-*-*-*-*-*-*-*'"
    , "-bg '" ++ solarized "background" ++ "'"
    , "-fg '" ++ solarized "text" ++ "'"
    , "-ta 'center'"
    , "-e 'onstart=lower'"]

myLoghook logPipe host =
  dynamicLogWithPP $
  defaultPP
  { ppCurrent = dzenColor (solarized "green") ""
  , ppVisible = dzenColor (solarized "cyan") ""
  , ppHidden = dzenColor (solarized "text") ""
  , ppUrgent = dzenColor (solarized "yellow") (solarized "red")
  , ppLayout = dzenColor (solarized "text") ""
  , ppTitle = shorten 100
  , ppSort = fmap (namedScratchpadFilterOutWorkspace .) DO.getSortByOrder
  , ppExtras =
    [ date "%a %b %d  %I:%M %p"
    , loadAvg
    , dzenColorL (solarized "green") "" $
      wrapL "Inbox: " "" $ maildirUnread ".maildir/Inbox"] ++
    (case host of
       Laptop{} -> [battery]
       Desktop _ _ -> [])
  , ppOrder = \(ws:l:t:exs) -> [t, l, ws] ++ exs
  , ppOutput = hPutStrLn logPipe
  }

-----------------------------------------------------------------------
-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig =
  defaultTheme
  { activeBorderColor = solarized "emphasis"
  , activeTextColor = solarized "green"
  , activeColor = solarized "bghighlights"
  , inactiveBorderColor = solarized "background"
  , inactiveTextColor = solarized "text"
  , inactiveColor = solarized "background"
  }

-----------------------------------------------------------------------
-- Prompt Config
myXPConfig
  :: XPConfig
myXPConfig =
  defaultXPConfig
  { font = "xft:Source Code Pro:pixelsize=16:autohint=true"
  , bgColor = solarized "background"
  , fgColor = solarized "text"
  , borderColor = solarized "emphasis"
  , promptBorderWidth = 1
  }

------------------------------------------------------------------------
-- Urgency Hook
data LibNotifyUrgencyHook =
  LibNotifyUrgencyHook
  deriving (Read,Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name' <- getName w
    Just idx <- W.findTag w <$> gets windowset
    safeSpawn "notify-send" [show name', "workspace " ++ idx]

------------------------------------------------------------------------
-- Code Workspaces
newCodeWS
  :: X ()
newCodeWS =
  withWindowSet $
  \w -> do
    let wss = W.workspaces w
        cws =
          map W.tag $
          filter
            (\ws -> "code" `isPrefixOf` W.tag ws && isJust (W.stack ws))
            wss
        num = head $ [0 ..] \\ mapMaybe (readMaybe . drop 4) cws
        new = "code" ++ show num
    unless (new `elem` map W.tag wss) $ addWorkspace new
    windows $ W.view new
  where
    readMaybe s =
      case reads s of
        [(r,_)] -> Just r
        _ -> Nothing
