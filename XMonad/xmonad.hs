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
import XMonad.Actions.Minimize
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.RotSlaves
import XMonad.Config.Desktop
import XMonad.Util.Run
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Ungrab
import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh
import XMonad.Prompt.Shell
import XMonad.Prompt.Workspace
import XMonad.Prompt.RunOrRaise
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Minimize
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.BoringWindows
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.Simplest
import XMonad.Layout.TwoPane
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import qualified XMonad.Layout.Magnifier as Mag
import qualified XMonad.Layout.GridVariants as G
import Data.Ratio ((%))
import System.Posix.Unistd
import XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Hooks.EwmhDesktops

type HasWinKey = Bool

type IsRetina = Bool

type StatusBarDisplay = Int

data AudioSystem
  = Alsa
  | Pulse
  deriving (Eq, Typeable, Read, Show)

data Host
  = Desktop StatusBarDisplay
            AudioSystem
  | Laptop HasWinKey
           IsRetina
           AudioSystem
  deriving (Eq, Typeable, Read, Show)
instance ExtensionClass Host where
    initialValue = Desktop (-1) Pulse

getHost :: IO Host
getHost = do
  hostName <- nodeName `fmap` getSystemID
  return $
    case hostName of
      "renard" -> Desktop 3 Pulse
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
  checkTopicConfig myTopicNames myTopicConfig
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
        , workspaces = myTopicNames
        , normalBorderColor = solarized "background"
        , focusedBorderColor = solarized "green"
        , borderWidth = 1
        , layoutHook = myLayoutHook
        , manageHook = myManageHook host <+> manageSpawn <+> manageHook desktopConfig
        , logHook = myLoghook logPipe <+> logHook desktopConfig
        , startupHook = myStartupHook host logPipe <+> startupHook desktopConfig
        , handleEventHook = minimizeEventHook <+> docksEventHook <+> handleEventHook desktopConfig
        } `additionalKeysP` myKeys host logPipe

------------------------------------------------------------------------
-- Usefull common vars
myTerminal :: String
myTerminal = "st"

------------------------------------------------------------------------
-- Helper functions
spawnShell :: Maybe String -> X ()
spawnShell name' = currentTopicDir myTopicConfig >>= spawnShellIn name'

spawnShellIn :: Maybe String -> Dir -> X ()
spawnShellIn Nothing dir = do
    tag <- gets (W.currentTag . windowset)
    spawn $ myTerminal ++ " -e tmux new -A -t '" ++ tag ++ "' -c '" ++ dir ++ "'"
spawnShellIn (Just name') dir =
    spawn $ myTerminal ++ " -e tmux new -A -t '" ++ name' ++ "' -c '" ++ dir ++ "'"

removeEmptyNonTopicWorkspaceAfter :: X () -> X ()
removeEmptyNonTopicWorkspaceAfter = removeEmptyWorkspaceAfterExcept myTopicNames

goto :: String -> X ()
goto topic = removeEmptyNonTopicWorkspaceAfter $ switchTopic myTopicConfig topic

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedShift :: X ()
promptedShift = do
    workspacePrompt myXPConfig $ windows . W.shift
    curTag <- gets (W.currentTag . windowset)
    when (curTag `notElem` myTopicNames) removeEmptyWorkspace

-- Solarized colors
solarized :: String -> String
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
  { topicName :: Topic
  , topicDir :: Dir
  , topicAction :: X ()
  }

-- define some custom topics for use with the TopicSpace module.
myTopics :: [TopicItem]
myTopics =
  [ TI "web" "." (spawn "firefox -P Reyu")
  , TI "work" "Projects" spawnTopicShell
  , TI "chat" "."
       (spawn "telegram-desktop" >>
        spawn "discord" >>
        spawnTopicShell)
  , TI "video" "." (spawn "/home/reyu/bin/brave")
  , TI "games" "." (spawn "steam")
  , TI "stream" "." (spawn "obs")
  , TI "virt" "." (spawn "virt-manager")
  , TI "modeling" "Projects/Blender" (spawn "~/bin/blender")
  , TI "capacity" "Projects/Capacity" (spawn (myTerminal ++ " -e ssh macbook")  >> spawn "firefox -P capacity")
  ]
  where
    spawnTopicShell = spawnShell Nothing

myTopicNames :: [Topic]
myTopicNames = map topicName myTopics

myTopicConfig :: TopicConfig
myTopicConfig =
  def
  { topicDirs = M.fromList $ map (\(TI n d _) -> (n, d)) myTopics
  , defaultTopicAction = \x ->
      spawnShell (Just x) >>
      spawn ("qutebrowser -r " ++ x)
  , defaultTopic = "web"
  , maxTopicHistory = 10
  , topicActions = M.fromList $ map (\(TI n _ a) -> (n, a)) myTopics
  }

------------------------------------------------------------------------
-- Scratchpads
scratchpads :: Host -> [NamedScratchpad]
scratchpads host =
  [ term "htop" "htop" mySPFloat
  , term "ghci" "ghci" mySPFloat
  , tmux "mail" "neomutt" mySPLargeFloat
  , term "news" "zsh -lc slrn" mySPLargeFloat
  , NS "thunderbird" "thunderbird" (className =? "Thunderbird") mySPLargeFloat
  , case getAudioSystem host of
      Alsa -> term "volume" "alsamixer" mySPLargeFloat
      Pulse -> NS "volume" "pavucontrol" (className =? "Pavucontrol") mySPLargeFloat
  , tmux "calendar" "ikhal" mySPLargeFloat]
  where
    -- Run in Terminal
    term n c = NS n (myTerminal ++ " -c " ++ n ++ " -- " ++ c) (className =? n)
    -- Run in TMUX session
    tmux n c = NS n (myTerminal ++ " -c " ++ n ++ " -e tmux new -As " ++ n ++ " " ++ c) (className =? n)
    -- Helpers
    mySPFloat = customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)
    mySPLargeFloat = customFloating $ W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
myKeys host logPipe = myKeymap host (myConfig host logPipe)

myKeymap host conf =
  [
    -- Media Keys
    ("<XF86AudioPlay>", spawn "mpc toggle")
  , ("<XF86AudioNext>", spawn "mpc next")
  , ("<XF86AudioPrev>", spawn "mpc prev")
  , ("<XF86Sleep>", spawn "xset dpms force off")
  , ("M-S-s", unGrab >> spawn "sleep 1;xset dpms force off")
  , ("M-S-<XF86Sleep>", spawn "sudo poweroff")
  ,
    -- General
    ("M-<Backspace>", focusUrgent)
  , ("M-S-<Backspace>", clearUrgents)
  , ("M-<Return>", windows W.swapMaster)
  , ("M-S-<Return>", spawnShell Nothing)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-M1-l", spawn "i3lock -d -c FFFFFF -t -i ~/Pictures/LockScreen.png")
  , ("M-m", windows W.focusMaster)
  , ("M-q", spawn "xmonad --recompile; xmonad --restart")
  , ("M-C-S-q", io exitSuccess)
  , ("M-t", withFocused $ windows . W.sink)
  , ("M-u", AL.launchApp myXPConfig { defaultText = "-P Reyu --new-window " } "firefox")
  , ("M-S-u", do
        workspace <- gets (W.currentTag . windowset)
        AL.launchApp myXPConfig { defaultText = "-P " ++ workspace } "firefox")
  , ("<Print>", unGrab >> spawn "scrot")
  , ("C-<Print>", unGrab >> spawn "scrot -s")
  , ("M-b", sendMessage ToggleStruts)
  , ("M-f", newCodeWS)
  , ("M-C-S-b", markBoring)
  , ("M-S-m", withFocused minimizeWindow)
  , ("M-S-C-m", withLastMinimized maximizeWindowAndFocus)
  ,
    -- Topic actions
    ("M-a", currentTopicAction myTopicConfig)
  ,
    -- Window Movement
    ("M-j", bindOn [ ("", focusDown)
                   , ("chat", rotAllDown)
                   ])
  , ("M-k", bindOn [ ("", focusUp)
                   , ("chat", rotAllUp)
                   ])
  , ("M-S-j", bindOn [ ("", windows W.swapDown)
                     , ("chat", rotSlavesDown)
                     ])
  , ("M-S-k", bindOn [ ("", windows W.swapUp)
                     , ("chat", rotSlavesUp)
                     ])
  , ("M-C-j", windows W.focusDown)
  , ("M-C-k", windows W.focusUp)
  , ("M-g", promptedGoto)
  , ("M-S-g", promptedShift)
  , ("M-z", removeEmptyNonTopicWorkspaceAfter $ toggleWS' ["NSP"])]
  -- I have the rest in list-comprehension groups, because they make
  -- it easier, for me personally, to read.
  ++ -- Volume
  let keys = ["M-v k", "M-v j", "M-v m"
             , "<XF86AudioRaiseVolume>", "<XF86AudioLowerVolume>", "<XF86AudioMute>"]
      actions = concat . repeat $ case getAudioSystem host of
        Alsa ->
          [ spawn "amixer sset Master 1dB+"
          , spawn "amixer sset Master 1dB-"
          , spawn "amixer sset Master toggle"
          ]
        Pulse ->
          [ spawn "~/.xmonad/bin/ponymix increase 5"
          , spawn "~/.xmonad/bin/ponymix decrease 5"
          , spawn "~/.xmonad/bin/ponymix toggle"
          ]
  in zip keys actions
  ++ -- Various Prompts
  [ ("M-p " ++ k, f)
  | (k,f) <-
     [ ("p", spawn "~/.xmonad/bin/passmenu")
     , ("r", runOrRaisePrompt myXPConfig)
     , ("e", prompt "exec" myXPConfig)
     , ("S-e", prompt (myTerminal ++ " -e") myXPConfig)
     , ("s", sshPrompt myXPConfig)
     , ("m", manPrompt myXPConfig)
     , ("l", safePrompt "~/bin/lights" myXPConfig)
     , ("t", mkXPrompt
                Tmux
                myXPConfig { defaultText = "new -A -s " }
                tmuxCompletion
                (\x -> spawn $ myTerminal ++ " -e tmux " ++ x))
  ]]
  ++ -- Workspace Groups
  [ ("M-y " ++ k, f)
  | (k,f) <-
     [ ("n", promptWSGroupAdd myXPConfig "Name this group: ")
     , ("g", promptWSGroupView myXPConfig "Go to group: " >> viewScreen def 1)
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
     , ("n", "news")
     , ("S-m", "thunderbird")
     , ("v", "volume")] ]
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
  | (k,s) <- zip ["'", ",", ".", "o"] [0 ..]
  , (f,m) <- [(viewScreen def, ""), (sendToScreen def, "S-")] ]

------------------------------------------------------------------------
-- Layouts:
myLayoutHook =
  --noFrillsDeco shrinkText solarizedTheme $
  avoidStrutsOn [U] $
  smartBorders $
  boringWindows $
  minimize $
  mkToggle1 NBFULL $
  mkToggle1 REFLECTX $
  mkToggle1 REFLECTY $
  mkToggle1 NOBORDERS $
  mkToggle1 MIRROR $
  onWorkspace "web" (noBorders Simplest ||| tiled) $
  onWorkspace "chat" chatLayout $
  onWorkspace "games" (noBorders Simplest) $
  onWorkspace "video" (noBorders Simplest) $
  onWorkspaces
    [ "code" ++ show i
    | i <- [0 .. 10] ]
    (noBorders Simplest ||| TwoPane (2 / 100) (1 / 2)) $
    TwoPane (2 / 100) (1 / 2) ||| tiled ||| Mag.magnifier Grid
  where
    tiled = ResizableTall 1 (2 / 100) (1 / 2) []
    chatLayout =
       splitGrid ||| tallGrid ||| tabbed shrinkText solarizedTheme
    tallGrid = G.TallGrid 2 1 (1 / 2) (16 / 10) 1
    splitGrid = G.SplitGrid G.L 1 1 (9 / 16) (16 / 10) 1
    magnify = Mag.magnifiercz (20 % 10)

------------------------------------------------------------------------
-- Window rules:
myManageHook :: Host -> Query (Endo WindowSet)
myManageHook host =
  composeAll $
  [ resource =? r --> doIgnore
  | r <- myIgnores ] ++
  [ className =? "Xmessage" --> doCenterFloat
  , className =? "Zenity" --> doCenterFloat
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
myStartupHook host logPipe = do
  XS.put host
  checkKeymap (myConfig host logPipe) (myKeys host logPipe)

------------------------------------------------------------------------
-- Set up status bar
myBar :: Host -> Bool -> String
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
        _ -> "-fn '-*-Source Code Pro-medium-r-*-*-12-*-*-*-*-*-*-*'"
    , "-bg '" ++ solarized "background" ++ "'"
    , "-fg '" ++ solarized "text" ++ "'"
    , "-ta 'center'"
    , "-e 'onstart=lower'"]

myLoghook logPipe = do
  host <- XS.get :: X Host
  dynamicLogWithPP $ defaultPP
    { ppCurrent = dzenColor (solarized "green") ""
    , ppVisible = dzenColor (solarized "cyan") ""
    , ppHidden = dzenColor (solarized "blue") ""
    , ppUrgent = dzenColor (solarized "yellow") (solarized "red")
    , ppLayout = dzenColor (solarized "text") ""
    , ppTitle = shorten 150
    , ppSort = fmap (namedScratchpadFilterOutWorkspace .) DO.getSortByOrder
    , ppExtras =
      [ date "%a %b %d %H%M"
      , loadAvg
      , dzenColorL (solarized "green") "" $
        wrapL "Proton: " "" $ maildirUnread "Mail/Proton"
      , dzenColorL (solarized "yellow") "" $
        wrapL "BlkFox: " "" $ maildirUnread "Mail/BlackFox"
      , dzenColorL (solarized "blue") "" $
        wrapL "Gmail: " "" $ maildirUnread "Mail/Tim"
      ] ++
      (case host of
         Laptop{} -> [battery]
         Desktop _ _ -> [])
    , ppOrder = \(ws:l:t:exs) -> [ws, l] ++ exs
    , ppOutput = hPutStrLn logPipe
    }

-----------------------------------------------------------------------
-- Themes
solarizedTheme :: Theme
solarizedTheme =
  defaultTheme
  { fontName = "Source Code Pro"
  , activeBorderColor = solarized "green"
  , activeTextColor = solarized "green"
  , activeColor = solarized "bghighlights"
  , inactiveBorderColor = solarized "background"
  , inactiveTextColor = solarized "text"
  , inactiveColor = solarized "background"
  , urgentBorderColor = solarized "red"
  , urgentTextColor = solarized "red"
  }

-----------------------------------------------------------------------
-- Prompt Config
myXPConfig :: XPConfig
myXPConfig =
  defaultXPConfig
  { font = "xft:Source Code Pro:pixelsize=16:autohint=true"
  , height = 23
  , bgColor = solarized "background"
  , fgColor = solarized "text"
  , borderColor = solarized "emphasis"
  , promptBorderWidth = 1
  -- , promptKeymap = vimLikeXPKeymay -- Needs >=xmonad-contrib-0.16
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
newCodeWS :: X ()
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


------------------------------------------------------------------------
-- Tmux Prompt
data Tmux = Tmux
instance XPrompt Tmux where
    showXPrompt Tmux     = "tmux "
    completionToCommand _ = escape

escape :: String -> String
escape []       = ""
escape (x:xs)
    | isSpecialChar x = '\\' : x : escape xs
    | otherwise       = x : escape xs

isSpecialChar :: Char -> Bool
isSpecialChar =  flip elem " &\\@\"'#?$*()[]{};"

tmuxCompletion :: String -> IO [String]
tmuxCompletion input = return []
