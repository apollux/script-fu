import XMonad

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.List (isPrefixOf, (\\), tails)
import Data.Maybe (isNothing, fromMaybe, isJust, catMaybes)

import System.IO
import System.Posix.Unistd
import System.Posix.Files

import Control.Arrow (second)
import Control.Concurrent (threadDelay)
import Control.Monad (when, replicateM)
import Control.Monad.Trans (liftIO)

import System.Directory
import System.FilePath

import Text.Printf (printf)

-- Hooks -----------------------------------------------------

import XMonad.Hooks.DynamicLog hiding (pprWindowSet)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

-- Layout ----------------------------------------------------

import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.HintedTile
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Named
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

-- Actions ---------------------------------------------------

import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS

import XMonad.Actions.Submap
import XMonad.Actions.WindowGo

import XMonad.Actions.WithAll
import XMonad.Actions.SpawnOn

import XMonad.Actions.TopicSpace
import XMonad.Actions.DynamicWorkspaces

-- Prompts ---------------------------------------------------

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Workspace

-- Utilities -------------------------------------------------

import XMonad.Util.EZConfig         -- (29) "M-C-x" style keybindings
import XMonad.Util.Run              -- (31) for 'spawnPipe', 'hPutStrLn'

-- Stuff to get Java working --------------------------------

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

main :: IO ()                                                     -- (31)
main = do
  h <- spawnPipe scvalexDzenBar
  spawn scvalexTrayer
  spawn "/usr/libexec/polkit-gnome-authentication-agent-1"
  --spawn "conky"
  spawn "emacs --daemon"
  --spawn "xcompmgr -c"
  spawn "inotify-daemon"
  spawn "mail-notification"
  spawn "nm-applet"
  host <- getHost
  checkTopicConfig myTopicNames myTopicConfig
  xmonad $ scvalexConfig h host                         -- (0)

scvalexDzenBar :: String
scvalexDzenBar = "dzen2 -ta r -fg '#cccccc' -bg '#111111' -e 'onstart=lower' -w 820 -x 69 -h 18"

scvalexTrayer :: String
scvalexTrayer = "trayer --edge top --align left --SetDockType true --SetPartialStrut true --expand false --width 5 --height 18 --tint 0x111111 --transparent true"

data Host = Desktop | Laptop Bool -- ^ Does the laptop have a Windows key?
  deriving (Eq, Read, Show)

getHost :: IO Host
getHost = do
  hostName <- nodeName `fmap` getSystemID
  return $ case hostName of
    "dakota" -> Laptop True
    _        -> Desktop

myTerminal = "terminal"
myShell = "zsh"

scvalexConfig h host =
     defaultConfig
       {
         borderWidth        = 1
       , terminal           = myTerminal
       , workspaces         = myTopicNames
       , modMask            = if host == Laptop False
                                then modMask defaultConfig
                                else mod4Mask

       , normalBorderColor  = "#dddddd"
       , focusedBorderColor = "#0033ff"
                                                                -- (22)
       , logHook            = myDynamicLog h host
       , manageHook         = manageSpawn
                              <+> myManageHook
                              <+> manageHook defaultConfig
       , layoutHook         = myLayoutHook
       , focusFollowsMouse  = True

       , startupHook        = ewmhDesktopsStartup >> setWMName "LG3D" >>
                              checkKeymap (scvalexConfig h host)
                                          (myKeys h host)
       , mouseBindings      = scvalexMouseBindings
       }
       `additionalKeysP` (myKeys h host)                        -- (29)

scvalexMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
scvalexMouseBindings (XConfig {XMonad.modMask = modMask}) =
    M.fromList $
         [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster))
         , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                          >> windows W.shiftMaster)) ]

data TopicItem = TI { topicName :: Topic
                    , topicDir  :: Dir
                    , topicAction :: X ()
                    }

-- define some custom topics for use with the TopicSpace module.
myTopics :: [TopicItem]
myTopics = [ TI "os" "" (spawnEmacs)
           , TI "web" "" (spawn "firefox-bin")
           , TI "irc" "" ({- spawn "skype" >> -} spawnShellWith "weechat-curses")
           , TI "mail" "" (spawnShellWith "tmux att -t email" >>
                           spawnShellWith "mutt")
           , TI "music" "music" (spawnShellWith "cmus")
           , TI "files" "" (spawn "dolphin")
           , TI "doc" "" (return ())
           , TI "alt" "" (return ())
           ]

myTopicNames :: [Topic]
myTopicNames = map topicName myTopics

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
  { topicDirs = M.fromList $ map (\(TI n d _) -> (n,d)) myTopics
  , defaultTopicAction = const (return ())
  , defaultTopic = "web"
  , maxTopicHistory = 10
  , topicActions = M.fromList $ map (\(TI n _ a) -> (n,a)) myTopics
  }

spawnShellWith :: String -> X ()
spawnShellWith what = spawn (myTerminal ++ printf " -e '%s'" what)

spawnShell :: X ()
spawnShell = spawnShellWith myShell

spawnEmacs :: X ()
spawnEmacs = spawn ("emacsclient -c")

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedGotoOtherScreen :: X ()
promptedGotoOtherScreen =
  workspacePrompt myXPConfig $ \ws -> do
    nextScreen
    goto ws

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

myDynamicLog h host = dynamicLogWithPP $ byorgeyPP              -- (1)
  { ppVisible = dzenColor "black" "#aaaaaa" . pad
  , ppOrder  = \(ws:l:t:exs) -> [t,l,ws]++exs                   -- (1)
  , ppOutput = hPutStrLn h                                      -- (1,31)
  , ppTitle  = shorten (case host of Laptop _ -> 45
                                     Desktop  -> 60)
  , ppHiddenNoWindows = const ""
  }

-- my custom keybindings.
myKeys h host = myKeymap host (scvalexConfig h host)

myKeymap host conf =
    -- mod-[1..],       Switch to workspace N
    -- mod-shift-[1..], Move client to workspace N
    -- mod-ctrl-[1..],  Switch to workspace N on other screen
    [ (m ++ "M-" ++ [k], f i)                                   -- (0)
        | (i, k) <- zip (XMonad.workspaces conf) "1234567890-=[]\\" -- (0)
        , (f, m) <- [ (windows . W.view, "")                    -- (0a)
                    , (windows . W.shift, "S-")
                    , (\ws -> nextScreen >> (windows . W.view $ ws), "C-")
                    ]
    ]

    ++
    [ ("M-S-x", spawnShell)
    , ("M-S-e", spawnEmacs)
    , ("M-g",   promptedGoto)
    , ("M-S-g", promptedShift)
    , ("M-S-C-g", workspacePrompt myXPConfig $ withAll' . W.shiftWin)

    -- sink floating windows
    , ("M-t", withFocused $ windows . W.sink)

    -- rotate workspaces.
    , ("M-C-<R>",   nextWS )                                    -- (16)
    , ("M-C-<L>",   prevWS )                                    --  "
    , ("M-S-<R>",   shiftToNext )                               --  "
    , ("M-S-<L>",   shiftToPrev )                               --  "
    , ("M-S-C-<R>", shiftToNext >> nextWS )                     --  "
    , ("M-S-C-<L>", shiftToPrev >> prevWS )                     --  "
    , ("M-<R>",     moveTo Next HiddenNonEmptyWS)               --  "
    , ("M-<L>",     moveTo Prev HiddenNonEmptyWS)               --  "
    , ("M-f",       newCodeWS)                                  --  "

    -- quick workspaces
    , ("<F4>", newCodeWS >> spawnShell)
    , ("<F5>", goto "web")
    , ("<F6>", goto "files")
    , ("<F7>", goto "mail")
    , ("<F8>", goto "irc")
    , ("<F9>", goto "os")
    , ("<F11>", goto "doc")

    -- dvorak
    , ("<Pause>", spawn "toggle_dvorak.sh")

    -- expand/shrink windows
    , ("M-r k", sendMessage MirrorExpand)
    , ("M-r j", sendMessage MirrorShrink)
    , ("M-r h", sendMessage Shrink)
    , ("M-r l", sendMessage Expand)

    -- switch to previous workspace
    , ("M-z", toggleWS)
    , ("M-S-z", killAll >> liftIO (threadDelay 2000) >> moveTo Prev HiddenNonEmptyWS)
    , ("C-M-<Delete>", replicateM 10 (killAll >> moveTo Prev HiddenNonEmptyWS) >> spawn "sudo poweroff")

    -- dynamic workspace bindings
    --, ("M-n", addWorkspacePrompt myXPConfig)
    , ("M-C-r", removeWorkspace)
    , ("M-C-S-r", killAll >> removeWorkspace)

    -- move between screens
    , ("M-s", nextScreen)
    , ("M-w", swapNextScreen)
    , ("M-e", shiftNextScreen)

    -- lock the screen with xscreensaver
    , ("M-S-l", spawn "xscreensaver-command -lock")

    -- alsamixer and xkill
    , ("M-c v", spawn "terminal -e alsamixer")
    , ("M-c k", spawn "xkill")

    -- multimedia
    , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 10%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 10%+")
    , ("<XF86AudioMute>", spawn "amixer sset Master toggle")
    , ("<XF86Display>", spawn "xrandr --auto")

    -- window navigation keybindings.
    , ("S-C-<R>", sendMessage $ Swap R)                         --  "
    , ("S-C-<L>", sendMessage $ Swap L)                         --  "
    , ("S-C-<U>", sendMessage $ Swap U)                         --  "
    , ("S-C-<D>", sendMessage $ Swap D)                         --  "

    -- toggles: fullscreen, flip x, flip y, mirror, no borders
    , ("M-C-<Space>", sendMessage $ Toggle NBFULL)              -- (14)
    , ("M-C-x",       sendMessage $ Toggle REFLECTX)            -- (14,13)
    , ("M-C-y",       sendMessage $ Toggle REFLECTY)            -- (14,13)
    , ("M-C-m",       sendMessage $ Toggle MIRROR)              --  "
    , ("M-S-f",       sendMessage $ ToggleStruts)

    , ("M-p", shellPrompt myXPConfig)
    , ("M-o", docPrompt myXPConfig)

    -- some random utilities.
    , ("M-<Print>", spawn "import screen.png")
    ]

newCodeWS :: X ()
newCodeWS = withWindowSet $ \w -> do
  let wss = W.workspaces w
      cws = map W.tag $ filter (\ws -> "code" `isPrefixOf` W.tag ws && isJust (W.stack ws)) wss
      num = head $ [0..] \\ catMaybes (map (readMaybe . drop 4) cws)
      new = "code" ++ show num
  when (new `notElem` (map W.tag wss)) $ addWorkspace new
  windows $ W.view new
 where readMaybe s = case reads s of
                       [(r,_)] -> Just r
                       _       -> Nothing

-- Switch to the "web" workspace
viewWeb = windows (W.view "web")                                -- (0,0a)

-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = defaultXPConfig                                    -- (23)
    { fgColor = "#cccccc"
    , bgColor = "#111111"
    , font    = "xft:LiberationMono:pixelsize=14"
    , height  = 30
    }

-- Set up a customized manageHook (rules for handling windows on
--   creation)
myManageHook :: ManageHook                                      -- (0)
myManageHook =
  composeAll
      $ [ className =? c <||> isDialog --> doCenterFloat | c <- myFloats ]
      ++ [ title =~ "Call with" --> doCenterFloat ]
      ++ [ className =? name --> doShift ws | (name, ws) <- myWindows ]
      ++ [ manageDocks ]
    where myFloats = [ "Volume"
                     , "XClock"
                     , "Ssh-askpass-fullscreen"
		     , "Wicd-client.py"
                     ]
          myWindows = [ ("Chrome",    "web")
                      , ("Chromium-browser", "web")
                      , ("Firefox",   "web")
                      , ("Kopete",    "irc")
                      , ("Empathy",   "irc")
                      , ("Skype",     "irc")
                      , ("Ktorrent",  "alt")
                      , ("Amarok",    "music")
                      , ("Dolphin",   "files")
                      , ("Kompare",   "alt")
                      , ("Okular",    "doc")
                      , ("Gitg",      "doc")
                      , ("Gwenview",  "doc") ]
          contains :: (Eq a) => [a] -> [a] -> Bool
          contains p = any (isPrefixOf p) . tails
          q =~ x = fmap (contains x) q

-- specify a custom layout hook.
myLayoutHook =

    -- automatically avoid overlapping my dzen status bar.
    avoidStrutsOn [U] $                                        -- (3)

    -- navigate directionally rather than with mod-j/k
    configurableNavigation (navigateColor "#00aa00") $          -- (8)

    -- ability to toggle between fullscreen, reflect x/y, no borders,
    -- and mirrored.
    mkToggle1 NBFULL $                                  -- (14)
    mkToggle1 REFLECTX $                                -- (14,13)
    mkToggle1 REFLECTY $                                -- (14,13)
    mkToggle1 NOBORDERS $                               --  "
    mkToggle1 MIRROR $                                  --  "

    smartBorders $

    Full |||
    TwoPane (3/100) (1/2) |||
    myTiled |||           -- resizable tall layout
    HintedTile 1 (3/100) (1/2) TopLeft Wide |||
    Grid


-- use ResizableTall instead of Tall, but still call it "Tall".
myTiled = named "Tall" $ ResizableTall 1 0.03 0.5 []

-- Doc prompt
data Doc = Doc

instance XPrompt Doc where
    showXPrompt Doc = "okular "

docPrompt :: XPConfig -> X ()
docPrompt c = do
  files <- liftIO $ flatDir "/home/scvalex/" 2
  let files' = filter ((==".pdf") . takeExtension . fst) files
      cmds = map (\(f, fp) -> (f, spawn $ "okular \"" ++ fp ++ "\"")) files'
  docPromptC cmds c
    where
      flatDir :: FilePath -> Int -> IO [(FilePath, FilePath)]
      flatDir _    0     = return []
      flatDir base depth = do
                ok <- return . isDirectory =<< getFileStatus base
                if ok
                   then do
                     fs <- getDirectoryContents base
                     fs' <- mapM (flip flatDir (depth - 1)) (map (base </>) fs)
                     return $ (concat fs') ++ (map (\f -> (f, base </> f)) fs)
                   else do
                     return []

docPromptC :: [(String, X ())] -> XPConfig -> X ()
docPromptC commands c =
    mkXPrompt Doc c (mkComplFunFromList' (map fst commands)) $
        fromMaybe (return ()) . (`lookup` commands)
