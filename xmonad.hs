{-# LANGUAGE DeriveDataTypeable #-}                                                                   
import Data.Ratio
import Data.List
import XMonad
-- import XMonad.Config.Desktop
--import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Config.Xfce
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.Font
import XMonad.Layout
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutHints (layoutHints)
-- GIMP stuff
import XMonad.Layout.IM as IM
import XMonad.Layout.Reflect
import XMonad.Layout.TrackFloating
-- End GIMP stuff
import XMonad.Actions.CycleWS
import XMonad.Actions.OnScreen
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.IO
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.WorkspaceNames hiding (workspaceNamesPP)
import Control.Monad (liftM)
import XMonad.Actions.SpawnOn
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Operations
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Loggers
import XMonad.Util.Paste
import XMonad.Actions.WindowGo


-- DynamicWorkspaces Stuff
import XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.Util.ExtensibleState as XS
import Control.Applicative
filterWorkspaces :: [WorkspaceId] -> [WindowSpace] -> [WindowSpace]
filterWorkspaces ws = filter (\(W.Workspace tag _ _) -> tag `elem` ws)
	-- xScreen are the type classes which hold the workspace name lists
newtype LeftScreen = LeftScreen {getLeftScreen :: [WorkspaceId]} deriving (Typeable,Read,Show) 
instance ExtensionClass LeftScreen where
	initialValue = LeftScreen []
	extensionType = PersistentExtension


newtype RightScreen = RightScreen {getRightScreen :: [WorkspaceId]} deriving (Typeable,Read,Show)
instance ExtensionClass RightScreen where
	initialValue = RightScreen []
	extensionType = PersistentExtension


-- End DynamicWorkspaces code (keybindings are below) --



-- get these with xprop
myManageHook = composeAll
  [ isDialog --> doFloat
	, className =? "Gnome-dictionary" --> doFloat
  , className =? "Xfce4-dict" --> doFloat
  , className =? "Last.fm"	--> doFloat
  , className =? "Xmessage"   --> doFloat
  , className	=? "Audacious"  --> doFloat
  --, className =? "Gimp" 		--> doFloat
  , className =? "Skype"		--> doFloat
  , className =? "Keepassx"	--> doFloat
  , className =? "Kcalc"		--> doFloat
	, className =? "Clementine" --> doFloat
	, className =? "SpiderOak" --> doFloat
  , className =? "Pavucontrol" --> doFloat

{- The following sets doFloat on the Orage window (as above)
  But also ensures that it appears only on the left screen
  (screen 0).  (screenWorkspace 0) returns X (Maybe WorkspaceId),
  and the liftX function lifts an X action to a Query (which is 
  Maybe WorkspaceId) and the next lines return the workspace (if 
not empty), or do	nothing if (Maybe WorkspaceId) -> Nothing.
idHook maps to mempty, which means do nothing
-}
  , className =? "Orage"	--> doFloat 
	<+> do 
  ws <- liftX (screenWorkspace 0) 
  case ws of
    Just w -> doShift w
    Nothing -> idHook 
    -- end Orage window stuff
  
  -- Weather report stuff
  , className =? "Wrapper"	--> doFloat 
	<+> do 
  ws <- liftX (screenWorkspace 0) 
  case ws of
    Just w -> doShift w
    Nothing -> idHook 
    -- end Weather Report window stuff


  , className =? "Plasma-desktop"	--> doFloat 
	<+> do 
  ws <- liftX (screenWorkspace 0) 
  case ws of
    Just w -> doShift w
    Nothing -> idHook 
    -- end Plasma desktop stuff

	, className =? "Xfce4-notifyd" --> doIgnore

  ]


shiftInsert w =
  let translatedProgs = ["Chromium", "Chrome"]
  in do
  c <- runQuery className w;
  let toTranslate = any (== c) translatedProgs
  if toTranslate then spawn ("CLIP=$(xclip -out -selection clipboard); xclip -out"
    ++ " | xclip -in -selection clipboard; xdotool key --clearmodifiers --window "
    ++ show w ++ " ctrl+v; echo -n $CLIP | xclip -in -selection clipboard")
  else sendKey shiftMask xK_Insert

layoutH = layoutHints
    --    $ onWorkspace "5:" (Mirror tiled2
    --		||| tiled ||| Full)
    --	$ onWorkspaces ["1:wkbr","2:wksh"] (Full
    --		||| tiled ||| Mirror tiled)
    $ tiled
    ||| Mirror tiled
    ||| Full
    where
      tiled   = Tall 1 (3 % 100) (1/2)
    --tiled2  = Tall 1 (3 % 100) (5 % 9)

fadeHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.2

makeLauncher yargs run exec close = concat
  ["exe=`yeganesh ", yargs, "` && ", run, " ", exec, "$exe", close]

launcher = makeLauncher "-x -- -nf grey -nb black" "eval" "\"exec " "\""

main = do
  -- xmproc <- spawnPipe "xmobar"
  -- xmonad $ gnomeConfig {
    dbus <- D.connectSession
    getWellKnownName dbus;
 --		xmonad $ kde4Config {
    xmonad $ xfceConfig {
--    workspaces = slotWksp
    workspaces = ["sh","sb","of","wc","ws","wb","hng"]
 -- workspaces = [ "dee", "dum" ]
  , terminal = myTerminal
	-- Goodbye, my sweet, sweet, sloppy focus
--	, focusFollowsMouse	= False
 	-- Hello again, my sweet, sweet, sloppy focus!
	, focusFollowsMouse	= True
  , manageHook = manageDocks <+> myManageHook -- <+> manageHook defaultConfig
  , layoutHook = avoidStruts $ onWorkspace "gimp" gimp $ layoutH 
  , logHook = dynamicLogWithPP (ppL dbus)
   			>> dynamicLogWithPP (ppR dbus)
	      >> fadeHook
  , borderWidth = 1
  , normalBorderColor = "#333333"
  , focusedBorderColor = "#CCCC00"
  , modMask = winKey
  , startupHook = startupHook xfceConfig >> setWMName "L3GD"
  --, startupHook = do 
   --   setWMName "LG3D"
  }	`additionalKeys` myKeys
	where --{
	   gimp	= IM.withIM 0.11 (Role "gimp-toolbox") $
		  reflectHoriz $
		  IM.withIM 0.15 (Role "gimp-dock") Full 
	 --, vbox =
	--}

getWellKnownName :: D.Client -> IO()
getWellKnownName dbus = do 
  D.requestName dbus (D.busName_ "org.xmonad.Log")               
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue] 
  return()


outputThroughDBusR :: D.Client -> String -> IO()
outputThroughDBusR dbus str = do 
  let signal =	(D.signal  (D.objectPath_ "/org/xmonad/Log")
		(D.interfaceName_ "org.xmonad.Log")
		(D.memberName_ "Update")) {
		D.signalBody = [D.toVariant ("<span font=\"Terminus Bold 9\">" ++
					    (UTF8.decodeString str) ++ "</span>")]
		} 
  D.emit dbus signal


outputThroughDBusL :: D.Client -> String -> IO()
outputThroughDBusL dbus str = do 
  let signal =  (D.signal (D.objectPath_ "/org/xmonad/LogL")                         
		(D.interfaceName_ "org.xmonad.LogL")
		(D.memberName_ "Update")) {
		  D.signalBody = [D.toVariant ("<span font=\"Terminus Bold 9\">" ++
		  (UTF8.decodeString str) ++ "</span>")]                         
		} 
  D.emit dbus signal


pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left 	= "<span foreground=\"" ++ fg ++ "\">"
    right	= "</span>"
    
pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>' 	acc = "&gt;" ++ acc
    sanitize '<' 	acc = "&lt;" ++ acc
    sanitize '\"' 	acc = "" ++ acc
    sanitize '&'  	acc = "&amp;" ++ acc
    sanitize x    	acc = x:acc

{--- Now here's some fuckin' Voodoo Magic
filterOutLeft :: [WindowSpace] -> [WindowSpace]
filterOutLeft = filter (\(W.Workspace tag _ _) -> tag `elem` rightWksp)

filterOutRight :: [WindowSpace] -> [WindowSpace]
filterOutRight = filter (\(W.Workspace tag _ _) -> tag `elem` leftWksp)
-- End Voodoo
-}
ppR dbus = defaultPP {   
    ppOutput		= outputThroughDBusR dbus   
  , ppCurrent           = pangoColor "yellow" . wrap "[" "]" . pangoSanitize   
  , ppVisible           = pangoColor "cyan" . wrap "(" ")" . pangoSanitize   
  , ppTitle             = pangoColor "green" . shorten 50. pangoSanitize   
  , ppUrgent            = pangoColor "red"   
  , ppSep               = " "
--  , ppSort		= (.) <$> XS.gets (filterWorkspaces . getRightScreen) <*> getSortByTag
  , ppSort		= (.) <$> XS.gets (filterWorkspaces . getRightScreen) <*> getSortByIndex
  --, ppSort		= (.) <$> XS.gets (filterWorkspaces . getRightScreen) <*> getSortByXineramaPhysicalRule
  --, ppSort		= (.) <$> XS.gets (filterWorkspaces . getRightScreen) <*> getSortByXineramaRule
--, ppSort							= fmap (.filterOutLeft) getSortByTag
  , ppHiddenNoWindows 	= const ""   
  , ppHidden		= pangoColor "gray"               
}

ppL dbus = defaultPP {   
    ppOutput		= outputThroughDBusL dbus   
  , ppCurrent		= pangoColor "yellow" . wrap "[" "]" . pangoSanitize   
  , ppVisible		= pangoColor "cyan" . wrap "(" ")" . pangoSanitize   
  , ppTitle		= pangoColor "maroon" . shorten 50. pangoSanitize   
  , ppUrgent		= pangoColor "red"   
  , ppSep		= " "   
--, ppSort		= fmap (.filterOutRight) getSortByTag
--  , ppSort		= (.) <$> XS.gets (filterWorkspaces . getLeftScreen) <*> getSortByTag
  , ppSort		= (.) <$> XS.gets (filterWorkspaces . getLeftScreen) <*> getSortByIndex
  --, ppSort		= (.) <$> XS.gets (filterWorkspaces . getLeftScreen) <*> getSortByXineramaPhysicalRule
  --, ppSort		= (.) <$> XS.gets (filterWorkspaces . getLeftScreen) <*> getSortByXineramaRule
  , ppHidden		= pangoColor "gray"  
  , ppHiddenNoWindows   = const ""   
--  , ppExtras	= [logCmd "qdbus org.mpris.clementine /Player org.freedesktop.MediaPlayer.GetMetadata | grep -E 'performer|title' | awk -F':' '{print $2}' | sed 'N;s/\n/ -/; s/^[[:space:]]*//'"]
  , ppExtras	        = [pangoColor "orange" `onLogger` logCmd "~/bin/now_playing"]
					--logCmd "qdbus org.mpris.clementine /Player org.freedesktop.MediaPlayer.GetMetadata | grep -E 'performer|title' | awk -F':' '{print $2}' | sed 'N;s/\n/ -/; s/^[[:space:]]*//'"]
  --, ppExtras	= onLogger $ pangoColor "orange" . logCmd "qdbus org.mpris.clementine /Player org.freedesktop.MediaPlayer.GetMetadata | grep -E 'performer|title' | awk -F':' '{print $2}' | sed 'N;s/\n/ -/; s/^[[:space:]]*//'"
--			  logCmd "mpc | awk -F: 'NR > 1 { exit }; { print $NF }'"]
--  , ppExtras	= fixedWidthL AlignRight " " 99 <$> pangoColor "orange" <$> logCmd "qdbus org.mpris.clementine /Player org.freedesktop.MediaPlayer.GetMetadata | grep -E 'performer|title' | awk -F':' '{print $2}' | sed 'N;s/\n/ -/; s/^[[:space:]]*//'"
--		    logCmd "mpc | awk -F: 'NR > 1 { exit }; { print $NF }'" 
	--, ppExtras	= [Just `fmap` runProcessWithInput "/bin/bash" [] 
	--		    ("mpc | awk -F: 'NR > 1 { exit }; { print $NF }'")]
				--("qdbus org.mpris.clementine /Player org.freedesktop.MediaPlayer.GetMetadata | grep -E 'performer|title' | awk -F':' '{print $2}' | sed 'N;s/\\n/ -/; s/^\s*//'")]
				--("qdbus org.mpris.clementine /Player org.freedesktop.MediaPlayer.GetMetadata | grep -E 'performer|title' | awk -F':' '{print $2}' | sed 'N;s/\n/ -/; s/^[[:space:]]*//'")]

}


winKey :: KeyMask
winKey = mod4Mask

lAlt :: KeyMask
lAlt = mod1Mask

{-Multiline comment for reference
winKey :: KeyMask
winKey :: KeyMask-}

--myTerminal :: String
--myTerminal = "konsole"
myTerminal = "urxvtc -cd ~"
altTerminal = "urxvtc -cd ~ -name altUrxvt"

--myTerminal = "gnome-terminal"
--myTerminal = "terminator"

{-slotWksp = ["1:main", "2:jobs", "3:is660","4:office","5:sysadmin","6:irc","7:digium", "8:vm","9:dump","0:music","F1:man","F2:jobs","F3:isovr","F4:offovr","F5:sysovr","F6:ircovr","F7:dgmovr","F8:vmovr","F9:dmpovr","F10:musovr"]

leftWksp = ["01:main","02:jobs","03:is660","04:office","05:sysadmin","06:irc","07:digium","08:vm","09:dump","10:music"]

rightWksp = [
	"F1:man"
      , "F2:jobs"
      , "F3:isovr"
      , "F4:offovr"
      , "F5:sysovr"
      , "F6:ircovr"
      , "F7:dgmovr"
      , "F8:vmovr"
      , "F9:dmpovr"
      , "F10:musovr"
]-}


{- Many of the key combinations below match or are analogs of key combinations
 - in Windows.  This is to keep the amount of mental context switching to a
 - minimum.
 - 
 -}
myKeys =	[ 
    --((winKey ,											xK_l),			  								spawnHere "xscreensaver-command --lock && sleep 3 && xset dpms force off")
    ((winKey ,											xK_l),			  								spawnHere "dm-tool lock && sleep 3 && xset dpms force off")
--    ((winKey ,											xK_l),			  								spawnHere "xscreensaver-command --lock")
--    ((winKey ,											xK_l),			  								spawnHere "qdbus org.kde.krunner /ScreenSaver Lock")
  , ((winKey ,											xK_Return),		  							do
                                                                      windows (viewOnScreen 0 "sh")
                                                                      ifWindows (resource =? "urxvt") (mapM_ focus) (spawnHere myTerminal))
  , ((controlMask .|. lAlt,         xK_BackSpace),                 (spawnHere "xfdesktop --quit"))
  , ((controlMask .|. shiftMask,		xK_Return),		  							do
                                                                      windows (viewOnScreen 1 "ws")
                                                                      ifWindows (resource =? "altUrxvt") (mapM_ focus) (spawnHere altTerminal))
  , ((winKey ,                      xK_v),                        do
                                                                    windows (viewOnScreen 1 "ws")
                                                                    ifWindows (className =? "Gvim") (mapM_ focus) (spawnHere "gvim"))
  , ((lAlt ,                        xK_v),                        spawnHere "xfce4-popup-clipman")
  --, ((winKey ,											xK_x),			  								windowPromptGoto dXPConfig)
  , ((winKey .|. shiftMask,					xK_x),			  								windowPromptBring dXPConfig)
  --, ((winKey .|. shiftMask,					xK_Return),		  							windows W.swapMaster)
  --, ((winKey .|. shiftMask,					xK_Return),		  							spawnHere myTerminal)
  , ((winKey ,											xK_b),			  								sendMessage ToggleStruts)
  --, ((winKey ,											xK_g),			  								spawnHere "chromium --allow-outdated-plugins --purge-memory-button ")
  --, ((winKey ,											xK_g),			  								ifWindows (className =? "Google-chrome") (mapM_ focus) (spawnHere "google-chrome"))
	, ((winKey ,											xK_g),												do
                                                                    windows (viewOnScreen 1 "sb")
                                                                    ifWindows (className =? "chromium") (mapM_ focus) (spawnHere "chromium"))
                                                                    --ifWindows (className =? "Chromium-browser") (mapM_ focus) (spawnHere "chromium-browser"))
  --, ((winKey ,											xK_r),												do
   --                                                                 windows (viewOnScreen 1 "wb")
     --                                                               ifWindows (className =? "Google-chrome-stable") (mapM_ focus) (spawnHere "google-chrome-stable"))
                                                                    --ifWindows (className =? "Google-chrome") (mapM_ focus) (spawnHere "chromium --user-data-directory=~/work/avoxi/chromium"))
--, ((winKey ,											xK_g),			  								spawnHere "google-chrome --purge-memory-button ")
  --, ((winKey ,											xK_i),				  							spawnHere "iceweasel")
  --, ((winKey ,											xK_i),				  							spawnHere "clementine")
	, ((winKey ,											xK_i),						  					ifWindows (className =? "Clementine") (mapM_ killWindow) (spawnHere "clementine"))
  --, ((winKey ,											xK_d),				  							spawnHere "xfce4-dict")
  , ((winKey ,												xK_d),	  									ifWindows (className =? "Xfce4-dict") (mapM_ killWindow) (spawnHere "xfce4-dict"))
  , ((winKey ,											xK_f),			  								spawnHere (myTerminal ++ " -e vifm . ~"))
  --, ((winKey ,											xK_f),			  								spawnHere ("export SHELL=/bin/bash && " ++ myTerminal ++ " -e mc"))
  --, ((winKey ,											xK_e),			  								spawnHere ("export SHELL=/bin/bash && " ++ myTerminal ++ " -e mc")) -- key stroke matches Win+E (from Win7)
  , ((winKey ,											xK_o),			  								do
                                                                    --windows (viewOnScreen 0 "of")
                                                                    ifWindows (fmap("libreoffice" `isPrefixOf`) className) (mapM_ focus) (spawnHere "libreoffice"))
 --   , ((winKey ,                      xK_o),                        do
  --                                                                    windows (viewOnScreen 1 "rbank")
   --                                                                   ifWindows (className =? "Opera") (mapM_ focus) (spawnHere "opera"))
--  , ((winKey .|. controlMask ,			xK_a),				  							spawnHere "/home/trey/launchers/airdroid.desktop")
--	, ((winKey ,											xK_s),												do
 --                                                                   windows (viewOnScreen 1 "hng")
   --                                                                 ifWindows (className =? "Skype") (mapM_ focus) (spawnHere "skype"))
  , ((winKey ,                      xK_s),                        ifWindows (className =? "Pavucontrol") (mapM_ killWindow) (spawnHere "pavucontrol"))
  , ((winKey ,											xK_c),				  							kill)
  --, ((winKey ,											xK_m),			  								windows W.focusMaster)
  , ((winKey ,											xK_comma),		  							sendMessage (IncMasterN 1))
  , ((winKey ,											xK_period),		  							sendMessage (IncMasterN (-1)))
  , ((winKey ,											xK_j),				  							windows W.focusDown) -- explicitly setting the default
  , ((winKey .|. controlMask,					xK_j),				  							windows W.swapDown) -- explicitly setting the default
  , ((winKey ,											xK_k),				  							windows W.focusUp) -- explicitly setting the default
  , ((winKey .|. controlMask,					xK_k),				  							windows W.swapUp) -- explicitly setting the default
  , ((lAlt ,												xK_Tab),			  							windows W.focusDown) -- replicating MS Windows task switcher behavior
  , ((lAlt .|. shiftMask,						xK_Tab),			  							windows W.focusUp)  -- replicating MS Windows task switcher behavior
  , ((winKey .|. controlMask,				xK_Return),		  							windows W.swapMaster)
 --, ((winKey .|. shiftMask,				xK_space),		  							setLayout $ XMonad.layoutHook conf) -- the default, commented here for documentation and posterity
  , ((winKey ,											xK_p),				  							spawnHere launcher)
	, ((winKey .|. shiftMask,					xK_p),												spawnHere "gmrun")
  --, ((winKey .|. controlMask ,			xK_d),				  							spawn ("sleep 1 && date +%F | tr -d '\n' | ~/bin/genxmacro | xmacroplay -d 0.1 $DISPLAY"))
	--, ((controlMask .|. shiftMask ,		xK_d),												spawn ("sleep 1 && date '+%F %T %p:  ' | tr -d '\n' | ~/bin/genxmacro | xmacroplay -d 0.1 $DISPLAY"))
  --, ((winKey .|. controlMask ,			xK_k),				  							spawn ("sleep 1 && cat ~/.macros/code.macro | xmacroplay -d 0.1 $DISPLAY"))
  , ((shiftMask,										xK_Insert),		  							withFocused shiftInsert)
	--, ((controlMask,									xK_n),												raiseMaybe (spawnHere myTerminal) (className =? "URxvt"))
  , ((winKey ,											xK_Print),		  							spawnHere "xfce4-screenshooter")
  , ((winKey ,											xK_Left),			  							prevWS)
  , ((winKey ,											xK_Right),		  							nextWS)
	, ((winKey ,											xK_Up),												spawnHere "skippy-xd")
  --, ((0,														xF86XK_Calculator),	  				spawnHere "/home/trey/bin/calc")
  --, ((0,														xF86XK_Calculator),	  				ifWindows (className =? "Gcalctool") (mapM_ killWindow) (spawnHere "gcalctool"))
  , ((0,														xF86XK_Calculator),	  				ifWindows (className =? "Gnome-calculator") (mapM_ killWindow) (spawnHere "gnome-calculator"))
  , ((0,														xF86XK_AudioPlay),	  				spawn "clementine --play-pause")
  --, ((0,														xF86XK_AudioMute),	  				spawn "amixer -c 0 set Master toggle")
  , ((0,														xF86XK_AudioMute),	  				spawn "/home/trey/bin/mute")
  , ((0,														xF86XK_AudioRaiseVolume), 		spawn "amixer -c 0 set Master 5dB+")
  , ((0,														xF86XK_AudioLowerVolume), 		spawn "amixer -c 0 set Master 5dB-")
-- , ((winKey .|. controlMask,				xK_Left),		  								shiftToPrev >> prevWS)
-- , ((winKey .|. controlMask,				xK_Right),		  							shiftToNext >> nextWS)
  , ((winKey .|. controlMask,					xK_h),		  									sendMessage Shrink)
  , ((winKey .|. lAlt,      					xK_h),		  									do
                                                                      --windows (viewOnScreen 1 "hng")
                                                                      windows (viewOnScreen 1 "sb")
                                                                      ifWindows (className =? "chromium") (mapM_ focus) (spawnHere "chromium"))
                                                                      --ifWindows (className =? "Iceweasel") (mapM_ focus) (spawnHere "iceweasel"))
  , ((winKey .|. controlMask,					xK_l),											  sendMessage Expand)
  , ((winKey ,												xK_1),			  								windows (viewOnScreen 0 "sh"))
  , ((winKey ,												xK_2),			  								windows (viewOnScreen 0 "sb"))
  , ((winKey ,                        xK_a),                        do 
                                                                        windows (viewOnScreen 1 "hip") 
                                                                        --ifWindows (className =? "Pidgin") (mapM_ focus) (spawnHere "pidgin"))
                                                                        ifWindows (className =? "Hipchat") (mapM_ focus) (spawnHere "hipchat"))
  {-
  , ((winKey ,											xK_2),			  								windows (viewOnScreen 0 "02:jobs"))
  , ((winKey ,											xK_3),											  windows (viewOnScreen 0 "03:is660"))
  , ((winKey ,											xK_4),											  windows (viewOnScreen 0 "04:office"))
  , ((winKey ,											xK_5),											  windows (viewOnScreen 0 "05:sysadmin"))
  , ((winKey ,											xK_6),											  windows (viewOnScreen 0 "06:irc"))
  , ((winKey ,											xK_7),											  windows (viewOnScreen 0 "07:digium"))
  , ((winKey ,											xK_8),			  								windows (viewOnScreen 0 "08:vm"))
  , ((winKey ,											xK_9),											  windows (viewOnScreen 0 "09:dump"))
  , ((winKey ,											xK_0),											  windows (viewOnScreen 0 "10:music"))
  , ((winKey ,											xK_F1),											  windows (viewOnScreen 1 "F1:man"))
  , ((winKey ,											xK_F2),											  windows (viewOnScreen 1 "F2:jobs"))
  , ((winKey ,											xK_F3),											  windows (viewOnScreen 1 "F3:isovr"))
  , ((winKey ,											xK_F4),											  windows (viewOnScreen 1 "F4:offovr"))
  , ((winKey ,											xK_F5),											  windows (viewOnScreen 1 "F5:sysovr"))
  , ((winKey ,											xK_F6),											  windows (viewOnScreen 1 "F6:ircovr"))
  , ((winKey ,											xK_F7),											  windows (viewOnScreen 1 "F7:dgmovr"))
  , ((winKey ,											xK_F8),											  windows (viewOnScreen 1 "F8:vmovr"))
  , ((winKey ,											xK_F9),											  windows (viewOnScreen 1 "F9:dmpovr"))
  , ((winKey ,											xK_F10),										  windows (viewOnScreen 1 "F10:musovr"))-}
  , ((winKey .|. shiftMask ,				xK_q),			  								spawn "xfce4-session-logout")
	-- win+h shows the selected workspace
  , ((winKey ,											xK_h),											  DW.withWorkspace myXPConfigSelect $ \wk -> do
      sc <- screenBy 0
      if sc == 0
	    --then XS.modify $ LeftScreen . (wk :) . getLeftScreen  -- prefix to list
			then XS.modify $ LeftScreen . (++ [wk]) . getLeftScreen -- append to list
      --else XS.modify $ RightScreen . (wk :) . getRightScreen -- prefix to list
      else XS.modify $ RightScreen . (++ [wk]) . getRightScreen -- append to list
      windows $ W.view wk)
	-- win+z moves the current window to the selected workspace
  , ((winKey ,											xK_z),											  DW.withWorkspace myXPConfigSelect (\ws -> do
	  sc <- screenBy 0
	  if sc == 0
	    then XS.modify $ LeftScreen . nub . (ws :) . getLeftScreen -- prefix to list
	    else XS.modify $ RightScreen . nub . (ws :) . getRightScreen -- prefix to list
      --then XS.modify $ LeftScreen . nub . (++ [ws]) . getLeftScreen  -- append to list
	    --else XS.modify $ RightScreen . nub . (++ [ws]) . getRightScreen -- append to list

	  windows $ W.shift ws
	  -- refresh
	))
	-- win+BackSpace removes the current workspace
  , ((winKey ,											xK_BackSpace),		  					do
    curr <- gets (W.currentTag . windowset)
    sc <- screenBy 0
    if sc == 0
      then do
				ws <- XS.gets getLeftScreen
				XS.put (LeftScreen (filter (/= curr) ws))
      else do
			ws <- XS.gets getRightScreen
			XS.put (RightScreen (filter (/= curr) ws))                                              
    DW.removeWorkspace
    )
	-- win+ctrl+r renames the current workspace
  , ((winKey .|. controlMask ,			xK_r),			  								do
	old <- gets (W.currentTag . windowset)
	DW.renameWorkspace myXPConfigNew
	created <- gets (W.currentTag . windowset)
	sc <- screenBy 0
	if sc == 0
	   then do
	      ws <- XS.gets getLeftScreen
	      XS.put (LeftScreen (filter (/= old) ws))
	      --XS.modify $ LeftScreen . (created :) . getLeftScreen -- prefix to list
	      XS.modify $ LeftScreen . (++ [created]) . getLeftScreen -- append to list
	   else do
	      ws <- XS.gets getRightScreen
	      XS.put (RightScreen (filter (/= old) ws))
	      --XS.modify $ RightScreen . (created :) . getRightScreen -- prefix to list
	      XS.modify $ RightScreen . (++ [created]) . getRightScreen -- append to list
	refresh)
	-- win+m creates a new workspace
  , ((winKey ,											xK_m) ,			  								DW.withWorkspace myXPConfigNew $ \wk -> do
      sc <- screenBy 0
      if sc == 0
			--then XS.modify $ LeftScreen . (wk :) . getLeftScreen  -- prefix to list
			then XS.modify $ LeftScreen . (++ [wk]) . getLeftScreen -- append to list
      --else XS.modify $ RightScreen . (wk :) . getRightScreen -- prefix to list
      else XS.modify $ RightScreen . (++ [wk]) . getRightScreen -- append to list
      windows $ W.view wk)
	--, ((winKey .|. shiftMask,					xK_d)   -- macro conflict dialog problem needs to be resolved
  ]
  ++
  -- Set up window -> workspace keys
  [((m .|. winKey, key), screenWorkspace sc >>= flip whenJust (windows . f))
--	| (key, sc) <- zip [xK_w, xK_r] [0..]
	-- | (key, sc) <- zip [xK_Left, xK_Right] [0..] -- For arrow keys
	| (key, sc) <- zip [xK_w, xK_r] [0..] -- For w,e keys
	, (f, m) <- [(W.view, 0), (W.shift, controlMask)]]

dXPConfig = defaultXPConfig {
   bgColor = "yellow"
, fgColor = "blue"
}

myXPConfigSelect = defaultXPConfig {
    bgColor		= "yellow"
  , fgColor		= "blue"
  , autoComplete	= Just 0
  , showCompletionOnTab = True
}

myXPConfigNew = defaultXPConfig {
    bgColor		= "yellow"
  , fgColor		= "blue"
  , autoComplete	= Nothing
  , showCompletionOnTab	= True
}

