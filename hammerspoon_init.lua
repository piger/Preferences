-- hammerspoon init.lua
-- NOTE: When you reload your configuration you keep all the old "watchers" running!

-- set grid size
hs.grid.GRIDWIDTH = 40
hs.grid.GRIDHEIGHT = 40
hs.grid.MARGINX = 0
hs.grid.MARGINY = 0

-- Audio Configuration
local dacName = "FiiO USB DAC-E10"
local speakersName = "Built-in Output"

-- no animation pls
hs.window.animationDuration = 0

local mash = {"cmd", "alt", "ctrl"}
-- local mashift = {"cmd", "alt", "shift"}

hs.hotkey.bind(mash, "M", function() hs.window.focusedWindow():maximize() end)
hs.hotkey.bind(mash, "space", hs.spotify.displayCurrentTrack)
hs.hotkey.bind(mash, "h", hs.grid.resizeWindowThinner)
hs.hotkey.bind(mash, "l", hs.grid.resizeWindowWider)
hs.hotkey.bind(mash, "j", hs.grid.resizeWindowTaller)
hs.hotkey.bind(mash, "k", hs.grid.resizeWindowShorter)
hs.hotkey.bind(mash, "up", hs.grid.pushWindowUp)
hs.hotkey.bind(mash, "down", hs.grid.pushWindowDown)
hs.hotkey.bind(mash, "left", hs.grid.pushWindowLeft)
hs.hotkey.bind(mash, "right", hs.grid.pushWindowRight)
hs.hotkey.bind(mash, ";", function() hs.grid.snap(hs.window.focusedWindow()) end)
hs.hotkey.bind(mash, "'", function() hs.fnutils.map(hs.window.visibleWindows(), hs.grid.snap) end)


-- functions and callbacks --
-----------------------------

-- Open a URL in Chrome
function openInChrome(url)
   local chrome = hs.application.find("Google Chrome")
   if chrome == nil then
      return
   end

   hs.urlevent.openURLWithBundle(url, chrome:bundleID())
end


-- WiFi Watcher --
------------------
local wifi_work_ssid = "zendesk"
local wifi_last_ssid = hs.wifi.currentNetwork()

local wifi_watcher = hs.wifi.watcher.new(function ()
   local wifi_new_ssid = hs.wifi.currentNetwork()

   if wifi_new_ssid == wifi_work_ssid and wifi_last_ssid ~= wifi_work_ssid then
      hs.audiodevice.defaultOutputDevice():setVolume(1)
      hs.notify.new({title="Hammerspoon", informativeText="Lowering volume as we are at Work!"}):send()
   end

   wifi_last_ssid = wifi_new_ssid
end)
wifi_watcher:start()


-- caffeine mode --
-------------------
caffeineMenu = hs.menubar.new()
caffeineMenu:setTooltip("Caffeinate mode (i.e. disable screen locking)")
caffeinateSleepType = "displayIdle"

function setCaffeineDisplay(state)
   if state then
      caffeineMenu:setTitle("🐴")
   else
      caffeineMenu:setTitle("🎋")
   end
end

function caffeineClicked()
   setCaffeineDisplay(hs.caffeinate.toggle(caffeinateSleepType))
   if hs.caffeinate.get(caffeinateSleepType) then
      hs.alert.show("Niiihhhh!")
   else
      hs.alert.show("Zzz...")
   end
end

caffeineMenu:setClickCallback(caffeineClicked)
setCaffeineDisplay(hs.caffeinate.get(caffeinateSleepType))

-- disable caffeine mode after sleep
local sleepWatcher = hs.caffeinate.watcher.new(function (eventType)
   if (eventType == hs.caffeinate.watcher.systemDidWake) then
      setCaffeineDisplay(hs.caffeinate.set(caffeinateSleepType, false, true))
   end
end)
sleepWatcher:start()


-- Monitor watcher, used to toggle the dynamic profile in iTerm2 --
-------------------------------------------------------------------
function hasExternalMonitor()
   for _, screen in pairs(hs.screen.allScreens()) do
      if screen:name() == "Thunderbolt Display" then
         return true
      end
   end
   return false
end

-- Switch dynamic profile in iTerm2
-- Valid values: iTerm2_Dynamic_12.json, iTerm2_Dynamic_11.json
function setIterm2Profile(filename)
   hs.execute("ln -sf $HOME/Preferences/iTerm2/" .. filename .. " \"$HOME/Library/Application Support/iTerm2/DynamicProfiles/iTerm2_Dynamic.json\"")
end

-- Switch font in emacs
function tellEmacsToChangeFont()
   hs.execute("/usr/local/bin/emacsclient -e '(set-the-right-font)'")
end

function displayWatcherCallback()
   tellEmacsToChangeFont()

   if hasExternalMonitor() then
      setIterm2Profile("iTerm2_Dynamic_12.json")
   else
      setIterm2Profile("iTerm2_Dynamic_11.json")
   end
end

local monitorWatcher = hs.screen.watcher.new(displayWatcherCallback)
monitorWatcher:start()


-- Audio Device toggle --
-------------------------
audioToggleMenu = hs.menubar.new()
audioToggleMenu:setTooltip("Toggle the default audio output device")

-- Set the menubar icon corresponding to the default output device
function setToggleAudioIcon()
   local device = hs.audiodevice.defaultOutputDevice()
   if device:name() == speakersName then
      audioToggleMenu:setTitle("🔈")
   else
      audioToggleMenu:setTitle("🎧")
   end
end

-- Toggle the default sound output bewteen built-in and DAC
function toggleAudioDevice(modifier)
   local currOutput = hs.audiodevice.defaultOutputDevice()
   local builtinOut = hs.audiodevice.findOutputByName(speakersName)
   local fioOutput = hs.audiodevice.findOutputByName(dacName)

   if not builtinOut or not fioOutput then
      hs.alert.show("Cannot find output devices, or FIO is not plugged in")
      return
   end

   if currOutput:name() == fioOutput:name() then
      builtinOut:setDefaultOutputDevice()
   else
      fioOutput:setDefaultOutputDevice()
   end

   hs.notify.new({title="Hammerspoon", informativeText="New default audio device: " .. hs.audiodevice.defaultOutputDevice():name()}):send()

   setToggleAudioIcon()
end
audioToggleMenu:setClickCallback(toggleAudioDevice)

-- Set up a watcher to update the menubar icon when the FIO gets unplugged
hs.audiodevice.watcher.setCallback(function(event)
      setToggleAudioIcon(audioToggleMenu)
end)
hs.audiodevice.watcher.start()

-- Set the initial menubar icon
setToggleAudioIcon()


-- Music players controls --
----------------------------
function getCurrentMusicPlayer()
   if hs.itunes.isRunning() and hs.itunes.isPlaying() then
      return hs.itunes
   elseif hs.spotify.isRunning() and hs.spotify.isPlaying() then
      return hs.spotify
   end
   return
end

-- Get current Artist and Track name from the current music player.
-- Returns: "Artist name", "Track name"
function getCurrentTrackInfo()
   local player = getCurrentMusicPlayer()
   if player == nil then
      return nil, nil
   end

   return player.getCurrentArtist(), player.getCurrentTrack()
end

function lookupOnYoutube()
   local currArtist, currTrack = getCurrentTrackInfo()
   if currArtist == nil or currTrack == nil then
      return
   end
   openInChrome("https://www.youtube.com/results?search_query=" .. hs.http.encodeForQuery(currArtist .. " " .. currTrack))
end

function lookupOnGenius()
   local currArtist, currTrack = getCurrentTrackInfo()
   if currArtist == nil or currTrack == nil then
      return
   end
   openInChrome("http://genius.com/search?q=" .. hs.http.encodeForQuery(currArtist .. " " .. currTrack))
end

musicMenu = hs.menubar.new()
musicMenu:setTitle("🎷")
musicMenu:setTooltip("Music Player (Spotify or iTunes) controls")

musicMenuLayout = {
   { title = "📺 Search on YouTube", fn = lookupOnYoutube },
   { title = "🔮 Search on Genius", fn = lookupOnGenius },
}
musicMenu:setMenu(musicMenuLayout)


-- Cuppa --
-----------
cuppaMenu = hs.menubar.new()
cuppaMenu:setTitle("🍵")
cuppaMenu:setTooltip("Cuppa tea timer. ⌘+click to cancel the timer")

cuppaTimer = nil
cuppaTimerTimer = nil

-- stolen from Cuppa.app
pourSound = hs.sound.getByFile(os.getenv("HOME") .. "/Preferences/hammerspoon/pour.aiff")
spoonSound = hs.sound.getByFile(os.getenv("HOME") .. "/Preferences/hammerspoon/spoon.aiff")

-- for notification center
cuppaImage = hs.image.imageFromPath(os.getenv("HOME") .. "/Preferences/hammerspoon/cuppa.jpg")


-- set the title of the menubar
function cuppaSetTitle(timeLeft)
   if timeLeft ~= nil then
      cuppaMenu:setTitle("🍵 " .. timeLeft)
   else
      cuppaMenu:setTitle("🍵")
   end
end

-- callback called when the cuppa timer reaches the end
function cuppaTimerEnd()
   hs.alert.show("🍵 is ready!")
   hs.notify.new({title="Cuppa", informativeText="Your tea cup 🍵 is ready!"}):setIdImage(cuppaImage):send()
   cuppaSetTitle()
   spoonSound:play()
end

-- callback called to update the countdown timer on the menubar
function cuppaTimerUpdate()
   if cuppaTimer ~= nil then
      local secondsLeft = cuppaTimer:nextTrigger()
      if secondsLeft < 0 then
         secondsLeft = 0
      end
      local timeLeft = os.date("!%X", math.floor(secondsLeft))
      if secondsLeft < (60 * 60) then
         timeLeft = string.sub(timeLeft, 4)
      end
      cuppaSetTitle(timeLeft)
   end
end

-- callback called by the supervisor timer to check on the cuppa timer
function cuppaTimerPing()
   if cuppaTimer == nil or not cuppaTimer:running() then
      return true
   end
   return false
end

-- cancel the current cuppa timer
function cuppaTimerCancel()
   if cuppaTimer ~= nil then
      cuppaTimer:stop()
   end
   if cuppaTimerTimer ~= nil then
      cuppaTimerTimer:stop()
   end
   hs.alert.show("Cuppa timer canceled")
   cuppaSetTitle()
end

-- prompt the user for a cuppa timer
function cuppaAskForTimer(mod)
   script = [[
   display dialog "How many minutes?" default answer "3"
   set answer to text returned of result
   return answer
   ]]
   
   if mod["cmd"] then cuppaTimerCancel(); return; end

   local success, out, rawout = hs.osascript.applescript(script)
   if success then
      local minutes = tonumber(out)
      if minutes ~= nil and minutes > 0 then
         cuppaTimer = hs.timer.doAfter(minutes * 60, cuppaTimerEnd)
         cuppaTimerTimer = hs.timer.doUntil(cuppaTimerPing, cuppaTimerUpdate, 1)
         cuppaTimerUpdate()
         pourSound:play()
      end
   end
end

cuppaMenu:setClickCallback(cuppaAskForTimer)


-- reload hammerspoon (and stop all the running watchers) --
------------------------------------------------------------
function reloadHammerspoon()
   wifi_watcher:stop()
   sleepWatcher:stop()
   monitorWatcher:stop()
   hs.audiodevice.watcher.stop()
   hs.reload()
end
hs.hotkey.bind(mash, "R", reloadHammerspoon)


--- ole'
hs.alert.show("Hammerspoon 💩 loaded!")
